{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Streaming.Paths (
  PathComponent(..)
, jpath
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Pat(..), Type(..), Lit(..))
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

-- | A generic path component
data PathComponent = Offset !Int | Field !Text
  deriving (Eq, Ord)

instance Show PathComponent where
  showsPrec d v = showParen (d > 10) $ doShow v
    where doShow (Offset i) = showString "Offset " . showsPrec 11 i
          doShow (Field f) = showString "Field " . showsPrec 11 f
  showList = renderPath

-- this renders a jq-style query path.
renderPath :: [PathComponent] -> ShowS
renderPath path suffix = preDot $ foldr render suffix path
  where
    render (Offset i) = ('[':) . shows i . (']':)
    render (Field f) =
      let fs = T.unpack f
      in if isIdentifier fs
         then ('.':) . showString fs
         else ('[':) . showString (TL.unpack $ A.encodeToLazyText f) . (']':)
    preDot cs@('.':_) = cs
    preDot cs = '.' : cs

isIdentifier :: String -> Bool
isIdentifier "" = False
isIdentifier (c : cs) = isInitial c && all isContinuation cs
isInitial, isContinuation :: Char -> Bool
isInitial c = isAsciiLower c || isAsciiUpper c || c == '_'
isContinuation c = isInitial c || isDigit c

instance Read PathComponent where
  readsPrec d r = readParen (d > 10) (\r' -> [(Offset m,t) |
                                              ("Offset",s) <- lex r',
                                               (m,t) <- readsPrec 11  s]) r
                  ++ readParen (d > 10) (\r' -> [(Field m,t) |
                                                 ("Field",s) <- lex r',
                                                 (m,t) <- readsPrec 11  s]) r
  readList = readP_to_S parsePath

parsePath :: ReadP [PathComponent]
parsePath = nonEmpty +++ empty
  where
    nonEmpty = do
      first <- initialComponent
      rest <- many component
      pure (first : rest)
    empty = do
      _ <- char '.'
      pure []
    initialComponent = parseField +++ (char '.' *> parseIndex)
    component = parseField +++ parseIndex
    parseField = do
      _ <- char '.'
      c <- satisfy isInitial
      cs <- munch isContinuation
      pure . Field $ T.pack (c:cs)
    parseIndex = char '[' *> (parseNumericIndex +++ parseStringIndex) <* char ']'
    parseNumericIndex = do
      cs <- munch1 isDigit
      pure . Offset $ read cs
    parseStringIndex = char '"' *> (deJSON . ('"':) . (++"\"") . concat =<< many parseStringChar) <* char '"'
    parseStringChar =
      get >>= \case
        '"' -> pfail
        '\\' -> ('\\':) . (:[]) <$> get
        other -> pure [other]
    deJSON t =
      case A.decode . BSL.fromStrict . T.encodeUtf8 $ T.pack t of
        Just s -> pure $ Field s
        Nothing -> pfail

-- | A quasiquoter for json path literals.
jpath :: QuasiQuoter
jpath = QuasiQuoter { quoteExp =
                        maybe (fail "invalid path") (pure . convertExp) . readMaybe
                    , quotePat =
                        maybe (fail "invalid path") (pure . convertPat) . readMaybe
                    , quoteType = const $ fail "invalid type"
                    , quoteDec = const $ fail "invalid declaration"
                    }

convertExp :: [PathComponent] -> Exp
convertExp pcs = SigE (ListE $ map convertComponent pcs) (AppT ListT $ ConT ''PathComponent)
  where
    convertComponent (Offset i) = AppE (ConE 'Offset) (LitE . IntegerL $ fromIntegral i)
    convertComponent (Field f) = AppE (ConE 'Field) (LitE . StringL $ T.unpack f)

convertPat :: [PathComponent] -> Pat
convertPat pcs = ListP $ map convertComponent pcs
  where
    convertComponent (Offset i) = ConP 'Offset [LitP . IntegerL $ fromIntegral i]
    convertComponent (Field f) = ConP 'Field [LitP . StringL $ T.unpack f]

