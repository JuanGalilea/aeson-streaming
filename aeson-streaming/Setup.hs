import Distribution.Simple

import Distribution.Extra.Doctest

main = defaultMainWithHooks $ addDoctestsUserHook "doctests" $ simpleUserHooks
