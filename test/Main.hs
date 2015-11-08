-- Automatically discover *Spec.hs files
-- with a GCC pre-processor directive.
-- See http://hspec.github.io/hspec-discover.html for more information.
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

-- The explicit form looks like this...
{-
import Test.Hspec

import qualified SExpTest
-- ...

tests = [ describe "SExp" SExpTest.spec
        -- ...
        ]

main :: IO ()
main = hspec $ sequence_ tests
-}
