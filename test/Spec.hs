module Main where

import Test.Hspec
import Chili.Diff
import Chili.HSX
import Chili.Patch
import Chili.Types
import Language.Haskell.HSX.QQ (hsx)

main :: IO ()
main = hspec spec
{-
diffAndPatch :: Html -> Html -> Bool
diffAndPatch h1 h2 =
  let patch = diff h1 h2
      h2' = 
-}
spec :: Spec
spec =
  pure ()
