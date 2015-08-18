import Test.Hspec (Spec, hspec, describe, it, shouldBe)

mainSpec :: Spec
mainSpec = do
  describe "piyo" $ do
    it "nyan" $
      3 + 5 `shouldBe` 6

doMainSpec :: IO ()
doMainSpec = hspec mainSpec


main :: IO ()
main = doMainSpec
