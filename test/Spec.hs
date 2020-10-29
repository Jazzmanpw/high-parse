import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "text" $ do
    let parseTest = text "test"
    let parseLaTeX = text "LaTeX"
    describe "value" $ do
      it "returns text if it's found" $
        rValue (parseTest "test string") `shouldBe` Just "test"
      it "return Nothing as a value if match failed" $
        rValue (parseTest "not") `shouldBe` Nothing
      it "matches with case sensitive matcher" $ do
        rValue (parseLaTeX "LaTeX is nice") `shouldBe` Just "LaTeX"
        rValue (parseLaTeX "latex is wrong casing") `shouldBe` Nothing
    describe "tail" $ do
      it "returns the remaining source" $
        rTail (parseTest "test string") `shouldBe` Just " string"
      it "returns the Nothing as a tail if the match have failed" $
        rTail (parseTest "not the text") `shouldBe` Nothing
  describe "regex" $ do
    let parseReg = regex "ba[rz]" []
    let parseCaseSensitiveReg = regex "10[GMT]B"
    let parseVariousLengthReg = regex "\\s+" []
    describe "value" $ do
      it "returns matched value if the regex matched" $ do
        rValue (parseReg "barbie") `shouldBe` Just "bar"
        rValue (parseReg "bazaar") `shouldBe` Just "baz"
      it "doesn't fail if match is an empty string" $
        rValue (regex "o?" [] "done") `shouldBe` Just ""
      it "return Nothing on failed match" $
        rValue (parseReg "football") `shouldBe` Nothing
      it "uses case sensitive match" $
        rValue (parseCaseSensitiveReg [] "10gb") `shouldBe` Nothing
      it "matches only at the beginning of the source" $ do
        rValue (parseReg "ambar") `shouldBe` Nothing
      it "supports IgnoreCase flag" $
        rValue (parseCaseSensitiveReg [IgnoreCase] "10gb") `shouldBe` Just "10gb"
      it "supports DotAll flag" $
        rValue (regex "abra.cadabra" [DotAll] "abra\ncadabra") `shouldBe` Just "abra\ncadabra"
    describe "tail" $ do
      it "returns the source without matched string" $ do
        rTail (parseVariousLengthReg " one space") `shouldBe` Just "one space"
        rTail (parseVariousLengthReg "    four spaces") `shouldBe` Just "four spaces"
        rTail (parseReg "barbarian") `shouldBe` Just "barian"
      it "returns Nothing if match failed" $
        rTail (parseReg "anything") `shouldBe` Nothing
