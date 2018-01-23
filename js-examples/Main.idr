module Main

import Specdris.SpecIO

%default total

querySelector : (selector : String) -> JS_IO Ptr
querySelector = foreign FFI_JS "document.querySelector(%0)" _

isNull : Ptr -> JS_IO Bool
isNull ptr = do
  "true" <- foreign FFI_JS "(%0 === null).toString()" (Ptr -> JS_IO String) ptr
    | _ => pure False
  pure True

getInnerText : Ptr -> JS_IO String
getInnerText = foreign FFI_JS "%0.innerText" _

removeTestElements : JS_IO SpecResult -> JS_IO SpecResult
removeTestElements resultIO = do
  result <- resultIO
  foreign FFI_JS
    """
      document.body.querySelectorAll(':not(script)').forEach(function(child) {
        document.body.removeChild(child);
      })
    """ (JS_IO ())
  pure result

addParagraph : String -> JS_IO ()
addParagraph = foreign FFI_JS
  "document.body.appendChild(document.createElement('p')).innerText = %0" _

addParagraphSpec : SpecTree' FFI_JS
addParagraphSpec =
  describe "addParagraph" $ do
    it "appends a <p> element" $ do
      addParagraph ""
      
      null <- querySelector "p" >>= isNull
      pure $ shouldBeFalse null
    it "creates the correct text" $ do
      let expected = "hello specdris"
      addParagraph expected
      
      actual <- querySelector "p" >>= getInnerText
      pure $ actual === expected

main : JS_IO ()
main = specIO' {around = removeTestElements} $ do
  addParagraphSpec
