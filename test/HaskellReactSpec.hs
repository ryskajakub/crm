module HaskellReactSpec where

import HaskellReact
import Fay.Text (pack)

render' :: (InnerData, SetState InnerData) -> DOMElement
render' (d, ss) = let
  text = companyName d
  click2 = setState ss (InnerData (pack "AAAAAAAAAAAA") 5)
  in constructDOMElement "span" (Attributes "blue" click2) text

singleElement :: DOMElement
singleElement = let
  innerData = InnerData (pack "Firma1") 8
  reactData = ReactData {
    render = render'
    , componentDidMount = return ()
    , displayName = "SpanClass"
    , getInitialState = innerData
  }
  in classInstance (declareReactClass reactData)
