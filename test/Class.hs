module Class where

import Sample
import Fay.Text (pack)

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
