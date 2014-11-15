{-# LANGUAGE OverloadedStrings #-}

module Class where

import Sample

singleElement :: DOMElement
singleElement = let
  innerData = InnerData ("Firma1") 8
  reactData = ReactData {
    render = render'
    , componentDidMount = return ()
    , displayName = "SpanClass"
    , getInitialState = innerData
  }
  in classInstance (declareReactClass reactData)
