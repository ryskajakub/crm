{-# LANGUAGE OverloadedStrings #-}

module Class where

import Sample

twoClasses :: Fay ()
twoClasses = do
  let
    afterMount = putStrLn("component did mount!!!")
    innerData = InnerData ("Firma1") 8
    reactData = ReactData {
      render = render'
      , componentDidMount = afterMount
      , displayName = "SpanClass"
      , getInitialState = innerData
    }
    spanClass = classInstance (declareReactClass reactData)
  putStrLn "ahoj"
  -- placeElement (constructDOMElementArray "div" (Attributes "blue" (return ())) [spanClass, differentClass])
