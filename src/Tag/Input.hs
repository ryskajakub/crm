module Tag.Input where

import HaskellReact
import FFI

newtype InputType = InputType { inputType :: String }

data InputAttributes = InputAttributes {
  type_ :: InputType
  , value :: Maybe String
  , checked :: Maybe String
  , onChange :: Maybe (SyntheticEvent -> Fay ())
}

defaultInputAttributes = InputAttributes {
  type_ = InputType "text"
  , value = Nothing
  , checked = Nothing
  , onChange = Nothing
}

input :: (Renderable a) => Attributes -> InputAttributes -> a -> DOMElement
input = ffi " constructDOMElement(\"input\", %1, Fay$$_(%3), %2) "
