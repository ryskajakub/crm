{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Tag.Input (
  InputAttributes(..) ,
  defaultInputAttributes ,
  mkInputAttrs ,
  input ,
  textarea ,
  InputType ,
  button ,
  checkbox ,
  password' ,
  color' ,
  date ,
  datetime ,
  datetimeLocal ,
  email ,
  file ,
  hidden ,
  image ,
  month ,
  number ,
  password ,
  radio ,
  range ,
  reset ,
  search ,
  submit ,
  tel ,
  text ,
  time ,
  url ,
  week ) where

import "fay-base" FFI
import "fay-base" Prelude
import "fay-base" Data.Text (Text, fromString)

import HaskellReact.Tag.Construct
import HaskellReact.Event

type InputType = Text

button :: InputType
button = "button"

checkbox :: InputType
checkbox = "checkbox"

color' :: InputType
color' = "color"

date :: InputType
date = "date"

datetime :: InputType
datetime = "datetime"

datetimeLocal :: InputType
datetimeLocal = "datetime-local"

email :: InputType
email = "email"

file :: InputType
file = "file"

hidden :: InputType
hidden = "hidden"

image :: InputType
image = "image"

month :: InputType
month = "month"

number :: InputType
number = "number"

password' :: InputType
password' = "password"

radio :: InputType
radio = "radio"

range :: InputType
range = "range"

reset :: InputType
reset = "reset"

search :: InputType
search = "search"

submit :: InputType
submit = "submit"

tel :: InputType
tel = "tel"

text :: InputType
text = "text"

time :: InputType
time = "time"

url :: InputType
url = "url"

week :: InputType
week = "week"


data InputAttributes = InputAttributes {
  type_ :: InputType , 
  value_ :: Defined Text , 
  defaultValue :: Defined Text , 
  checked :: Defined Text , 
  onChange :: Defined (SyntheticEvent -> Fay ()) ,
  onPaste :: Defined (SyntheticClipboardEvent -> Fay ()) ,
  disabled_ :: Defined Text ,
  name :: Defined Text ,
  onKeyDown :: Defined (SyntheticKeyboardEvent -> Fay ()) ,
  onKeyPress :: Defined (SyntheticKeyboardEvent -> Fay ()) ,
  onKeyUp :: Defined (SyntheticKeyboardEvent -> Fay ()) ,
  onBlur :: Defined (SyntheticEvent -> Fay ()) ,
  rows :: Defined Int ,
  cols :: Defined Int }

mkInputAttrs :: InputAttributes
mkInputAttrs = defaultInputAttributes

defaultInputAttributes :: InputAttributes
defaultInputAttributes = InputAttributes {
  type_ = text ,
  value_ = Undefined ,
  checked = Undefined ,
  onChange = Undefined ,
  defaultValue = Undefined ,
  disabled_ = Undefined ,
  name = Undefined ,
  onPaste = Undefined ,
  onKeyDown = Undefined ,
  onKeyPress = Undefined ,
  onKeyUp = Undefined ,
  onBlur = Undefined ,
  cols = Undefined ,
  rows = Undefined }

input' :: (Renderable a) => Attributes -> InputAttributes -> a -> DOMElement
input' = constructDOMElement "input"

input :: Attributes -> InputAttributes -> DOMElement
input a i = input' a i ([]::[DOMElement])

textarea :: Attributes -> InputAttributes -> DOMElement
textarea aAttrs iAttrs = constructDOMElement "textarea" aAttrs iAttrs (Null :: Nullable DOMElement)

password :: Attributes -> InputAttributes -> DOMElement
password attributes inputAttributes =
  input attributes $ inputAttributes { type_ = password' }
