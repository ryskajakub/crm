{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap where

import "fay-base" FFI (ffi, Automatic, Defined(Defined, Undefined), Nullable(Null))
import "fay-base" Data.Text (fromString, Text, showInt, (<>))
import "fay-base" Prelude

import HaskellReact

data ReactBootstrap
instance CommonJSModule ReactBootstrap

requireReactBootstrap :: ReactBootstrap
requireReactBootstrap = ffi " require('react-bootstrap') "

-- | Creates an instance of a React Bootstrap class
reactBootstrap :: (Renderable b)
               => Text -- ^ The name of the Bootstrap class
               -> a -- ^ The props passed to the instance
               -> b -- ^ The children passed to the instance
               -> DOMElement
reactBootstrap = foreignReact requireReactBootstrap

data Technical1 = Technical1 {
  role :: Text }

data TooltipData = TooltipData {
  data_toggle :: Text ,
  data_placement :: Text ,
  title :: Text }

technical1 :: Technical1
technical1 = Technical1 "navigation"

tooltip :: Renderable a
        => TooltipData
        -> a
        -> DOMElement
tooltip tooltipData child = constructDOMElement "span" mkAttrs tooltipData child

navBar' :: Renderable a
        => (Attributes -> Attributes)
        -> a
        -> DOMElement
navBar' modifyAttrs children = 
  constructDOMElement "nav" (modifyAttrs $ class'' ["navbar", "navbar-default"]) technical1 $
    div' (class' "container") children

navBar :: Renderable a
       => a
       -> DOMElement
navBar children = navBar' Prelude.id children

nav :: Renderable a
    => a 
    -> DOMElement
nav children = ul' (class'' ["nav", "navbar-nav"]) children

table :: Renderable a
      => Automatic a
      -> DOMElement
table = reactBootstrap "Table" Null

row' :: Renderable a
     => (Attributes -> Attributes)
     -> a -- ^ children
     -> DOMElement
row' modifyAttributes children = div' (modifyAttributes $ class' "row") children

row :: Renderable a
    => a
    -> DOMElement
row = row' Prelude.id

jumbotron :: Renderable a
          => Automatic a
          -> DOMElement
jumbotron = reactBootstrap "Jumbotron" Null

grid :: Renderable a
     => Automatic a
     -> DOMElement
grid = reactBootstrap "Grid" Null

data ColProps = ColProps {
  md :: Int , 
  mdOffset :: Defined Int }

mkColProps :: Int -> ColProps
mkColProps int = ColProps int Undefined

col' :: Renderable a
     => ColProps
     -> Defined Text
     -> a
     -> DOMElement
col' colProps key' = let
  md' = ["col-md-" <> (showInt $ md colProps)]
  mdOffset' = case mdOffset colProps of
    Defined int -> ["col-md-offset-" <> showInt int]
    _ -> []
  bothClasses = mdOffset' ++ md'
  in div' ((class'' bothClasses) {key = key' })

col :: Renderable a
    => ColProps
    -> a
    -> DOMElement
col props children = col' props Undefined children

panel :: Renderable a
      => Automatic a
      -> DOMElement
panel = reactBootstrap "Panel" Null
