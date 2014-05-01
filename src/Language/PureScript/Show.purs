module Language.PureScript.Show where

foreign import defaultShow 
  "var defaultShow = JSON.stringify" :: forall a. a -> String
