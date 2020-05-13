{-# LANGUAGE OverloadedStrings #-}
module Concur.React.Run where

import           Concur.Core            (SuspendF (..),
                                         Widget (..))
import           Concur.React.DOM       (DOMNode, HTML, bakeAttrs,
                                         bakeReactHTML, documentBody,
                                         mkReactParent, renderReactDOM)
import           Control.Monad.Free     (Free (..))
import           GHCJS.Marshal.Pure     (pToJSVal)
import           GHCJS.Types            (JSString)

runWidgetInBody :: Widget HTML a -> IO a
runWidgetInBody w = do
  let root = documentBody
  runWidget w root

runWidget :: Widget HTML a -> DOMNode -> IO a
runWidget (Widget w) root = go w
  where
    go :: Free (SuspendF HTML) a -> IO a
    go (Free (StepView v next)) = do
      let tag = pToJSVal ("div" :: JSString)
      attrs <- bakeAttrs []
      html <- bakeReactHTML v
      renderReactDOM root (mkReactParent tag attrs html)
      go next
    go (Free (StepIO io next)) = fmap next io >>= go
    go (Free (StepBlock io next)) = fmap next io >>= go
    go (Free Forever) = error "Application diverged!"
    go (Pure a) = do
      putStrLn "WARNING: Application exited: This may have been unintentional!"
      pure a
