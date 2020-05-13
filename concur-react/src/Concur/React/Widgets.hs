{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Concur.React.Widgets
  ( MonadWidget
    -- * Ready made widgets
  , text
  , textS
  , button
  , inputEnter
  , inputEnterShowRead

    -- * Custom widgets
  , Comp(..)
  , el
  , elEvent
  , elLeaf
  , mkEventHandlerAttr
  ) where

import Concur.Core
import Control.Monad (when)
import Control.ShiftMap
import qualified Data.JSString as JSS
import GHCJS.Types (JSString, JSVal)
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal))
import Data.String (IsString(..))
import Data.Void (Void, absurd)
import Text.Read (readMaybe)
import Prelude hiding (div)
import Concur.React.DOM

-- | A monad capable of rendering and interacting with widgets must satisfy
-- these constraints.
--
-- Note that 'Widget HTML' satisfies these constaints.
type MonadWidget m
  = ( ShiftMap (Widget HTML) m
    , MonadUnsafeBlockingIO m
    , MonadSafeBlockingIO m
    , MultiAlternative m
    , Monad m
    )

-- | React component.
data Comp
  = CompTag !JSString  -- ^ A react component by tag name.
  | CompRef !JSVal     -- ^ A react component by reference.

instance PToJSVal Comp where
  pToJSVal (CompRef v) = v
  pToJSVal (CompTag s) = pToJSVal s

-- |
-- @
-- 'fromString' == 'CompTag'
-- @
instance IsString Comp where
  fromString = CompTag . fromString

-- | A text widget.
--
-- Note: Since we can't have a top level text in React, this is rendered in a
-- @span@.
text :: JSString -> Widget HTML a
text txt = display [vtext txt]

-- | Like 'text', but takes a 'String'.
textS :: String -> Widget HTML a
textS = text . fromString

-- | A React component.
el
  :: ShiftMap (Widget HTML) m
  => Comp     -- ^ React component.
  -> [VAttr]  -- ^ Attributes.
  -> m a      -- ^ Child.
  -> m a
el c attrs = shiftMap (wrapView (vnode (pToJSVal c) attrs))

-- | A leaf React component.
elLeaf
  :: Comp
  -> [VAttr]
  -> Widget HTML a -- ^
elLeaf e attrs = display [vleaf (pToJSVal e) attrs]

mkEventHandlerAttr
  :: (Monad m, MonadUnsafeBlockingIO m, MonadSafeBlockingIO n)
  => JSString
  -- ^ Event name (e.g. @\"onClick\"@).
  -> m (VAttr, n DOMEvent)
  -- ^ Attribute to add to an element.
  --
  --   Blocking action to get the event payload.
mkEventHandlerAttr evtName = do
  n <- liftUnsafeBlockingIO newNotify
  let attr = VAttr evtName $ Right (notify n)
  return (attr, liftSafeBlockingIO (await n))

-- | Handle arbitrary events on an element.
elEvent
  :: MonadWidget m
  => JSString        -- ^ Event name.
  -> (DOMEvent -> a) -- ^ Event handler.
  -> Comp            -- ^ React component.
  -> [VAttr]         -- ^ Attributes.
  -> m a             -- ^ Child.
  -> m a
elEvent evtName xtract e attrs child = do
  (a, w) <- mkEventHandlerAttr evtName
  orr [fmap xtract w, el e (a : attrs) child]

-- | A clickable button widget.
button
  :: MonadWidget m
  => [VAttr] -- ^ Attributes.
  -> m Void  -- ^ Child producing no events. If your child produces events, use
             -- 'elEvent'.
  -> m ()
button attrs ch =
  elEvent "onClick" (const ()) (CompTag "button") attrs (fmap absurd ch)

-- Text input with an initial value.
--
-- Returns the contents on keypress enter.
inputEnter
  :: JSString  -- ^ Initial value.
  -> [VAttr]   -- ^ Attributes.
  -> Widget HTML JSString
inputEnter curVal attrs = awaitViewAction $ \n ->
   let evtattr = vevt "onKeyDown" (handleKey n)
       valattr = vattr "defaultValue" curVal
   in [vleaf (pToJSVal (CompTag "input"))
             (evtattr : valattr : attrs)]
 where
   handleKey :: Notify JSString -> DOMEvent -> IO ()
   handleKey n = \e -> do
     let e' :: JSVal = unDOMEvent e
     when (getProp "key" e' == "Enter") $ do
       notify n $! getProp "value" $ getPropObj "target" e'

-- | Text input for a value that can be 'Show'n and 'Read'.
--
-- Returns the contents on keypress enter.
inputEnterShowRead
  :: forall a
  .  (Show a, Read a)
  => Maybe a  -- ^ Optional initial value.
  -> Widget HTML a
inputEnterShowRead = go . JSS.pack . maybe "" show
  where
    go :: JSString -> Widget HTML a
    go a0 = do
      a1 <- inputEnter a0 []
      maybe (go a1) pure (readMaybe (JSS.unpack a1))
