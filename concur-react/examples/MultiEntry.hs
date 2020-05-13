{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad        (forever, void)
import           Control.Monad.State  (execStateT, get, put, lift)
import           Data.JSString (JSString)

import           Concur.Core
import           Concur.React

type MenuItems a = [(a,JSString)]

-- A Double menu, where the entries in the second menu depend on the first
doubleMenu :: JSString -> JSString -> MenuItems a -> (a -> MenuItems b) -> Widget HTML b
doubleMenu label1 label2 items f = menu1 >>= go
  where
    menu1 = menu label1 items
    menu2 a = menu label2 (f a)
    go a = orr [fmap Left menu1, fmap Right (menu2 a)] >>= either go return

-- A simple select menu
menu :: JSString -> MenuItems a -> Widget HTML a
menu label items = el "div" [vattr "className" "menu"] $ do
  button [] (text label)
  orr $ map menuButton items
  where
    menuButton (ret,str) = ret <$ button [] (text str)

-- State
data EntryState = EntryState { color :: JSString, items :: [JSString] }
data EntriesState = EntriesState { entries :: [EntryState] }

entryStateInit :: EntryState
entryStateInit = EntryState "Red" []
entriesStateInit :: Int -> EntriesState
entriesStateInit n = EntriesState $ replicate n entryStateInit

-- Widget that allows the user to add an item to an entry
entryWidget :: EntryState -> Widget HTML EntryState
entryWidget (EntryState {..}) = go color
  where
    go col = do
      w <- el "div" [vattr "className" "main"] $ orr
        [ elLeaf "hr" []
        , heading "Select a color"
        , Left <$> selColor
        , heading "Make entries"
        , Right <$> newEntry col
        , heading "Current entries"
        , entriesList
        ]
      either go (\e -> return (EntryState col (e:items))) w
    heading = el "h4" [] . text
    selColor = doubleMenu "Fruits" "Color" itemsFruit itemsColor
    newEntry col = menu ("New Entry for " <> col <> " fruit") (itemsFruitColor col)
    entriesList = orr $ map (el "div" [] . text) items

-- Main
main :: IO ()
main = void $ runWidgetInBody $ flip execStateT (entriesStateInit 5) $ forever $ do
    EntriesState {..} <- get
    (i, e') <- lift $ orr (renderEntry <$> zip [0..] entries)
    put $ EntriesState (take i entries ++ [e'] ++ drop (i+1) entries)
  where
    renderEntry (i,e) = (i,) <$> entryWidget e

-- Items for first menu
itemsFruit :: MenuItems JSString
itemsFruit =
  [ ("Apple","Apple")
  , ("Banana","Banana")
  ]

-- Items for second menu
itemsColor :: JSString -> MenuItems JSString
itemsColor "Apple" =
  [ ("Red","Red")
  , ("Green","Green")
  ]
itemsColor "Banana" =
  [ ("Yellow","Yellow")
  , ("Green","Green")
  ]
itemsColor _ = itemsColor "Apple"

-- Items for selecting fruit from color
itemsFruitColor :: JSString -> MenuItems JSString
itemsFruitColor "Red" =
  [ ("Apple","Apple")
  ]
itemsFruitColor "Green" =
  [ ("Apple","Apple")
  , ("Banana","Banana")
  ]
itemsFruitColor "Yellow" =
  [ ("Banana","Banana")
  ]
itemsFruitColor _ = itemsFruitColor "Red"
