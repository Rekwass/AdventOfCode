{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Lib.MyDialog
  ( Dialog
  , dialogTitle
  , dialogButtons
  , dialogWidth
  -- * Construction and rendering
  , dialog
  , renderDialog
  , getDialogFocus
  , setDialogFocus
  -- * Handling events
  , handleDialogEvent
  -- * Getting a dialog's current value
  , dialogSelection
  -- * Attributes
  , dialogAttr
  , buttonAttr
  , buttonSelectedAttr
  -- * Lenses
  , dialogButtonsL
  , dialogWidthL
  , dialogTitleL
  )
where

import Lens.Micro
import Lens.Micro.Mtl ((%=))
import Data.List (intersperse, find)
import Graphics.Vty.Input (Event(..), Key(..))

import Brick.Focus
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.AttrMap
import Data.List.Split (chunksOf)

data Dialog a n =
    Dialog { dialogTitle :: Widget n
           , dialogButtons :: [(String, n, a)]
           , dialogWidth :: Int
           , dialogFocus :: FocusRing n
           }

suffixLenses ''Dialog

handleDialogEvent :: Event -> EventM n (Dialog a n) ()
handleDialogEvent ev = do
    case ev of
        EvKey KUp    [] -> dialogFocusL %= focusAbove
        EvKey KRight [] -> dialogFocusL %= focusNext
        EvKey KDown  [] -> dialogFocusL %= focusBelow
        EvKey KLeft  [] -> dialogFocusL %= focusPrev
        _ -> return ()
  where
    focusAbove focus = iterate focusPrev focus !! 5
    focusBelow focus = iterate focusNext focus !! 5

setDialogFocus :: (Eq n) => n -> Dialog a n -> Dialog a n
setDialogFocus n d = d { dialogFocus = focusSetCurrent n $ dialogFocus d }

getDialogFocus :: Dialog a n -> Maybe n
getDialogFocus = focusGetCurrent . dialogFocus

dialog :: (Eq n) => Widget n -> Maybe (n, [(String, n, a)]) -> Int -> Dialog a n
dialog title buttonData w =
    let (r, buttons) = case buttonData of
            Nothing                 -> (focusRing [], [])
            Just (focName, entries) ->
                let ns = (\(_, n, _) -> n) <$> entries
                in (focusSetCurrent focName $ focusRing ns, entries)
    in Dialog title buttons w r

dialogAttr :: AttrName
dialogAttr = attrName "dialog"

buttonAttr :: AttrName
buttonAttr = attrName "button"

buttonSelectedAttr :: AttrName
buttonSelectedAttr = buttonAttr <> attrName "selected"

renderDialog :: (Ord n) => Dialog a n -> Widget n
renderDialog d =
    let buttonPadding = str "   "
        foc = focusGetCurrent $ dialogFocus d
        mkButton (s, n, _) =
            let att = if Just n == foc
                      then buttonSelectedAttr
                      else buttonAttr
                csr = if Just n == foc
                      then putCursor n (Location (1,0))
                      else id
            in csr $
               clickable n $
               withAttr att $
               str $ "  " <> s <> "  "
        buttons = mkButton <$> (d^.dialogButtonsL)
        btnRows = hBox . intersperse buttonPadding
        realBtnRow = map btnRows $ chunksOf 5 buttons
        vertBtns = vBox $ mapTail (padTop (Pad 1)) realBtnRow
        doBorder = borderWithLabel (d^.dialogTitleL)
    in centerLayer $
       withDefAttr dialogAttr $
       hLimit (d^.dialogWidthL) $
       doBorder $
       vBox [
       str " ", hCenter vertBtns, str " "
       ]

mapTail :: (a -> a) -> [a] -> [a]
mapTail _ []     = []
mapTail _ [x]    = [x]
mapTail f (x:xs) = x : map f xs

dialogSelection :: (Eq n) => Dialog a n -> Maybe (n, a)
dialogSelection d = do
    n' <- focusGetCurrent $ dialogFocus d
    let matches (_, n, _) = n == n'
    (_, n, a) <- find matches (d^.dialogButtonsL)
    return (n, a)
