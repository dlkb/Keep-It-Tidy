port module Ports exposing
  ( createTab
  , createWindow
  , removeTabs
  , extractTabs
  , sortTabs
  , pinTabs
  , focusWindow
  , focusTab
  , moveTabs
  , updatedTree
  )

-- to avoid this bug: https://github.com/adityav/elm-ports-issue
import Json.Decode

type alias Bundle = (List Int, Int, Int)

-- Output

port createTab : Int -> Cmd msg

port createWindow : () -> Cmd msg

port removeTabs : List Int -> Cmd msg

port extractTabs : List Int -> Cmd msg

port sortTabs : List Int -> Cmd msg

port pinTabs : List Int -> Cmd msg

port focusWindow : Int -> Cmd msg

port focusTab : Int -> Cmd msg

port moveTabs : Bundle -> Cmd msg

-- Input

port updatedTree : (String -> msg) -> Sub msg -- when tabs get removed, added or moved