port module Ports exposing
    ( createTab
    , createWindow
    , extractTabs
    , focusTab
    , focusWindow
    , moveTabs
    , pinTabs
    , removeDuplicates
    , removeTabs
    , sortTabs
    , updatedTree
    )


type alias Bundle =
    ( List Int, Int, Int )



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


port removeDuplicates : List Int -> Cmd msg



-- Input


port updatedTree : (String -> msg) -> Sub msg
