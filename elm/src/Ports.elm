port module Ports exposing
    ( createTab
    , createWindow
    , extractTabs
    , focusTab
    , moveTabs
    , openUrl
    , pinTabs
    , removeTabs
    , sortTabs
    , updatedTree
    )

-- Output


port createTab : Int -> Cmd msg


port createWindow : () -> Cmd msg


port removeTabs : List Int -> Cmd msg


port extractTabs : List Int -> Cmd msg


port sortTabs : List Int -> Cmd msg


port pinTabs : List Int -> Cmd msg


port focusTab : Int -> Cmd msg


port openUrl : String -> Cmd msg


port moveTabs : ( List Int, Int, Int ) -> Cmd msg



-- Input


port updatedTree : (String -> msg) -> Sub msg
