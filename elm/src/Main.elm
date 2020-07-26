module Main exposing (main)

import Array
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Regex exposing (Regex)
import String


type alias Model =
    { windows : List Window
    , search : String
    , sortBy : Sort
    , prefs : Prefs
    , panel : Bool
    , colorOf : Dict Int String
    , processing : Bool
    , visited : List Int -- [last, next-to-last, ...]
    , groupChecked : Bool
    }


type alias Flags =
    { windows : String
    , visited : List Int
    , prefs : Prefs
    , colorOf : String
    }


type alias Prefs =
    { alwaysOnPanel : Bool
    , hints : Bool
    }


type alias Window =
    { id : Int
    , incognito : Bool
    , tabs : List Tab
    , enabled : Bool
    , index : Int
    }


type alias Tab =
    { id : Int
    , url : String
    , faviconUrl : String
    , title : String
    , pinned : Bool
    , windowId : Int
    , active : Bool
    , checked : CheckStatus
    , index : Int
    }


type CheckStatus
    = NotChecked
    | Checked
    | CheckedFromSearch


type Kind
    = Hint String
    | Other String


type Sort
    = Visited
    | Websites
    | Windows


type Edit
    = Not
    | Similar
    | Uncheck


type Action
    = Extract
    | Delete
    | Sort
    | Pin


type GlobalAction
    = HighlightDuplicates
    | DeleteAll
    | SortAll


type alias WindowInfos =
    { id : Int
    , incognito : Bool
    , tabsInfos : List TabInfos
    }


type alias TabInfos =
    { id : Int
    , url : String
    , faviconUrl : Maybe String
    , title : String
    , pinned : Bool
    , windowId : Int
    , active : Bool
    }


type Msg
    = NoOp
    | CheckboxClick Tab
    | TabClick Tab
    | SortBy Sort
    | GroupCheckedClick
    | SearchInput String
    | ClearSearch
    | CreateWindow
    | CreateTab Window
    | ToggleWindow Window
    | ToggleWindows
    | Execute Action
    | ClickGlobalButton GlobalAction
    | Apply Edit
    | UpdatedTree String -- String is a JSON encoded string
    | FocusTab Tab
    | Drop Int Int
    | EnterPressed
    | ClickPanelButton
    | DragPreviewContent


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        colorOf =
            case Decode.decodeString colorOfDecoder flags.colorOf of
                Ok dict ->
                    dict

                Err _ ->
                    Dict.empty

        initModel =
            Model [] "" Visited flags.prefs flags.prefs.alwaysOnPanel colorOf False flags.visited False

        enableAllWindows m =
            { m | windows = List.map (\window -> { window | enabled = True }) m.windows }

        filledModel =
            initModel |> buildTree flags.windows |> enableAllWindows |> paintWindows
    in
    ( filledModel
    , storeToLocalStorage filledModel
    )


buildWindow : Int -> WindowInfos -> Window
buildWindow index { id, incognito, tabsInfos } =
    { id = id
    , incognito = incognito
    , tabs = List.indexedMap buildTab tabsInfos
    , enabled = False
    , index = index
    }


buildTab : Int -> TabInfos -> Tab
buildTab index { id, url, faviconUrl, title, pinned, windowId, active } =
    let
        default =
            "img/favicon-16.png"

        fav =
            if String.left 4 url /= "http" then
                default

            else
                case faviconUrl of
                    Just favUrl ->
                        favUrl

                    Nothing ->
                        default
    in
    { id = id
    , url = url
    , faviconUrl = fav
    , title = title
    , pinned = pinned
    , windowId = windowId
    , active = active
    , checked = NotChecked
    , index = index
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CheckboxClick tab ->
            ( model |> updateTabs [ tab.id ] toggleChecked |> fossilizeSelection
            , Cmd.none
            )

        TabClick tab ->
            let
                enableWindow =
                    enableWindows [ tab.windowId ]

                updateTab =
                    updateTabs [ tab.id ] toggleChecked
            in
            ( model |> enableWindow |> updateTab |> fossilizeSelection
            , Cmd.none
            )

        SortBy sort ->
            ( { model | sortBy = sort }
            , Cmd.none
            )

        GroupCheckedClick ->
            ( { model | groupChecked = not model.groupChecked } |> fossilizeSelection
            , Cmd.none
            )

        SearchInput string ->
            let
                words =
                    string
                        |> String.trimLeft
                        |> String.trimRight
                        |> Regex.split (regexify " *, *")
                        |> List.map (Regex.split (regexify " +"))

                contains tab word =
                    if word == "" then
                        False

                    else
                        String.contains (String.toLower word) (String.toLower <| tab.url ++ " " ++ tab.title)

                match tab =
                    -- expl: SUPERMAN, BATMAN, POWER RANGERS = SUPERMAN or BATMAN or (POWER and RANGERS)
                    List.map (List.map (contains tab)) words
                        |> List.map (List.foldl (&&) True)
                        |> List.foldl (||) False

                updateSearchResults m =
                    let
                        lastSearchResults =
                            getCheckedFromSearch m
                                |> List.map .id

                        searchResults =
                            getList m
                                |> List.filter match
                                |> List.map .id

                        f tab =
                            -- We don't overwrite the check status of manually checked tabs
                            if tab.checked == Checked then
                                tab

                            else
                                { tab | checked = CheckedFromSearch }
                    in
                    m
                        |> updateTabs lastSearchResults (setChecked NotChecked)
                        |> updateTabs searchResults f

                updateField m =
                    { m | search = string }
            in
            ( model |> updateField |> moveSelectionTop |> updateSearchResults
            , Cmd.none
            )

        ClearSearch ->
            let
                clearField m =
                    { m | search = "" }

                uncheckMatches m =
                    updateTabs
                        (getCheckedFromSearch m |> List.map .id)
                        (setChecked NotChecked)
                        m
            in
            ( model |> clearField |> uncheckMatches
            , Cmd.none
            )

        CreateWindow ->
            ( model |> processingUI
            , Ports.createWindow ()
            )

        CreateTab window ->
            ( model |> processingUI
            , Ports.createTab window.id
            )

        ToggleWindow window ->
            ( model |> toggleWindow window.id |> fossilizeSelection
            , Cmd.none
            )

        ToggleWindows ->
            let
                enabledWindows =
                    model.windows
                        |> List.filter .enabled
                        |> List.map .id

                allWindows =
                    model.windows
                        |> List.map .id

                toggleWindows =
                    if allWindows /= enabledWindows then
                        enableWindows allWindows

                    else
                        disableWindows enabledWindows
            in
            ( model |> toggleWindows |> fossilizeSelection
            , Cmd.none
            )

        Execute action ->
            let
                checked =
                    List.map .id (getChecked model)
            in
            case action of
                Extract ->
                    ( model |> processingUI
                    , Ports.extractTabs checked
                    )

                Delete ->
                    ( model |> processingUI
                    , Ports.removeTabs checked
                    )

                Pin ->
                    ( model |> processingUI
                    , Ports.pinTabs checked
                    )

                Sort ->
                    ( model |> processingUI
                    , Ports.sortTabs checked
                    )

        ClickGlobalButton action ->
            let
                list =
                    List.map .id (getList model)

                windows =
                    getEnabledWindows model
            in
            case action of
                HighlightDuplicates ->
                    let
                        duplicates =
                            getDuplicates model

                        selectDuplicates =
                            updateTabs duplicates (setChecked Checked)
                    in
                    if List.length duplicates > 0 then
                        ( model |> selectDuplicates |> moveSelectionTop
                        , Cmd.none
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                DeleteAll ->
                    ( model |> processingUI
                    , Ports.removeWindows windows
                    )

                SortAll ->
                    ( model |> processingUI
                    , Ports.sortTabs list
                    )

        Apply edit ->
            case edit of
                Not ->
                    ( model |> invertSelection |> fossilizeSelection
                    , Cmd.none
                    )

                Similar ->
                    ( model |> selectSimilar |> fossilizeSelection
                    , Cmd.none
                    )

                Uncheck ->
                    ( model |> uncheckAll |> fossilizeSelection
                    , Cmd.none
                    )

        UpdatedTree toBeDecoded ->
            let
                updatedModel =
                    model |> onUpdatedTree toBeDecoded
            in
            ( updatedModel
            , storeToLocalStorage updatedModel
            )

        FocusTab tab ->
            ( model |> processingUI
            , Ports.focusTab tab.id
            )

        Drop windowId index ->
            ( model |> processingUI
            , Ports.moveTabs ( List.map .id (getChecked model), windowId, index )
            )

        EnterPressed ->
            let
                checked =
                    getChecked model

                checkedFromSearch =
                    getCheckedFromSearch model
            in
            if List.length checkedFromSearch == 0 && model.search /= "" then
                ( model |> processingUI
                , Ports.openUrl ("https://www.google.com/search?q=" ++ model.search)
                )

            else if List.length checked == 1 then
                case List.head checked of
                    Just tab ->
                        ( model |> processingUI
                        , Ports.focusTab tab.id
                        )

                    Nothing ->
                        ( model
                        , Cmd.none
                        )

            else
                ( model
                , Cmd.none
                )

        ClickPanelButton ->
            ( { model | panel = True }
            , Cmd.none
            )

        DragPreviewContent ->
            ( { model | panel = True }
            , Cmd.none
            )


paintWindows : Model -> Model
paintWindows model =
    let
        windowIds =
            List.map .id model.windows

        paint : List Int -> Model -> Model
        paint ids m =
            case ids of
                windowId :: etc ->
                    paint etc (paintWindow m windowId)

                [] ->
                    m
    in
    model |> cleanColorOf |> paint windowIds


cleanColorOf : Model -> Model
cleanColorOf model =
    let
        currentWindows =
            List.map .id model.windows

        storedWindows =
            Dict.keys model.colorOf

        toDelete =
            List.filter (\windowId -> not <| List.member windowId currentWindows) storedWindows

        cleanDict : List Int -> Dict Int String -> Dict Int String
        cleanDict ids dict =
            case ids of
                windowId :: etc ->
                    cleanDict etc (Dict.remove windowId dict)

                [] ->
                    dict
    in
    { model | colorOf = cleanDict toDelete model.colorOf }


paintWindow : Model -> Int -> Model
paintWindow model windowId =
    let
        colors =
            Array.fromList [ "#3cb44b", "#ffe119", "#4363d8", "#FF1493", "#ADFF2F", "#9370DB", "#FFA500", "#87CEFA", "violet", "#aaffc3", "#dcbeff", "#fabed4", "darkgreen", "darkblue", "darkred", "#00FF00", "#42d4f4", "#f032e6", "#808000", "#469990", "#9A6324" ]

        taken =
            Dict.values model.colorOf

        available =
            Array.filter (\color -> not <| List.member color taken) colors

        updatedColorOf =
            if Dict.member windowId model.colorOf then
                model.colorOf

            else if Array.isEmpty available then
                -- beyond XX windows, windows are gray because such a rare situation is not worth the hassle of generating random colors
                Dict.insert windowId "gray" model.colorOf

            else
                Dict.insert windowId (Maybe.withDefault "black" (Array.get 0 available)) model.colorOf
    in
    { model | colorOf = updatedColorOf }


buildTree : String -> Model -> Model
buildTree toBeDecoded model =
    let
        decoded =
            Decode.decodeString windowsDecoder toBeDecoded

        windowsInfos =
            case decoded of
                Ok value ->
                    removeEmptyWindows value

                Err _ ->
                    []

        removeEmptyWindows : List WindowInfos -> List WindowInfos
        removeEmptyWindows =
            List.filter (\windowInfos -> List.length windowInfos.tabsInfos > 0)
    in
    { model | windows = List.indexedMap buildWindow windowsInfos }


onUpdatedTree : String -> Model -> Model
onUpdatedTree toBeDecoded model =
    let
        enabledWindows =
            List.filter .enabled model.windows
                |> List.map .id

        checked =
            List.map .id (getChecked model)

        enableToolbar m =
            { m | processing = False }

        atLeastOneTabChecked tabs =
            case tabs of
                tab :: etc ->
                    isChecked tab || atLeastOneTabChecked etc

                [] ->
                    False

        restoreState m =
            let
                restoreWindow window =
                    if List.member window.id enabledWindows || atLeastOneTabChecked window.tabs then
                        { window | enabled = True }

                    else
                        { window | enabled = False }

                restoreTab tab =
                    if List.member tab.id checked then
                        { tab | checked = Checked }

                    else
                        { tab | checked = NotChecked }
            in
            m
                |> updateTabs (getTabIds m) restoreTab
                |> updateWindows (List.map .id m.windows) restoreWindow
    in
    model
        |> enableToolbar
        |> fossilizeSelection
        |> buildTree toBeDecoded
        |> restoreState
        |> paintWindows


storeToLocalStorage : Model -> Cmd Msg
storeToLocalStorage model =
    Ports.storeString ( "colorOf", Encode.encode 0 (colorOfEncoder model.colorOf) )


updateTabs : List Int -> (Tab -> Tab) -> Model -> Model
updateTabs tabIds f model =
    let
        updateTab tab =
            if List.member tab.id tabIds then
                f tab

            else
                tab

        windows =
            model.windows
                |> List.map (\window -> { window | tabs = List.map updateTab window.tabs })
    in
    { model | windows = windows }


enableWindows : List Int -> Model -> Model
enableWindows windowIds model =
    let
        updateWindow window =
            { window | enabled = True }
    in
    updateWindows windowIds updateWindow model


disableWindows : List Int -> Model -> Model
disableWindows windowIds model =
    let
        updateWindow window =
            let
                updatedTabs =
                    List.map (\tab -> { tab | checked = NotChecked }) window.tabs
            in
            { window | enabled = False, tabs = updatedTabs }
    in
    updateWindows windowIds updateWindow model


toggleWindow : Int -> Model -> Model
toggleWindow windowId model =
    let
        updateWindow window =
            let
                tabs =
                    if window.enabled then
                        List.map (\tab -> { tab | checked = NotChecked }) window.tabs

                    else
                        window.tabs
            in
            { window | enabled = not window.enabled, tabs = tabs }
    in
    updateWindows [ windowId ] updateWindow model


updateWindows : List Int -> (Window -> Window) -> Model -> Model
updateWindows windowIds f model =
    let
        updateWindow window =
            if List.member window.id windowIds then
                f window

            else
                window
    in
    { model | windows = List.map updateWindow model.windows }


fossilizeSelection : Model -> Model
fossilizeSelection model =
    let
        clearField m =
            { m | search = "" }

        -- Fossilize the checked tabs from search
        checkify m =
            let
                checkedFromSearch =
                    List.map .id (getCheckedFromSearch m)
            in
            updateTabs checkedFromSearch (setChecked Checked) m
    in
    model |> clearField |> checkify


toggleChecked : Tab -> Tab
toggleChecked tab =
    { tab
        | checked =
            case tab.checked of
                NotChecked ->
                    Checked

                Checked ->
                    NotChecked

                CheckedFromSearch ->
                    NotChecked
    }


setChecked : CheckStatus -> Tab -> Tab
setChecked status tab =
    { tab | checked = status }


getEnabledWindows : Model -> List Int
getEnabledWindows model =
    model.windows
        |> List.filter .enabled
        |> List.map .id


invertSelection : Model -> Model
invertSelection model =
    let
        selected =
            getList model
                |> List.map .id
    in
    updateTabs selected toggleChecked model


selectSimilar : Model -> Model
selectSimilar model =
    let
        checkedDomains =
            model
                |> getChecked
                |> List.map .url
                |> List.map schemeHostOf

        tabIds : List Tab -> List (Maybe String) -> List Int
        tabIds tabs domains =
            tabs
                |> List.filter (\tab -> List.member (schemeHostOf tab.url) domains)
                |> List.map .id
    in
    updateTabs (tabIds (getList model) checkedDomains) (setChecked Checked) model


schemeHostOf : String -> Maybe String
schemeHostOf url =
    Regex.findAtMost 1 (regexify "^.+?://[^/]+") url
        |> List.map .match
        |> List.head


processingUI : Model -> Model
processingUI model =
    { model | processing = True }


moveSelectionTop : Model -> Model
moveSelectionTop model =
    { model | groupChecked = True }


getDuplicates : Model -> List Int
getDuplicates model =
    let
        list =
            getList model

        -- https://en.wikipedia.org/wiki/Cattle#Etymology is considered a duplicate of https://en.wikipedia.org/wiki/Cattle
        truncate url =
            url
                |> Regex.replace (regexify "#.*$") (\_ -> "")

        f : List String -> List Tab -> List Int
        f urls tabs =
            case tabs of
                tab :: etc ->
                    if List.member (truncate tab.url) urls then
                        tab.id :: f urls etc

                    else
                        f (truncate tab.url :: urls) etc

                [] ->
                    []
    in
    f [] list


regexify : String -> Regex
regexify s =
    Maybe.withDefault Regex.never <| Regex.fromString s


uncheckAll : Model -> Model
uncheckAll model =
    updateTabs (getTabIds model) (setChecked NotChecked) model


setTooltip : Model -> Kind -> List (Html.Attribute Msg)
setTooltip model message =
    case message of
        Hint msg ->
            if model.prefs.hints then
                [ Attributes.title msg ]

            else
                []

        Other msg ->
            [ Attributes.title msg ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.updatedTree UpdatedTree
        , Browser.Events.onKeyDown keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toMsg (Decode.field "key" Decode.string)


toMsg : String -> Msg
toMsg string =
    if string == "Enter" then
        EnterPressed

    else
        NoOp


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.classList
            [ ( "main", True )
            , ( "expanded", model.panel )
            ]
        ]
        [ viewPanel model
        , viewOpenPanel model
        , viewTogglebar model
        , viewToolbar model
        , viewList model
        ]


viewPanel : Model -> Html Msg
viewPanel model =
    if model.panel then
        Html.div
            [ Attributes.class "panel" ]
            [ viewBrowserHeader model
            , Html.div
                [ Attributes.class "browser_container" ]
                [ viewBrowser model ]
            ]

    else
        Html.text ""


viewTogglebar : Model -> Html Msg
viewTogglebar model =
    if not model.panel then
        Html.div
            [ Attributes.class "togglebar" ]
            (List.map
                (viewToggleWindow model)
                model.windows
            )

    else
        Html.text ""


viewOpenPanel : Model -> Html Msg
viewOpenPanel model =
    Html.div
        ([ Attributes.class "openPanel"
         , Events.onClick ClickPanelButton
         ]
            ++ setTooltip model (Hint "Open the browser panel")
        )
        []


viewToggleWindow : Model -> Window -> Html Msg
viewToggleWindow model window =
    let
        color =
            if window.enabled then
                case Dict.get window.id model.colorOf of
                    Just c ->
                        c

                    Nothing ->
                        "black"

            else
                "white"

        class =
            if model.panel then
                ( "toggleWindow_panel", True )

            else
                ( "toggleWindow_togglebar", True )

        weightedHeight =
            let
                weight =
                    sqrt <| toFloat <| List.length window.tabs
            in
            if model.panel then
                []

            else
                [ Attributes.style "flex" (String.fromFloat weight) ]
    in
    Html.div
        ([ Attributes.classList
            [ ( "toggleWindow", True )
            , class
            ]
         , Attributes.style "background-color" color
         , Events.onClick (ToggleWindow window)
         ]
            ++ weightedHeight
            ++ setTooltip model (messageToggleWindow window.enabled)
        )
        []


viewBrowserHeader : Model -> Html Msg
viewBrowserHeader model =
    let
        nEnabledWindows =
            List.length (getEnabledWindows model)

        nWindows =
            List.length model.windows
    in
    Html.div
        [ Attributes.class "browserHeader" ]
        [ Html.div
            [ Attributes.class "browserHeader_caption" ]
            [ Html.text (String.fromInt nEnabledWindows ++ " / " ++ String.fromInt nWindows) ]
        , Html.div
            ([ Attributes.classList
                [ ( "toggleAllWindows", True )
                , ( "allEnabled", nEnabledWindows == nWindows )
                , ( "someEnabled", nEnabledWindows /= 0 && (nEnabledWindows /= nWindows) )
                ]
             , Events.onClick ToggleWindows
             ]
                ++ setTooltip model (messageToggleWindows (nEnabledWindows /= nWindows))
            )
            []
        ]


messageToggleWindows : Bool -> Kind
messageToggleWindows enable =
    if enable then
        Hint "Enable all windows"

    else
        Hint "Disable all windows"


viewBrowser : Model -> Html Msg
viewBrowser model =
    let
        createWindow =
            Html.div
                [ Attributes.class "createWindow_container" ]
                [ Html.div
                    ([ Attributes.class "createTab"
                     , Events.onClick CreateWindow
                     , Events.on "drop" (Decode.succeed <| Execute Extract)
                     ]
                        ++ setTooltip model (Hint "Open a new window")
                        ++ dropzonable
                    )
                    []
                ]

        windows =
            List.map (viewWindow model) model.windows
    in
    Html.div
        [ Attributes.class "browser" ]
        (windows ++ [ createWindow ])


viewWindow : Model -> Window -> Html Msg
viewWindow model window =
    let
        toggleWindowSideBar =
            viewToggleWindow model window

        createTab =
            Html.div
                ([ Attributes.class "createTab"
                 , Events.onClick (CreateTab window)
                 , Events.on "drop" (Decode.succeed <| Drop window.id -1)
                 ]
                    ++ setTooltip model (Hint "Open a new tab")
                    ++ dropzonable
                )
                []

        tabs =
            List.map (viewTab model) window.tabs

        container =
            Html.div
                [ Attributes.class "tabs_container" ]
                (tabs ++ [ createTab ])
    in
    if List.isEmpty tabs then
        Html.text ""
        --empty node

    else
        Html.div
            [ Attributes.classList
                [ ( "window", True )
                , ( "incognito", window.incognito )
                ]
            ]
            [ container, toggleWindowSideBar ]


messageToggleWindow : Bool -> Kind
messageToggleWindow enabled =
    if enabled then
        Hint "Disable this window"

    else
        Hint "Enable this window"


viewTab : Model -> Tab -> Html Msg
viewTab model tab =
    Html.div
        ([ Attributes.classList
            [ ( "tab", True )
            , ( "pinned", tab.pinned )
            , ( "checked", isChecked tab )
            , ( "active", tab.active )
            ]
         , Attributes.style
            "background-image"
            ("url(" ++ tab.faviconUrl ++ ")")
         , Events.onClick (TabClick tab)
         , Events.onDoubleClick (FocusTab tab)
         , Events.on "drop" (Decode.succeed <| Drop tab.windowId tab.index)
         ]
            ++ setTooltip model (Other tab.title)
            ++ dropzonable
        )
        []


dropzonable : List (Html.Attribute Msg)
dropzonable =
    [ Events.custom "dragover" (Decode.succeed { message = NoOp, preventDefault = True, stopPropagation = True })
    , Events.custom "dragenter" (Decode.succeed { message = NoOp, preventDefault = True, stopPropagation = True })
    ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    let
        checked =
            getChecked model

        right =
            if model.processing then
                [ viewProcessing ]

            else if List.isEmpty (getEnabledWindows model) then
                []

            else if List.isEmpty checked then
                [ viewNoPreview model ]

            else
                [ viewPreview model ]

        left =
            if List.isEmpty (getEnabledWindows model) then
                []

            else
                [ viewInput model
                , viewSortList model
                ]
    in
    Html.div
        [ Attributes.class "toolbar" ]
        [ Html.div
            [ Attributes.class "toolbar_left" ]
            left
        , Html.div
            [ Attributes.class "toolbar_right" ]
            right
        ]


viewProcessing : Html Msg
viewProcessing =
    Html.div
        [ Attributes.class "processing" ]
        [ Html.text "Processing..." ]


viewInput : Model -> Html Msg
viewInput model =
    Html.div
        [ Attributes.class "search_container" ]
        [ Html.input
            [ Attributes.class "search"
            , Attributes.value model.search
            , Events.onInput SearchInput
            , Attributes.autofocus True
            , Attributes.placeholder "Search"
            ]
            []
        , Html.div
            ([ Attributes.classList
                [ ( "clearSearch", model.search /= "" ) ]
             , Events.onClick ClearSearch
             ]
                ++ setTooltip model (Hint "Clear the search field")
            )
            []
        ]


viewEditSelection : Model -> Html Msg
viewEditSelection model =
    let
        edits =
            [ Not, Uncheck, Similar ]
    in
    Html.div
        [ Attributes.class "editSelection" ]
        (List.map (viewEditHelp model) edits)


viewEditHelp : Model -> Edit -> Html Msg
viewEditHelp model edit =
    let
        str =
            case edit of
                Not ->
                    "not"

                Similar ->
                    "similar"

                Uncheck ->
                    "uncheck"

        message =
            case edit of
                Not ->
                    Hint "Invert selection"

                Similar ->
                    Hint "Add similar tabs to the selection"

                Uncheck ->
                    Hint "Unselect all tabs"
    in
    Html.div
        ([ Attributes.classList
            [ ( "edit", True )
            , ( str, True )
            ]
         , Events.onClick (Apply edit)
         ]
            ++ setTooltip model message
        )
        []


viewGroupChecked : Model -> Html Msg
viewGroupChecked model =
    Html.div
        ([ Attributes.classList
            [ ( "groupChecked", True )
            , ( "enabled", model.groupChecked )
            ]
         , Events.onClick GroupCheckedClick
         ]
            ++ setTooltip model (messageGroupChecked model.groupChecked)
        )
        []


messageGroupChecked : Bool -> Kind
messageGroupChecked grouped =
    if grouped then
        Hint "Unstick selected tabs from the top of the list"

    else
        Hint "Stick selected tabs to the top of the list"


viewSortList : Model -> Html Msg
viewSortList model =
    let
        sorts =
            [ Visited, Websites, Windows ]
    in
    Html.div
        [ Attributes.class "sortList" ]
        (List.map
            (viewSortListHelp model)
            sorts
            ++ [ viewGroupChecked model ]
        )


viewSortListHelp : Model -> Sort -> Html Msg
viewSortListHelp model sort =
    let
        str =
            case sort of
                Visited ->
                    "visited"

                Websites ->
                    "website"

                Windows ->
                    "window"

        message =
            case sort of
                Visited ->
                    "Sort list by last visited"

                Websites ->
                    "Group tabs by website"

                Windows ->
                    "Group tabs by window"
    in
    Html.div
        ([ Attributes.classList
            [ ( "sortBy", True )
            , ( str, True )
            , ( "selected", model.sortBy == sort )
            ]
         , Events.onClick (SortBy sort)
         ]
            ++ setTooltip model (Hint message)
        )
        [ Html.text str ]


viewPreview : Model -> Html Msg
viewPreview model =
    let
        checked =
            getChecked model

        content tabs =
            let
                maxFavicons =
                    33

                lengthText =
                    -- in number of favicons
                    4

                nOthers =
                    if List.length checked - maxFavicons > 0 then
                        List.length checked - maxFavicons + lengthText

                    else
                        List.length checked - maxFavicons
            in
            case tabs of
                tab :: etc ->
                    if List.length etc == nOthers && nOthers /= 0 then
                        [ viewPreviewHelp tab
                        , Html.div
                            [ Attributes.class "nOtherTabs" ]
                            [ Html.text ("+ " ++ String.fromInt nOthers ++ " tabs") ]
                        ]

                    else
                        viewPreviewHelp tab :: content etc

                [] ->
                    []
    in
    Html.div
        [ Attributes.class "preview" ]
        [ viewEditSelection model
        , Html.div
            [ Attributes.class "preview_content" ]
            [ Html.div
                ([ Attributes.class "preview_favicons"
                 , Events.custom "drag" (Decode.succeed { message = DragPreviewContent, preventDefault = False, stopPropagation = False })
                 , Attributes.draggable "true"
                 ]
                    ++ setTooltip model (Hint "You can drag these tabs to a new position")
                )
                (content checked)
            ]
        , viewExecuteAction model
        ]


viewNoPreview : Model -> Html Msg
viewNoPreview model =
    let
        globalActions =
            [ HighlightDuplicates, DeleteAll, SortAll ]
    in
    Html.div
        [ Attributes.class "noPreview" ]
        (List.map (viewGlobalActionHelp model) globalActions)


viewGlobalActionHelp : Model -> GlobalAction -> Html Msg
viewGlobalActionHelp model action =
    let
        str =
            case action of
                HighlightDuplicates ->
                    "duplicates"

                DeleteAll ->
                    "delete"

                SortAll ->
                    "sort"

        message =
            case action of
                HighlightDuplicates ->
                    "Select all duplicates"

                DeleteAll ->
                    "Remove all tabs"

                SortAll ->
                    "Sort all tabs by website"
    in
    Html.div
        ([ Attributes.classList
            [ ( "globalAction", True )
            , ( str, True )
            ]
         , Events.onClick (ClickGlobalButton action)
         ]
            ++ setTooltip model (Hint message)
        )
        []


viewPreviewHelp : Tab -> Html Msg
viewPreviewHelp tab =
    Html.div
        [ Attributes.class "preview_favicon"
        , Attributes.style
            "background-image"
            ("url(" ++ tab.faviconUrl ++ ")")
        ]
        []


viewExecuteAction : Model -> Html Msg
viewExecuteAction model =
    let
        actions =
            [ Extract, Delete, Sort, Pin ]
    in
    Html.div
        [ Attributes.class "executeAction" ]
        (List.map (viewExecuteActionHelp model) actions)


viewExecuteActionHelp : Model -> Action -> Html Msg
viewExecuteActionHelp model action =
    let
        str =
            case action of
                Extract ->
                    "extract"

                Delete ->
                    "delete"

                Sort ->
                    "sort"

                Pin ->
                    "pin"

        message =
            case action of
                Extract ->
                    "Extract tabs to a new window"

                Delete ->
                    "Remove tabs"

                Sort ->
                    "Sort tabs by website"

                Pin ->
                    "Pin/unpin tabs"
    in
    Html.div
        ([ Attributes.classList
            [ ( "action", True )
            , ( str, True )
            ]
         , Events.onClick (Execute action)
         ]
            ++ setTooltip model (Hint message)
        )
        []


viewList : Model -> Html Msg
viewList model =
    Html.div
        [ Attributes.class "list_container" ]
        [ Html.div
            [ Attributes.class "list" ]
            (List.map (viewItem model) (getList model))
        ]


viewItem : Model -> Tab -> Html Msg
viewItem model tab =
    let
        windowColor =
            case Dict.get tab.windowId model.colorOf of
                Just c ->
                    c

                Nothing ->
                    "black"

        favicon =
            Html.div
                [ Attributes.class "item_favicon"
                , Attributes.style
                    "background-image"
                    ("url(" ++ tab.faviconUrl ++ ")")
                , Events.onClick (FocusTab tab)
                ]
                []

        title =
            let
                text =
                    if tab.title /= "" then
                        tab.title

                    else
                        tab.url
            in
            Html.div
                [ Attributes.classList
                    [ ( "item_title", True )
                    , ( "checkedFromSearch", tab.checked == CheckedFromSearch )
                    ]
                , Events.onClick (FocusTab tab)
                ]
                [ Html.text text ]

        website =
            let
                text =
                    let
                        strip : String -> Maybe String
                        strip schemeHost =
                            if String.left 8 schemeHost == "https://" then
                                Just (String.slice 8 (String.length schemeHost) schemeHost)

                            else
                                Just schemeHost
                    in
                    Maybe.withDefault "" (tab.url |> schemeHostOf |> Maybe.andThen strip)
            in
            Html.div
                [ Attributes.class "item_website"
                , Events.onClick (FocusTab tab)
                ]
                [ Html.text text ]

        checkbox =
            Html.div
                [ Attributes.class "item_checkbox"
                , Attributes.style "background-color" windowColor
                , Events.onClick (CheckboxClick tab)
                ]
                [ Html.div
                    [ Attributes.classList
                        [ ( "item_check", True )
                        , ( "checked", isChecked tab )
                        , ( "visible", List.length (getChecked model) > 0 )
                        ]
                    ]
                    []
                ]
    in
    Html.div
        [ Attributes.classList
            [ ( "item", True )
            , ( "checked", isChecked tab )
            ]
        ]
        [ favicon, title, website, checkbox ]


isChecked : Tab -> Bool
isChecked tab =
    tab.checked == Checked || tab.checked == CheckedFromSearch


getList : Model -> List Tab
getList model =
    let
        tabs =
            model.windows
                |> List.filter .enabled
                |> List.map .tabs
                |> List.concat

        augmentedSortByFor tab sortBy =
            if model.groupChecked && tab.checked == CheckedFromSearch then
                ( 1, sortBy )

            else if model.groupChecked && tab.checked == Checked then
                ( 2, sortBy )

            else
                ( 3, sortBy )
    in
    case model.sortBy of
        Visited ->
            List.sortBy
                (\tab -> augmentedSortByFor tab (getIndexOf tab.id model.visited))
                tabs

        Websites ->
            List.sortBy
                (\tab -> augmentedSortByFor tab tab.url)
                tabs

        Windows ->
            let
                indexInBrowser tab =
                    case getIndexOfWindow tab.windowId model of
                        Just i ->
                            ( i, tab.index )

                        Nothing ->
                            ( List.length model.windows, tab.index )
            in
            List.sortBy
                (\tab -> augmentedSortByFor tab (indexInBrowser tab))
                tabs


getIndexOfWindow : Int -> Model -> Maybe Int
getIndexOfWindow windowId model =
    model.windows
        |> List.filter (\window -> window.id == windowId)
        |> List.map .index
        |> List.head


getIndexOf : Int -> List Int -> Int
getIndexOf tabId tabIds =
    let
        helper : List Int -> Int -> Int -> Int
        helper lst elem offset =
            case lst of
                [] ->
                    offset + 1

                x :: xs ->
                    if x == elem then
                        offset

                    else
                        helper xs elem (offset + 1)
    in
    helper tabIds tabId 0


getChecked : Model -> List Tab
getChecked model =
    getList model
        |> List.filter (\tab -> isChecked tab)


getCheckedFromSearch : Model -> List Tab
getCheckedFromSearch model =
    getChecked model
        |> List.filter (\tab -> tab.checked == CheckedFromSearch)


getTabIds : Model -> List Int
getTabIds model =
    model.windows
        |> List.map .tabs
        |> List.concat
        |> List.map .id



-- JSON


colorOfEncoder : Dict Int String -> Encode.Value
colorOfEncoder colorOf =
    let
        tupleEncoder : ( Int, String ) -> Encode.Value
        tupleEncoder ( windowId, color ) =
            Encode.object
                [ ( "windowId", Encode.int windowId )
                , ( "color", Encode.string color )
                ]
    in
    Encode.list tupleEncoder <| Dict.toList colorOf


colorOfDecoder : Decode.Decoder (Dict Int String)
colorOfDecoder =
    let
        tupleDecoder =
            Decode.map2 Tuple.pair
                (Decode.field "windowId" Decode.int)
                (Decode.field "color" Decode.string)
    in
    Decode.list tupleDecoder
        |> Decode.map Dict.fromList


windowsDecoder : Decode.Decoder (List WindowInfos)
windowsDecoder =
    Decode.list windowDecoder


windowDecoder : Decode.Decoder WindowInfos
windowDecoder =
    Decode.map3 WindowInfos
        (Decode.field "id" Decode.int)
        (Decode.field "incognito" Decode.bool)
        (Decode.field "tabs" (Decode.list tabDecoder))


tabDecoder : Decode.Decoder TabInfos
tabDecoder =
    Decode.map7 TabInfos
        (Decode.field "id" Decode.int)
        (Decode.field "url" Decode.string)
        (Decode.maybe (Decode.field "favIconUrl" Decode.string))
        (Decode.field "title" Decode.string)
        (Decode.field "pinned" Decode.bool)
        (Decode.field "windowId" Decode.int)
        (Decode.field "active" Decode.bool)
