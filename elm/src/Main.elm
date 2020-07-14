module Main exposing (main)

import Array
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import Ports
import Regex exposing (Regex)
import String


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { windows : List Window
    , search : String
    , sortBy : Sort
    , footer : Kind
    , visited : List TabId -- [last, next-to-last, ...]
    , groupChecked : Bool -- stick selected tabs to the top of the list
    }


type alias Window =
    { id : WindowId
    , incognito : Bool
    , tabs : List Tab
    , enabled : Bool
    , color : String
    , index : Int
    }


type alias Tab =
    { id : TabId
    , url : String
    , faviconUrl : String
    , title : String
    , pinned : Bool
    , windowId : Int
    , active : Bool
    , checked : CheckStatus
    , index : Int
    }


type alias TabId =
    Int


type alias WindowId =
    Int


type CheckStatus
    = NotChecked
    | Checked
    | CheckedFromSearch


type Kind
    = Url String
    | Hint String
    | Title String


type Sort
    = Visited
    | Websites
    | Windows


type Edit
    = Not
    | Similar
    | Duplicates
    | Uncheck


type Action
    = Extract
    | Delete
    | Sort
    | Pin


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
    | Apply Edit
    | UpdatedTree String -- String is a JSON encoded string
    | FocusTab Tab
    | SetMessage Kind
    | Drop WindowId Int
    | EnterPressed


type alias WindowInfos =
    { id : WindowId
    , incognito : Bool
    , tabsInfos : List TabInfos
    }


type alias TabInfos =
    { id : TabId
    , url : String
    , faviconUrl : Maybe String
    , title : String
    , pinned : Bool
    , windowId : WindowId
    , active : Bool
    }


type alias Flags =
    { windows : String
    , visited : List TabId
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initModel =
            Model [] "" Visited (Hint "Welcome!") flags.visited False

        buildTheTree m =
            onUpdatedTree flags.windows m

        enableAllWindows m =
            { m | windows = List.map (\window -> { window | enabled = True }) m.windows }
    in
    ( initModel |> buildTheTree |> enableAllWindows
    , Cmd.none
    )


buildWindow : Int -> WindowInfos -> Window
buildWindow index { id, incognito, tabsInfos } =
    let
        colors =
            Array.fromList [ "MediumAquaMarine", "MediumSlateBlue", "Coral", "SkyBlue", "DeepPink", "SlateGray", "Chartreuse", "SandyBrown", "DarkViolet", "DodgerBlue", "PaleVioletRed", "#39689a", "#96ff98", "#b63c77" ]

        color =
            Array.get (modBy (Array.length colors) index) colors

        picked =
            case color of
                Just c ->
                    c

                Nothing ->
                    "Black"
    in
    { id = id
    , incognito = incognito
    , tabs = List.indexedMap buildTab tabsInfos
    , enabled = False
    , color = picked
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
            ( { model | groupChecked = not model.groupChecked, footer = messageGroupChecked (not model.groupChecked) }
            , Cmd.none
            )

        SearchInput string ->
            let
                words =
                    string
                        |> Regex.replace (regexify "^ *") (\_ -> "")
                        |> Regex.replace (regexify " *$") (\_ -> "")
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

                -- Enable all windows in case the user has forgotten to enable any window
                enableAllIfNone m =
                    if List.isEmpty (getEnabledWindows m) then
                        enableWindows (List.map .id m.windows) m

                    else
                        m

                updateSearchResults m =
                    let
                        lastSearchResults =
                            getCheckedFromSearch m
                                |> List.map .id

                        searchResults =
                            getSelection m
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

                moveSelectionTop m =
                    { m | groupChecked = True }
            in
            ( model |> updateField |> enableAllIfNone |> moveSelectionTop |> updateSearchResults
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
            ( model
            , Ports.createWindow ()
            )

        CreateTab window ->
            ( model
            , Ports.createTab window.id
            )

        ToggleWindow window ->
            let
                updateMessage m =
                    { m | footer = messageToggleWindow <| not window.enabled }
            in
            ( model |> toggleWindow window.id |> updateMessage |> fossilizeSelection
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

                updateMessage m =
                    { m | footer = messageToggleWindows <| enabledWindows == allWindows }
            in
            ( model |> toggleWindows |> updateMessage |> fossilizeSelection
            , Cmd.none
            )

        Execute action ->
            let
                checked =
                    List.map .id (getChecked model)

                selected =
                    List.map .id (getSelection model)

                cmd =
                    case action of
                        Extract ->
                            if List.isEmpty checked then
                                Cmd.none

                            else
                                Ports.extractTabs checked

                        Delete ->
                            if List.isEmpty checked then
                                Cmd.none

                            else
                                Ports.removeTabs checked

                        Pin ->
                            if List.isEmpty checked then
                                Cmd.none

                            else
                                Ports.pinTabs checked

                        Sort ->
                            if List.isEmpty checked then
                                if List.isEmpty selected then
                                    Cmd.none

                                else
                                    -- if there are no checked tabs then we default to tabs from enabled windows
                                    Ports.sortTabs selected

                            else
                                Ports.sortTabs checked
            in
            ( model
            , cmd
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

                Duplicates ->
                    ( model |> selectDuplicates |> fossilizeSelection
                    , Cmd.none
                    )

                Uncheck ->
                    ( model |> uncheckAll |> fossilizeSelection
                    , Cmd.none
                    )

        UpdatedTree toBeDecoded ->
            ( model |> onUpdatedTree toBeDecoded
            , Cmd.none
            )

        FocusTab tab ->
            ( model
            , Ports.focusTab ( tab.id, tab.windowId )
            )

        SetMessage message ->
            ( { model | footer = message }
            , Cmd.none
            )

        Drop windowId index ->
            ( model
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
                ( model
                , Ports.openUrl ("https://www.google.com/search?q=" ++ model.search)
                )

            else if List.length checked == 1 then
                case List.head checked of
                    Just tab ->
                        ( model
                        , Ports.focusTab ( tab.id, tab.windowId )
                        )

                    Nothing ->
                        ( model
                        , Cmd.none
                        )

            else
                ( model
                , Cmd.none
                )


onUpdatedTree : String -> Model -> Model
onUpdatedTree toBeDecoded model =
    let
        enabledWindows =
            List.filter .enabled model.windows
                |> List.map .id

        checked =
            List.map .id (getChecked model)

        decoded =
            Decode.decodeString windowsDecoder toBeDecoded

        windowsInfos =
            case decoded of
                Ok value ->
                    removeEmptyWindows value

                Err _ ->
                    []

        removeEmptyWindows : List WindowInfos -> List WindowInfos
        removeEmptyWindows list =
            List.filter (\windowInfos -> List.length windowInfos.tabsInfos > 0) list

        updateTree m =
            { m | windows = List.indexedMap buildWindow windowsInfos }

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
    model |> fossilizeSelection |> updateTree |> restoreState


updateTabs : List TabId -> (Tab -> Tab) -> Model -> Model
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


enableWindows : List WindowId -> Model -> Model
enableWindows windowIds model =
    let
        updateWindow window =
            { window | enabled = True }
    in
    updateWindows windowIds updateWindow model


disableWindows : List WindowId -> Model -> Model
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


toggleWindow : WindowId -> Model -> Model
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


updateWindows : List WindowId -> (Window -> Window) -> Model -> Model
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


getEnabledWindows : Model -> List WindowId
getEnabledWindows model =
    model.windows
        |> List.filter .enabled
        |> List.map .id


invertSelection : Model -> Model
invertSelection model =
    let
        selected =
            getSelection model
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
                |> List.map domainOf

        tabIds : List Tab -> List (Maybe String) -> List TabId
        tabIds tabs domains =
            tabs
                |> List.filter (\tab -> List.member (domainOf tab.url) domains)
                |> List.map .id
    in
    updateTabs (tabIds (getSelection model) checkedDomains) (setChecked Checked) model


domainOf : String -> Maybe String
domainOf url =
    case
        Regex.findAtMost 1 (regexify "^.+?://([^/]+).*") url
            |> List.map .submatches
            |> List.concat
    of
        domain :: _ ->
            domain

        [] ->
            Nothing


selectDuplicates : Model -> Model
selectDuplicates model =
    let
        selected =
            getSelection model

        -- https://en.wikipedia.org/wiki/Cattle#Etymology is considered a duplicate of https://en.wikipedia.org/wiki/Cattle
        truncate url =
            url
                |> Regex.replace (regexify "#.*$") (\_ -> "")

        f : List String -> List Tab -> List TabId
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
    model
        |> uncheckAll
        |> updateTabs (f [] selected) (setChecked Checked)


regexify : String -> Regex
regexify s =
    Maybe.withDefault Regex.never <| Regex.fromString s


uncheckAll : Model -> Model
uncheckAll model =
    updateTabs (getTabIds model) (setChecked NotChecked) model


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
        [ Attributes.class "popup"
        ]
        [ Html.div
            [ Attributes.class "main" ]
            [ Html.div
                [ Attributes.class "left" ]
                [ viewBrowserHeader model
                , Html.div
                    [ Attributes.class "browser_container" ]
                    [ viewBrowser model ]
                ]
            , Html.div
                [ Attributes.class "right" ]
                [ viewToolbar model
                , viewSelection model
                ]
            ]
        , Html.div
            [ Attributes.class "footer" ]
            [ viewFooter model ]
        ]


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
                ++ displayMessage (messageToggleWindows (nEnabledWindows /= nWindows))
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
                     , Events.on "drop" (Decode.succeed (Execute Extract))
                     ]
                        ++ displayMessage (Hint "Open a new window")
                        ++ dropzonable
                    )
                    []
                ]

        windows =
            List.map viewWindow model.windows
    in
    Html.div
        [ Attributes.class "browser" ]
        (windows ++ [ createWindow ])


viewWindow : Window -> Html Msg
viewWindow window =
    let
        color =
            if window.enabled then
                window.color

            else
                "white"

        toggleWindowSideBar =
            Html.div
                ([ Attributes.class "toggleWindow"
                 , Attributes.style "background-color" color
                 , onEventThenStop "click" (ToggleWindow window)
                 ]
                    ++ displayMessage (messageToggleWindow window.enabled)
                )
                []

        createTab =
            Html.div
                ([ Attributes.class "createTab"
                 , onEventThenStop "click" (CreateTab window)
                 , Events.on "drop" (Decode.succeed (Drop window.id -1))
                 ]
                    ++ displayMessage (Hint "Open a new tab")
                    ++ dropzonable
                )
                []

        tabs =
            List.map viewTab window.tabs

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


viewTab : Tab -> Html Msg
viewTab tab =
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
         , onEventThenStop "click" (TabClick tab)
         , onEventThenStop "dblclick" (FocusTab tab)
         , Events.on "drop" (Decode.succeed (Drop tab.windowId tab.index))
         ]
            ++ displayMessage (Title tab.title)
            ++ dropzonable
        )
        []


dropzonable : List (Html.Attribute Msg)
dropzonable =
    [ Events.custom "dragover" (Decode.succeed { message = NoOp, preventDefault = True, stopPropagation = True })
    , Events.custom "dragenter" (Decode.succeed { message = NoOp, preventDefault = True, stopPropagation = True })
    ]


onEventThenStop : String -> msg -> Html.Attribute msg
onEventThenStop event message =
    Events.custom event (Decode.succeed { message = message, preventDefault = False, stopPropagation = True })


viewToolbar : Model -> Html Msg
viewToolbar model =
    Html.div
        [ Attributes.class "toolbar" ]
        [ Html.div
            [ Attributes.class "row1" ]
            [ viewInput model
            ]
        , Html.div
            [ Attributes.class "row2" ]
            [ viewEditSelection
            , viewGroupChecked model
            , viewSortSelection model
            ]
        , Html.div
            [ Attributes.class "row3" ]
            [ viewPreview model
            , viewExecuteAction
            ]
        , Html.div
            [ Attributes.class "toolbar_separator" ]
            []
        ]


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
                ++ displayMessage (Hint "Clear the search field")
            )
            []
        ]


viewEditSelection : Html Msg
viewEditSelection =
    let
        edits =
            [ Not, Similar, Duplicates, Uncheck ]
    in
    Html.div
        [ Attributes.class "editSelection" ]
        (List.map viewEditHelp edits)


viewEditHelp : Edit -> Html Msg
viewEditHelp edit =
    let
        str =
            case edit of
                Not ->
                    "not"

                Similar ->
                    "similar"

                Duplicates ->
                    "duplicates"

                Uncheck ->
                    "uncheck"

        message =
            case edit of
                Not ->
                    "Invert selection"

                Similar ->
                    "Add similar tabs to the selection"

                Duplicates ->
                    "Select duplicates"

                Uncheck ->
                    "Unselect all tabs"
    in
    Html.div
        ([ Attributes.classList
            [ ( "edit", True )
            , ( str, True )
            ]
         , Events.onClick (Apply edit)
         ]
            ++ displayMessage (Hint message)
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
            ++ displayMessage (messageGroupChecked model.groupChecked)
        )
        []


messageGroupChecked : Bool -> Kind
messageGroupChecked grouped =
    if grouped then
        Hint "Unstick selected tabs from the top of the list"

    else
        Hint "Stick selected tabs to the top of the list"


viewSortSelection : Model -> Html Msg
viewSortSelection model =
    let
        sorts =
            [ Visited, Websites, Windows ]
    in
    Html.div
        [ Attributes.class "sortSelection" ]
        (List.map
            (viewSortSelectionHelp model)
            sorts
        )


viewSortSelectionHelp : Model -> Sort -> Html Msg
viewSortSelectionHelp model sort =
    let
        str =
            case sort of
                Visited ->
                    "visited"

                Websites ->
                    "websites"

                Windows ->
                    "windows"

        message =
            case sort of
                Visited ->
                    "Sort tabs by last visited"

                Websites ->
                    "Group tabs by website"

                Windows ->
                    "Group tabs by window"
    in
    Html.div
        ([ Attributes.classList
            [ ( "sort", True )
            , ( str, True )
            , ( "selected", model.sortBy == sort )
            ]
         , Events.onClick (SortBy sort)
         ]
            ++ displayMessage (Hint message)
        )
        [ Html.text str ]


viewPreview : Model -> Html Msg
viewPreview model =
    let
        selected =
            getSelection model

        checked =
            getChecked model
    in
    if List.isEmpty checked then
        if List.isEmpty selected then
            Html.div
                [ Attributes.class "noPreview" ]
                [ Html.text "No window enabled" ]

        else
            Html.div
                [ Attributes.class "noPreview" ]
                [ Html.text "No tab selected" ]

    else
        Html.div
            [ Attributes.class "preview" ]
            [ Html.div
                ([ Attributes.class "preview_content"
                 , Attributes.draggable "true"
                 ]
                    ++ displayMessage (Hint "You can drag these tabs to a new position")
                )
                (checked |> List.map viewPreviewHelp)
            ]


viewPreviewHelp : Tab -> Html Msg
viewPreviewHelp tab =
    Html.div
        [ Attributes.class "preview_favicon"
        , Attributes.style
            "background-image"
            ("url(" ++ tab.faviconUrl ++ ")")
        ]
        []


viewExecuteAction : Html Msg
viewExecuteAction =
    let
        actions =
            [ Extract, Delete, Sort, Pin ]
    in
    Html.div
        [ Attributes.class "executeAction" ]
        (List.map viewExecuteActionHelp actions)


viewExecuteActionHelp : Action -> Html Msg
viewExecuteActionHelp action =
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
            ++ displayMessage (Hint message)
        )
        []


viewSelection : Model -> Html Msg
viewSelection model =
    Html.div
        [ Attributes.class "selection_container" ]
        [ Html.div
            [ Attributes.class "selection" ]
            (List.map (viewItem model) (getSelection model))
        ]


viewItem : Model -> Tab -> Html Msg
viewItem model tab =
    let
        color =
            model.windows
                |> List.filter (\window -> window.id == tab.windowId)
                |> List.map .color
                |> List.head

        windowColor =
            case color of
                Just c ->
                    c

                Nothing ->
                    "Blue"

        favicon =
            Html.div
                [ Attributes.class "item_favicon"
                , Attributes.style
                    "background-image"
                    ("url(" ++ tab.faviconUrl ++ ")")
                ]
                []

        title =
            Html.div
                [ Attributes.classList
                    [ ( "item_title", True )
                    , ( "checkedFromSearch", tab.checked == CheckedFromSearch )
                    ]
                , Events.onClick (FocusTab tab)
                ]
                [ Html.text tab.title ]

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
                        ]
                    ]
                    []
                ]
    in
    Html.div
        (Attributes.classList
            [ ( "item", True )
            , ( "checked", isChecked tab )
            ]
            :: displayMessage (Url tab.url)
        )
        [ favicon, title, checkbox ]


viewFooter : Model -> Html Msg
viewFooter model =
    let
        txt kind str =
            Html.div
                [ Attributes.class kind
                , Attributes.class "message"
                ]
                [ Html.text str ]
    in
    case model.footer of
        Hint str ->
            txt "hint" str

        Url str ->
            txt "url" str

        Title str ->
            txt "title" str


displayMessage : Kind -> List (Html.Attribute Msg)
displayMessage message =
    [ Events.onMouseEnter (SetMessage message), Events.onMouseLeave (SetMessage (Hint "")) ]


isChecked : Tab -> Bool
isChecked tab =
    tab.checked == Checked || tab.checked == CheckedFromSearch


getSelection : Model -> List Tab
getSelection model =
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


getIndexOfWindow : WindowId -> Model -> Maybe Int
getIndexOfWindow windowId model =
    model.windows
        |> List.filter (\window -> window.id == windowId)
        |> List.map .index
        |> List.head


getIndexOf : TabId -> List TabId -> Int
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
    getSelection model
        |> List.filter (\tab -> isChecked tab)


getCheckedFromSearch : Model -> List Tab
getCheckedFromSearch model =
    getChecked model
        |> List.filter (\tab -> tab.checked == CheckedFromSearch)


getTabIds : Model -> List TabId
getTabIds model =
    model.windows
        |> List.map .tabs
        |> List.concat
        |> List.map .id



-- JSON


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
