module Main exposing (main)

import Array
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import Ports
import Regex
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
    , search : String -- search bar request
    , sortBy : Sort
    , footer : Kind -- hint, title or url
    , mouseOverFavicons : Bool -- mouse over favicons in the toolbar
    , visited : List Int -- last visited tabs
    }


type alias Window =
    { id : Int
    , incognito : Bool
    , focused : Bool
    , color : String
    , index : Int
    , tabs : List Tab
    }


type alias Tab =
    { id : Int
    , url : String
    , faviconUrl : String
    , title : String
    , pinned : Bool
    , windowId : Int
    , selected : Bool
    , checked : Bool
    , spotlight : Bool
    , mouseOverItem : Bool -- mouse over list item representation of a tab
    , mouseOverFavicon : Bool -- mouse over list item favicon
    , index : Int
    }


type Kind
    = Url String
    | Hint String
    | Title String


type Sort
    = Visited
    | Checked
    | Websites
    | Windows


type Edit
    = All
    | Empty
    | Not
    | Similar
    | Uncheck


type Action
    = Extract
    | Delete
    | Sort
    | Pin
    | RemoveDuplicates


type Msg
    = NoOp
    | MouseEnterItem Int String -- an item is a tab in the selection (right): tabId, Url
    | MouseLeaveItem Int
    | MouseOverFavicon Int Bool
    | CheckboxClick Int
    | TabClick Int
    | SortBy Sort
    | Search String
    | ClearSearch
    | CreateWindow
    | CreateTab Int -- windowId
    | SelectTabs Int -- windowId
    | Execute Action
    | Apply Edit
    | MouseOverFavicons Bool -- mouse over favicons in preview (toolbar)
    | FaviconClick Int -- favicon of an item
    | RemoveClick Int
    | UpdatedTree String -- String is a JSON encoded string
    | FaviconsClick
    | FocusWindow Int
    | FocusTab Int
    | SetMessage Kind
    | Drop Int Int -- windowId, index
    | EnterPressed


type alias WindowInfos =
    { id : Int
    , incognito : Bool
    , focused : Bool
    , tabsInfos : List TabInfos
    }


type alias TabInfos =
    { id : Int
    , url : String
    , faviconUrl : Maybe String
    , title : String
    , pinned : Bool
    , windowId : Int
    }


type alias Flags =
    { windows : String
    , visited : List Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Model [] "" Visited (Hint "Welcome!") False flags.visited

        model1 =
            onUpdatedTree flags.windows model

        model2 =
            updateTabsField (getTabIds model1) (setSelected True) model1
    in
    ( model2, Cmd.none )


buildWindow : Int -> WindowInfos -> Window
buildWindow index { id, incognito, focused, tabsInfos } =
    let
        colors =
            Array.fromList [ "SlateBlue", "MediumAquaMarine", "SkyBlue", "DeepPink", "SlateGray", "Coral", "Olive", "Peru", "Navy", "PaleVioletRed" ]

        color =
            Array.get (modBy (Array.length colors) index) colors

        picked =
            case color of
                Just c ->
                    c

                Nothing ->
                    "Gray"
    in
    { id = id
    , incognito = incognito
    , focused = focused
    , color = picked
    , index = index
    , tabs = List.indexedMap buildTab tabsInfos
    }


buildTab : Int -> TabInfos -> Tab
buildTab index { id, url, faviconUrl, title, pinned, windowId } =
    let
        default =
            "img/default_favicon.png"

        fav =
            case faviconUrl of
                Just value ->
                    if String.left 15 value == "chrome://theme/" then
                        -- We can't load these urls
                        default

                    else
                        value

                Nothing ->
                    default
    in
    { id = id
    , url = url
    , faviconUrl = fav
    , title = title
    , pinned = pinned
    , windowId = windowId
    , selected = False
    , checked = False
    , spotlight = False
    , mouseOverItem = False
    , mouseOverFavicon = False
    , index = index
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseEnterItem tabId url ->
            let
                model1 =
                    { model | footer = Url url }
            in
            ( updateTabsField [ tabId ] (setSpotlight True) model1
            , Cmd.none
            )

        MouseLeaveItem tabId ->
            let
                model1 =
                    { model | footer = Hint "" }
            in
            ( updateTabsField [ tabId ] (setSpotlight False) model1
            , Cmd.none
            )

        MouseOverFavicon tabId bool ->
            ( updateTabsField [ tabId ] (setMouseOverFavicon bool) model
            , Cmd.none
            )

        CheckboxClick tabId ->
            let
                model1 =
                    model
                        |> updateTabsField [ tabId ] toggleChecked
                        |> updateTabsField [ tabId ] (setSpotlight False)
            in
            ( model1
            , Cmd.none
            )

        TabClick tabId ->
            ( updateTabsField [ tabId ] toggleSelected model
            , Cmd.none
            )

        SortBy sort ->
            ( { model | sortBy = sort }
            , Cmd.none
            )

        Search string ->
            let
                regexify str =
                    case Regex.fromString str of
                        Just r ->
                            r

                        Nothing ->
                            Regex.never

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
                        String.contains (String.toLower word) (String.toLower (tab.url ++ " " ++ tab.title))

                match tab =
                    -- expl: SUPERMAN, BATMAN, POWER RANGERS = SUPERMAN or BATMAN or (POWER and RANGERS)
                    List.map (List.map (contains tab)) words
                        |> List.map (List.foldl (&&) True)
                        |> List.foldl (||) False

                tabIds =
                    getSelected model
                        |> List.filter match
                        |> List.map .id

                allTabIds =
                    getSelected model
                        |> List.map .id

                model1 =
                    model
                        |> updateTabsField allTabIds (setChecked False)
                        |> updateTabsField tabIds (setChecked True)
            in
            ( { model1 | search = string, sortBy = Checked }
            , Cmd.none
            )

        ClearSearch ->
            ( clearSearch model
            , Cmd.none
            )

        CreateWindow ->
            ( model
            , Ports.createWindow ()
            )

        CreateTab windowId ->
            ( model
            , Ports.createTab windowId
            )

        SelectTabs windowId ->
            let
                model1 =
                    model
                        |> updateTabsField (getTabIds model) (setSelected False)
                        |> updateTabsField (getTabIdsOfWindow model windowId) (setSelected True)
                        |> clearSearch
            in
            ( model1
            , Cmd.none
            )

        Execute action ->
            case action of
                Extract ->
                    ( model
                    , Ports.extractTabs (getPertinentSelection model)
                    )

                Delete ->
                    ( model
                    , Ports.removeTabs (getPertinentSelection model)
                    )

                Sort ->
                    ( model
                    , Ports.sortTabs (getPertinentSelection model)
                    )

                Pin ->
                    ( model
                    , Ports.pinTabs (getPertinentSelection model)
                    )

                RemoveDuplicates ->
                    ( model
                    , Ports.removeDuplicates (getPertinentSelection model)
                    )

        Apply edit ->
            case edit of
                All ->
                    ( updateTabsField (getTabIds model) (setSelected True) model
                        |> clearSearch
                    , Cmd.none
                    )

                Empty ->
                    ( updateTabsField (getTabIds model) (setSelected False) model
                        |> clearSearch
                    , Cmd.none
                    )

                Not ->
                    ( invertSelection model
                        |> clearSearch
                    , Cmd.none
                    )

                Similar ->
                    ( selectSimilar model
                        |> clearSearch
                    , Cmd.none
                    )

                Uncheck ->
                    ( updateTabsField (getTabIds model) (setChecked False) model
                        |> clearSearch
                    , Cmd.none
                    )

        MouseOverFavicons bool ->
            let
                footer =
                    if bool then
                        Hint "Click to make these tabs the new selection"

                    else
                        Hint ""
            in
            ( { model | mouseOverFavicons = bool, footer = footer }
            , Cmd.none
            )

        FaviconClick tabId ->
            let
                model1 =
                    model
                        |> updateTabsField (getTabIds model) (setSelected False)
                        |> updateTabsField [ tabId ] (setSelected True)
                        |> updateTabsField [ tabId ] (setMouseOverFavicon False)
                        |> updateTabsField [ tabId ] (setSpotlight False)
                        |> clearSearch
            in
            ( model1
            , Cmd.none
            )

        RemoveClick tabId ->
            let
                model1 =
                    model
                        |> updateTabsField [ tabId ] (setSelected False)
                        |> updateTabsField [ tabId ] (setSpotlight False)
            in
            ( { model1 | footer = Hint "" }
            , Cmd.none
            )

        UpdatedTree toBeDecoded ->
            ( onUpdatedTree toBeDecoded model
            , Cmd.none
            )

        FaviconsClick ->
            let
                toBeSelected =
                    getChecked model
                        |> List.map .id

                model1 =
                    model
                        |> updateTabsField (getTabIds model) (setSelected False)
                        |> updateTabsField toBeSelected (setSelected True)
                        |> clearSearch
            in
            ( { model1 | mouseOverFavicons = False }
              -- the mouseleave event doesn't fire when we remove the favicons
            , Cmd.none
            )

        FocusWindow windowId ->
            ( model
            , Ports.focusWindow windowId
            )

        FocusTab tabId ->
            ( model
            , Ports.focusTab tabId
            )

        SetMessage message ->
            ( { model | footer = message }
            , Cmd.none
            )

        Drop windowId index ->
            ( model
            , Ports.moveTabs ( getPertinentSelection model, windowId, index )
            )

        EnterPressed ->
            let
                sel =
                    getPertinentSelection model

                first =
                    List.head sel
            in
            if List.length sel == 1 then
                -- if only one tab is selected
                case first of
                    Just tabId ->
                        ( model, Ports.focusTab tabId )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )


onUpdatedTree : String -> Model -> Model
onUpdatedTree toBeDecoded model =
    let
        decoded =
            Decode.decodeString windowsDecoder toBeDecoded

        windowsInfos =
            case decoded of
                Ok value ->
                    value

                Err _ ->
                    []

        windows =
            windowsInfos
                |> List.indexedMap buildWindow
    in
    { model | windows = windows }


updateTabsField : List Int -> (Tab -> Tab) -> Model -> Model
updateTabsField tabIds f model =
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


clearSearch : Model -> Model
clearSearch model =
    { model | search = "" }


toggleSelected : Tab -> Tab
toggleSelected tab =
    { tab
        | selected = not tab.selected
        , checked = False
    }


toggleChecked : Tab -> Tab
toggleChecked tab =
    { tab | checked = not tab.checked }


setMouseOverFavicon : Bool -> Tab -> Tab
setMouseOverFavicon bool tab =
    { tab | mouseOverFavicon = bool }


setSpotlight : Bool -> Tab -> Tab
setSpotlight bool tab =
    { tab | spotlight = bool }


setSelected : Bool -> Tab -> Tab
setSelected bool tab =
    { tab
        | selected = bool
        , checked = False
    }


setChecked : Bool -> Tab -> Tab
setChecked bool tab =
    { tab | checked = bool }


selectTabsInWindow : Window -> Window
selectTabsInWindow window =
    let
        newTabs =
            window.tabs
                |> List.map (\tab -> { tab | selected = True })
    in
    { window | tabs = newTabs }


invertSelection : Model -> Model
invertSelection model =
    let
        checked =
            getChecked model
                |> List.map .id

        selected =
            getSelected model
                |> List.map .id
    in
    if List.isEmpty checked then
        -- if no subselection then we invert the selection
        updateTabsField (getTabIds model) toggleSelected model

    else
        -- otherwise we invert the subselection
        updateTabsField selected toggleChecked model


selectSimilar : Model -> Model
selectSimilar model =
    let
        regex =
            case Regex.fromString "^.+?://([^/]+).*" of
                Just r ->
                    r

                Nothing ->
                    Regex.never

        domainOf tab =
            Regex.findAtMost 1 regex tab.url
                |> List.map .submatches
                |> List.concat

        checkedDomains =
            getChecked model
                |> List.map domainOf

        selectedDomains =
            getSelected model
                |> List.map domainOf

        tabIds : List Tab -> List (List (Maybe String)) -> List Int
        tabIds tabs domains =
            tabs
                |> List.filter (\tab -> List.member (domainOf tab) domains)
                |> List.map .id
    in
    if List.isEmpty checkedDomains then
        updateTabsField (tabIds (getTabs model) selectedDomains) (setSelected True) model

    else
        updateTabsField (tabIds (getSelected model) checkedDomains) (setChecked True) model


subscriptions : Model -> Sub Msg
subscriptions model =
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
                [ viewBrowser model ]
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


viewBrowser : Model -> Html Msg
viewBrowser model =
    let
        createWindow =
            Html.div
                [ Attributes.class "createWindow_container" ]
                [ Html.div
                    ([ Attributes.class "createWindow"
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
        selectTabs =
            Html.div
                ([ Attributes.class "selectTabs"
                 , Attributes.style "background-color" window.color
                 , onEventThenStop "click" (SelectTabs window.id)
                 ]
                    ++ displayMessage (Hint "Select tabs from this window")
                )
                []

        footer =
            Html.div
                [ Attributes.class "window_footer" ]
                [ selectTabs ]

        createTab =
            Html.div
                ([ Attributes.class "createTab"
                 , onEventThenStop "click" (CreateTab window.id)
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
            , Events.onDoubleClick (FocusWindow window.id)
            ]
            [ container, footer ]


viewTab : Tab -> Html Msg
viewTab tab =
    Html.div
        ([ Attributes.classList
            [ ( "tab", True )
            , ( "selected", tab.selected )
            , ( "pinned", tab.pinned )
            , ( "checked", tab.checked )
            , ( "spotlight", tab.spotlight )
            ]
         , Attributes.title tab.title
         , Attributes.style
            "background-image"
            ("url(" ++ tab.faviconUrl ++ ")")
         , onEventThenStop "click" (TabClick tab.id)
         , onEventThenStop "dblclick" (FocusTab tab.id)
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
            , Events.onInput Search
            , Attributes.autofocus True
            , Attributes.placeholder "Search"
            ]
            []
        , Html.div
            ([ Attributes.classList
                [ ( "clearSearch", model.search /= "" ) ]
             , Events.onClick ClearSearch
             ]
                ++ displayMessage (Hint "Clear")
            )
            []
        ]


viewEditSelection : Html Msg
viewEditSelection =
    let
        edits =
            [ All, Empty, Not, Similar, Uncheck ]
    in
    Html.div
        [ Attributes.class "editSelection" ]
        (List.map viewEditSelectionHelp edits)


viewEditSelectionHelp : Edit -> Html Msg
viewEditSelectionHelp edit =
    let
        str =
            case edit of
                All ->
                    "all"

                Empty ->
                    "empty"

                Not ->
                    "not"

                Similar ->
                    "similar"

                Uncheck ->
                    "uncheck"

        message =
            case edit of
                All ->
                    "Select all tabs"

                Empty ->
                    "Select none"

                Not ->
                    "Invert selection"

                Similar ->
                    "Add similar tabs to the selection"

                Uncheck ->
                    "Uncheck all tabs"
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


viewSortSelection : Model -> Html Msg
viewSortSelection model =
    let
        sorts =
            [ Visited, Checked, Websites, Windows ]
    in
    Html.div
        [ Attributes.class "sortSelection" ]
        (List.map (viewSortSelectionHelp model) sorts)


viewSortSelectionHelp : Model -> Sort -> Html Msg
viewSortSelectionHelp model sort =
    let
        str =
            case sort of
                Visited ->
                    "visited"

                Checked ->
                    "checked"

                Websites ->
                    "websites"

                Windows ->
                    "windows"

        message =
            case sort of
                Visited ->
                    "Sort the selection by last visited"

                Checked ->
                    "Put the checked tabs at the top of the selection"

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
        checked =
            getChecked model

        selected =
            getSelected model

        tab =
            Html.div
                [ Attributes.classList
                    [ ( "preview_tab", True )
                    , ( "selected", True )
                    ]
                , Attributes.style
                    "background-image"
                    "url(img/default_favicon.png)"
                ]
                []

        content =
            if List.isEmpty selected then
                [ Html.text "" ]

            else if List.isEmpty checked then
                [ Html.div
                    ([ Attributes.class "preview_xtabs"
                     , Attributes.draggable "true"
                     ]
                        ++ displayMessage (Hint "You can drag these tabs to a new position")
                    )
                    [ Html.text (String.fromInt (List.length selected) ++ " x")
                    , tab
                    ]
                ]

            else
                checked
                    |> List.map viewPreviewHelp

        noBraces =
            not model.mouseOverFavicons

        events =
            if not <| List.isEmpty checked then
                [ Events.onMouseEnter (MouseOverFavicons True)
                , Events.onMouseLeave (MouseOverFavicons False)
                , Events.onClick FaviconsClick
                ]

            else
                []
    in
    Html.div
        [ Attributes.class "preview" ]
        [ Html.div
            [ Attributes.classList
                [ ( "preview_brace", True )
                , ( "opening", True )
                , ( "hidden", noBraces )
                ]
            ]
            []
        , Html.div
            ([ Attributes.classList
                [ ( "preview_content", True )
                , ( "text", List.isEmpty checked )
                , ( "none", List.isEmpty selected )
                ]
             ]
                ++ events
            )
            content
        , Html.div
            [ Attributes.classList
                [ ( "preview_brace", True )
                , ( "closing", True )
                , ( "hidden", noBraces )
                ]
            ]
            []
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
            [ Extract, Delete, Sort, Pin, RemoveDuplicates ]
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

                RemoveDuplicates ->
                    "remove_duplicates"

        message =
            case action of
                Extract ->
                    "Extract tabs to a new window"

                Delete ->
                    "Remove tabs"

                Sort ->
                    "Sort tabs by url and title"

                Pin ->
                    "Pin/Unpin tabs"

                RemoveDuplicates ->
                    "Remove duplicates"
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

        opening =
            Html.div
                [ Attributes.classList
                    [ ( "item_brace", True )
                    , ( "opening", True )
                    , ( "hidden", not tab.mouseOverFavicon )
                    ]
                ]
                []

        closing =
            Html.div
                [ Attributes.classList
                    [ ( "item_brace", True )
                    , ( "closing", True )
                    , ( "hidden", not tab.mouseOverFavicon )
                    ]
                ]
                []

        favicon =
            Html.div
                [ Attributes.class "item_favicon"
                , Attributes.style
                    "background-image"
                    ("url(" ++ tab.faviconUrl ++ ")")
                , Events.onClick (FaviconClick tab.id)
                , Events.onMouseEnter (MouseOverFavicon tab.id True)
                , Events.onMouseLeave (MouseOverFavicon tab.id False)
                ]
                []

        title =
            Html.div
                [ Attributes.class "item_title"
                , Events.onClick (FocusTab tab.id)
                ]
                [ Html.text tab.title ]

        checkbox =
            Html.div
                [ Attributes.class "item_checkbox"
                , Attributes.style "background-color" windowColor
                , Events.onClick (CheckboxClick tab.id)
                ]
                [ Html.div
                    [ Attributes.classList
                        [ ( "item_check", True )
                        , ( "checked", tab.checked )
                        ]
                    ]
                    []
                ]

        remove =
            Html.div
                [ Attributes.class "item_remove"
                , Events.onClick (RemoveClick tab.id)
                ]
                []
    in
    Html.div
        [ Attributes.classList
            [ ( "item", True )
            , ( "checked", tab.checked )
            ]
        , Events.onMouseEnter (MouseEnterItem tab.id tab.url)
        , Events.onMouseLeave (MouseLeaveItem tab.id)
        ]
        [ opening, favicon, closing, title, checkbox, remove ]


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


getSelection : Model -> List Tab
getSelection model =
    let
        tabs =
            getSelected model
    in
    case model.sortBy of
        Visited ->
            List.sortBy (\tab -> getIndexOf model tab.id) tabs

        Checked ->
            List.sortBy
                (\tab ->
                    if tab.checked then
                        1

                    else
                        2
                )
                tabs

        Websites ->
            List.sortBy .url tabs

        Windows ->
            List.sortBy
                (\tab ->
                    case getIndexOfWindow model tab.windowId of
                        Just i ->
                            ( i, tab.index )

                        Nothing ->
                            ( List.length model.windows, tab.index )
                )
                tabs


getIndexOfWindow : Model -> Int -> Maybe Int
getIndexOfWindow model windowId =
    model.windows
        |> List.filter (\window -> window.id == windowId)
        |> List.map .index
        |> List.head


getIndexOf : Model -> Int -> Int
getIndexOf model tabId =
    -- finds the index of a tab in the visited list
    let
        helper : List Int -> Int -> Int -> Int
        helper lst elem offset =
            case lst of
                [] ->
                    offset + 1

                -- tabs which aren't present in the visited list are appended to the end
                x :: xs ->
                    if x == elem then
                        offset

                    else
                        helper xs elem (offset + 1)
    in
    helper model.visited tabId 0


getSelected : Model -> List Tab
getSelected model =
    model.windows
        |> List.map .tabs
        |> List.concat
        |> List.filter .selected


getChecked : Model -> List Tab
getChecked model =
    model.windows
        |> List.map .tabs
        |> List.concat
        |> List.filter .checked


getPertinentSelection : Model -> List Int
getPertinentSelection model =
    -- return the checked tabIds, or if no checked tabs, the whole selection
    let
        checked =
            getChecked model
    in
    if List.isEmpty checked then
        List.map .id (getSelected model)

    else
        List.map .id checked


getTabs : Model -> List Tab
getTabs model =
    model.windows
        |> List.map .tabs
        |> List.concat


getTabIds : Model -> List Int
getTabIds model =
    model.windows
        |> List.map .tabs
        |> List.concat
        |> List.map .id


getTabIdsOfWindow : Model -> Int -> List Int
getTabIdsOfWindow model windowId =
    model.windows
        |> List.filter (\window -> window.id == windowId)
        |> List.map .tabs
        |> List.concat
        |> List.map .id



-- JSON


windowsDecoder : Decode.Decoder (List WindowInfos)
windowsDecoder =
    Decode.list windowDecoder


windowDecoder : Decode.Decoder WindowInfos
windowDecoder =
    Decode.map4 WindowInfos
        (Decode.field "id" Decode.int)
        (Decode.field "incognito" Decode.bool)
        (Decode.field "focused" Decode.bool)
        (Decode.field "tabs" (Decode.list tabDecoder))


tabDecoder : Decode.Decoder TabInfos
tabDecoder =
    Decode.map6 TabInfos
        (Decode.field "id" Decode.int)
        (Decode.field "url" Decode.string)
        (Decode.maybe (Decode.field "favIconUrl" Decode.string))
        (Decode.field "title" Decode.string)
        (Decode.field "pinned" Decode.bool)
        (Decode.field "windowId" Decode.int)
