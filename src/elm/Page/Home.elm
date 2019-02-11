module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Events
import Coord2D exposing (Coord2D)
import Html exposing (Html, div, h2, text, textarea, Attribute)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput, onMouseDown, onMouseUp)
import Json.Decode
import List.Extra
import Markdown
import Maybe.Extra
import Session exposing (Session)


onEscapeKeyDown : Msg -> Attribute Msg
onEscapeKeyDown msg =
    let
        isEsc code =
            if code == 27 then
                Json.Decode.succeed msg

            else
                Json.Decode.fail "not ESC"
    in
    Html.Events.on "keydown" (Json.Decode.andThen isEsc Html.Events.keyCode)



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , windows_model : WindowsModel
    }


type alias WindowsModel =
    { windows : List Window
    , window_orders : List WindowId
    , cards : List Card
    , current_window : Maybe CurrentWindow
    , mouse_position : Coord2D
    , mouse_delta : Coord2D
    }


type alias CurrentWindow =
    { click_position : Coord2D
    , window_id : WindowId
    , manipulation : WindowManipulationType
    }


type WindowManipulationType
    = MoveWindow
    | ResizeWindow


type alias Window =
    { card_id : CardId
    , position : Coord2D
    , size : Coord2D
    , mode : WindowMode
    }


type WindowMode
    = Read
    | Edit


type alias Coord2D =
    {- 2d position in pixels. -}
    { x : Int
    , y : Int
    }


type alias CardId =
    {- Index of card in WindowsModel.cards list -}
    Int


type alias WindowId =
    {- Index of card in WindowsModel.cards list -}
    Int


type alias Card =
    { title : String
    , content : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = ""
      , pageBody = ""
      , windows_model = initialWindows
      }
    , Cmd.none
    )


initialWindows : WindowsModel
initialWindows =
    { windows =
        [ { card_id = 0, size = { x = 100, y = 100 }, position = { x = 30, y = 40 }, mode = Edit }
        , { card_id = 1, size = { x = 150, y = 100 }, position = { x = 150, y = 30 }, mode = Read }
        ]
    , window_orders = [ 0, 1 ]
    , cards =
        [ { title = "Testcard 1", content = "Lorem Ipsum sit dolor amet" }
        , { title = "testcard 2", content = "The quick brown fox **jumps** over the [lazy dog](http://wmcode.nl)" }
        ]
    , current_window = Nothing
    , mouse_position = Coord2D.zero
    , mouse_delta = Coord2D.zero
    }



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , viewWindows model.windows_model
            ]
    }


viewWindows : WindowsModel -> Html Msg
viewWindows model =
    let
        windows_html_elements =
            model.windows
                |> List.map normalizeWindowSize
                |> List.indexedMap (\window_index window -> viewWindow window_index model.cards window)

        sorted_windows_html_elements =
            windows_html_elements
                |> List.indexedMap
                    (\window_index window_html_fn ->
                        case List.Extra.elemIndex window_index model.window_orders of
                            Nothing ->
                                Nothing

                            Just index ->
                                Just (window_html_fn index)
                    )
                |> Maybe.Extra.values

        -- model.window_orders
        --     |> List.map (\index -> List.Extra.getAt index windows_html_elements |> Maybe.map (\fn -> fn index))
        --     |> Maybe.Extra.values
    in
    div [ class "windows", onMouseUp (WindowsMessage StopWindowManipulation) ] sorted_windows_html_elements


viewWindow window_id cards window window_depth_index =
    let
        card =
            cards
                |> List.Extra.getAt window.card_id
                |> Maybe.withDefault { content = "Unknown Card.", title = "???" }

        content =
            card.content

        title =
            card.title

        size =
            window.size

        position =
            window.position

        attributes =
            [ style "width" (String.fromInt size.x ++ "px")
            , style "height" (String.fromInt size.y ++ "px")
            , style "left" (String.fromInt position.x ++ "px")
            , style "top" (String.fromInt position.y ++ "px")
            , style "z-index" (String.fromInt -window_depth_index)
            ]

        window_mode_class =
            case window.mode of
                Read ->
                    "read"

                Edit ->
                    "edit"
    in
    div ([ class "window", class window_mode_class ] ++ attributes)
        [ div [ class "window-body" ]
            [ div [ class "window-bar", onMouseDown (WindowsMessage <| StartWindowMove window_id) ] [ text title ]
            , div [ class "window-resize-handle", onMouseDown (WindowsMessage <| StartWindowResize window_id) ] [ text "" ]
            , textarea
                [ class "window-content-edit"
                , onInput (\str -> WindowsMessage <| ChangeCardContent window.card_id str)
                , onMouseDown (WindowsMessage <| MoveWindowToFront window_id)
                , onBlur (WindowsMessage <| ChangeWindowModeTo window_id Read)
                , onEscapeKeyDown (WindowsMessage <| ChangeWindowModeTo window_id Read)
                ]
                [ text content ]
            , div
                [ class "window-content-show"
                , onMouseDown (WindowsMessage <| MoveWindowToFront window_id)
                , onDoubleClick (WindowsMessage <| ChangeWindowModeTo window_id Edit)
                ]
                (Markdown.toHtml Nothing content)
            ]
        ]



-- UPDATE


type Msg
    = WindowsMessage WindowsMessage
    | MouseMove Int Int
    | Todo


type WindowsMessage
    = StartWindowMove WindowId
    | StartWindowResize WindowId
    | StopWindowManipulation
    | ChangeCardContent CardId String
    | MoveWindowToFront WindowId
    | ChangeWindowModeTo WindowId WindowMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )

        MouseMove x y ->
            let
                -- _ =
                --     Debug.log "MouseMove" ( x, y )
                windows_model =
                    model.windows_model

                new_windows_model =
                    let
                        windows =
                            windows_model.windows

                        mouse_position =
                            windows_model.mouse_position

                        new_mouse_postion =
                            Coord2D x y

                        mouse_delta =
                            Coord2D (x - mouse_position.x) (y - mouse_position.y)

                        new_windows =
                            case windows_model.current_window of
                                Nothing ->
                                    windows

                                Just current_window ->
                                    List.Extra.updateAt current_window.window_id (updateWindow mouse_delta current_window.manipulation) windows
                    in
                    { windows_model | windows = new_windows, mouse_position = Coord2D x y, mouse_delta = mouse_delta }
            in
            ( { model | windows_model = new_windows_model }, Cmd.none )

        WindowsMessage window_message ->
            let
                ( windows_model, commands ) =
                    updateWindowsMessage window_message model.windows_model
            in
            ( { model | windows_model = windows_model }, commands )


updateWindow mouse_delta manipulation window =
    case manipulation of
        MoveWindow ->
            let
                new_position =
                    { x = window.position.x + mouse_delta.x, y = window.position.y + mouse_delta.y }
            in
            { window | position = new_position }

        ResizeWindow ->
            let
                new_size =
                    Coord2D.add window.size mouse_delta
            in
            { window | size = new_size }


updateWindowsMessage : WindowsMessage -> WindowsModel -> ( WindowsModel, Cmd Msg )
updateWindowsMessage msg model =
    case msg of
        StartWindowMove window_id ->
            let
                new_window_orders =
                    moveValueToFront window_id model.window_orders

                current_window =
                    Just { window_id = window_id, click_position = model.mouse_position, manipulation = MoveWindow }
            in
            ( { model | window_orders = new_window_orders, current_window = current_window }, Cmd.none )

        StartWindowResize window_id ->
            let
                -- new_windows =
                --     moveToFront window_id model.windows
                new_window_orders =
                    moveValueToFront window_id model.window_orders

                current_window =
                    Just { window_id = window_id, click_position = model.mouse_position, manipulation = ResizeWindow }
            in
            ( { model | window_orders = new_window_orders, current_window = current_window }, Cmd.none )

        StopWindowManipulation ->
            let
                new_windows =
                    model.windows
                        |> List.map normalizeWindowSize
            in
            ( { model | current_window = Nothing, windows = new_windows }, Cmd.none )

        MoveWindowToFront window_id ->
            let
                new_window_orders =
                    moveValueToFront window_id model.window_orders
            in
            ( { model | window_orders = new_window_orders }, Cmd.none )

        ChangeCardContent card_id content ->
            let
                new_cards =
                    model.cards
                        |> List.Extra.updateAt card_id (changeCardContent content)
            in
            ( { model | cards = new_cards }, Cmd.none )

        ChangeWindowModeTo window_id new_mode ->
            let
                new_windows =
                    model.windows
                        |> List.Extra.updateAt window_id (\window -> { window | mode = new_mode })
            in
            ( { model | windows = new_windows }, Cmd.none )


changeCardContent content card =
    { card | content = content }


normalizeWindowSize window =
    let
        size =
            window.size
                |> Coord2D.maxXY 100 100

        position =
            window.position
                |> Coord2D.maxXY 0 0
                |> Coord2D.minXY 1600 800
    in
    { window | size = size, position = position }


moveToFront : Int -> List a -> List a
moveToFront index list =
    list



-- let
--     list_head =
--         List.Extra.getAt index list
--             |> Maybe.map List.singleton
--             |> Maybe.withDefault []
--     list_tail =
--         List.Extra.removeAt index list
-- in
-- list_head ++ list_tail


moveValueToFront : Int -> List Int -> List Int
moveValueToFront value list =
    let
        list_head =
            value

        list_tail =
            List.filter (\x -> x /= value) list
    in
    list_head :: list_tail



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- Sub.none
    Browser.Events.onMouseMove mousePosDecoder


mousePosDecoder =
    Json.Decode.map2 MouseMove
        (Json.Decode.field "clientX" Json.Decode.int)
        (Json.Decode.field "clientY" Json.Decode.int)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
