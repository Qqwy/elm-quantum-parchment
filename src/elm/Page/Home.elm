module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Events
import Coord2D exposing (Coord2D)
import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onMouseDown, onMouseUp)
import Json.Decode
import List.Extra
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , windows_model : WindowsModel
    }


type alias WindowsModel =
    { windows : List Window
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
    { cardId : CardId
    , position : Coord2D
    , size : Coord2D

    -- , width : Int
    -- , height : Int
    }


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
        [ { cardId = 0, size = { x = 100, y = 100 }, position = { x = 30, y = 40 } }
        , { cardId = 1, size = { x = 150, y = 100 }, position = { x = 150, y = 30 } }
        ]
    , cards =
        [ { title = "Testcard 1", content = "Lorem Ipsum sit dolor amet" }
        , { title = "testcard 2", content = "The quick brown fox jumps over the lazy dog" }
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
        windows_html =
            model.windows
                |> List.map normalizeWindowSize
                |> List.indexedMap (\window_index window -> viewWindow window_index model.cards window)
                |> List.reverse
    in
    div [ onMouseUp (WindowsMessage StopWindowManipulation) ] windows_html


viewWindow window_id cards window =
    let
        content =
            cards
                |> List.Extra.getAt window.cardId
                |> Maybe.map .content
                |> Maybe.withDefault "Unknown Card."

        size =
            window.size

        position =
            window.position

        attributes =
            [ style "width" (String.fromInt size.x ++ "px")
            , style "height" (String.fromInt size.y ++ "px")
            , style "left" (String.fromInt position.x ++ "px")
            , style "top" (String.fromInt position.y ++ "px")
            ]
    in
    div ([ class "window" ] ++ attributes)
        [ div [ class "window-body" ]
            [ div [ class "window-bar", onMouseDown (WindowsMessage <| StartWindowMove window_id) ] []
            , div [ class "window-resize-handle", onMouseDown (WindowsMessage <| StartWindowResize window_id) ] [ text "" ]
            , div [ class "window-content" ]
                [ text content
                ]
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
                                    List.Extra.updateAt 0 (updateWindow mouse_delta current_window.manipulation) windows
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
                new_windows =
                    moveToFront window_id model.windows

                current_window =
                    Just { window_id = window_id, click_position = model.mouse_position, manipulation = MoveWindow }
            in
            ( { model | windows = new_windows, current_window = current_window }, Cmd.none )

        StartWindowResize window_id ->
            let
                new_windows =
                    moveToFront window_id model.windows

                current_window =
                    Just { window_id = window_id, click_position = model.mouse_position, manipulation = ResizeWindow }
            in
            ( { model | windows = new_windows, current_window = current_window }, Cmd.none )

        StopWindowManipulation ->
            let
                new_windows =
                    model.windows
                        |> List.map normalizeWindowSize
            in
            ( { model | current_window = Nothing, windows = new_windows }, Cmd.none )


normalizeWindowSize window =
    let
        size =
            window.size
                |> Coord2D.maxXY 100 100

        position =
            window.position
                |> Coord2D.maxXY 0 0
                |> Coord2D.minXY 500 500
    in
    { window | size = size, position = position }


moveToFront : Int -> List a -> List a
moveToFront index list =
    let
        list_head =
            List.Extra.getAt index list
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        list_tail =
            List.Extra.removeAt index list
    in
    list_head ++ list_tail



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
