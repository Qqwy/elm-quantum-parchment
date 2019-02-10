module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onMouseDown, onMouseUp)
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
    }


type alias CurrentWindow =
    { click_position : Position
    , window_id: WindowId
    }


type alias Window =
    { cardId : CardId
    , position : Position
    , width : Int
    , height : Int
    }


type alias Position =
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
        [ { cardId = 0, width = 100, height = 100, position = { x = 30, y = 40 } }
        , { cardId = 1, width = 150, height = 100, position = { x = 150, y = 30 } }
        ]
    , cards =
        [ { title = "Testcard 1", content = "Lorem Ipsum sit dolor amet" }
        , { title = "testcard 2", content = "The quick brown fox jumps over the lazy dog" }
        ]
    , current_window = Nothing
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
            List.indexedMap (\window_index window -> viewWindow window_index model.cards window) model.windows
    in
    div [onMouseUp (WindowsMessage StopWindowManipulation)] windows_html


viewWindow window_id cards window =
    let
        content =
            cards
                |> List.Extra.getAt window.cardId
                |> Maybe.map .content
                |> Maybe.withDefault "Unknown Card."

        attributes =
            [ style "width" (String.fromInt window.width ++ "px")
            , style "height" (String.fromInt window.height ++ "px")
            , style "left" (String.fromInt window.position.x ++ "px")
            , style "top" (String.fromInt window.position.y ++ "px")
            ]
    in
    div ([ class "window" ] ++ attributes)
        [ div [ class "window-body" ]
            [ div [ class "window-bar", onMouseDown (WindowsMessage <| StartWindowMove window_id) ] []
            , div [ class "window-resize-handle", onMouseDown (WindowsMessage <| StartWindowResize window_id)] [ text "" ]
            , div [ class "window-content" ]
                [ text content
                ]
            ]
        ]



-- UPDATE


type Msg
    = WindowsMessage WindowsMessage
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

        WindowsMessage window_message ->
            let (windows_model, commands) =
                  updateWindowsMessage window_message model.windows_model
              in
                  ({model | windows_model = windows_model}, commands)

updateWindowsMessage : WindowsMessage -> WindowsModel -> (WindowsModel, Cmd Msg)
updateWindowsMessage msg model =
    case msg of
        StartWindowMove window_id ->
            ( {model | current_window = Just {window_id = window_id, click_position = {x = 0, y = 0}}}, Cmd.none )

        StartWindowResize window_id ->
            ( model, Cmd.none )

        StopWindowManipulation ->
            ( {model | current_window = Nothing}, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
