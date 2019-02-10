module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class, style)
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


type alias Card =
    { title : String
    , content : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Home"
      , pageBody = "This is the home page"
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
            List.map (viewWindow model.cards) model.windows
    in
    div [] windows_html


viewWindow cards window =
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
            [ div [ class "window-bar" ] []
            , div [ class "window-resize-handle" ] [ text "" ]
            , div [ class "window-content" ]
                [ text content
                ]
            ]
        ]



-- UPDATE


type Msg
    = Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
