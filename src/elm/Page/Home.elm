module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Events
import Html exposing (Attribute, Html, div, h2, span, text, textarea)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput, onMouseDown, onMouseUp)
import Json.Decode
import List.Extra
import Markdown
import Maybe.Extra
import Ports
import Session exposing (Session)
import Windows.Decode
import Windows.Models exposing (..)
import Windows.Msgs exposing (..)
import Windows.Update
import Windows.View



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , windows_model : WindowsModel
    }


type alias Msg =
    Windows.Msgs.Msg


init : Session -> WindowsModel -> ( Model, Cmd Msg )
init session windows_model =
    ( { session = session
      , pageTitle = ""
      , pageBody = ""
      , windows_model = windows_model
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ div [ class "button-menu" ]
                [ div [class "header"] [text "QuantumParchment v0.1"]
                , div [ class "new-window-button button", onClick NewWindow ] [ span [ class "icon-note_add" ] [], text "Add Card" ]
                , div [ class "download-button button", onClick DownloadWindowsModelAsFile ] [ span [ class "icon-download" ] [], text "Save" ]
                , div [ class "upload-button button", onClick RequestLoadWindowsModelFile ] [ span [ class "icon-upload" ] [], text "Load" ]
                ]
            , Windows.View.view model.windows_model
            ]
    }



-- UPDATE


update msg model =
    Windows.Update.update msg model



-- let
--     list_head =
--         List.Extra.getAt index list
--             |> Maybe.map List.singleton
--             |> Maybe.withDefault []
--     list_tail =
--         List.Extra.removeAt index list
-- in
-- list_head ++ list_tail
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    Sub.batch
        [ Browser.Events.onMouseMove mousePosDecoder
        , Ports.storageUpdate
            (\json ->
                json
                    |> Json.Decode.decodeValue Json.Decode.string
                    |> Result.andThen Windows.Decode.fromJSONString
                    |> Result.withDefault model.windows_model
                    |> WindowsModelLoaded
            )
        ]


mousePosDecoder =
    Json.Decode.map2 MouseMove
        (Json.Decode.field "clientX" Json.Decode.int)
        (Json.Decode.field "clientY" Json.Decode.int)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
