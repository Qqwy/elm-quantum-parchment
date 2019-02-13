module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Events
import Html exposing (Attribute, Html, div, h2, text, textarea)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput, onMouseDown, onMouseUp)
import Json.Decode
import List.Extra
import Markdown
import Maybe.Extra
import Session exposing (Session)
import Windows.Models exposing (..)
import Windows.Msgs exposing (..)
import Windows.Update
import Windows.View
import Ports
import Windows.Decode



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , windows_model : WindowsModel
    }

type alias Msg =
    Windows.Msgs.Msg


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = ""
      , pageBody = ""
      , windows_model = Windows.Models.initialWindows
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , div [ class "new-window-button", onClick NewWindow ] [ text "New Window" ]
            , div [ class "download-button", onClick DownloadWindowsModelAsFile ] [ text "Save" ]
            , div [ class "upload-button", onClick (RequestLoadWindowsModelFile) ] [ text "Load" ]
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
    Sub.batch [
    Browser.Events.onMouseMove mousePosDecoder
        , Ports.storageUpdate (\json ->
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
