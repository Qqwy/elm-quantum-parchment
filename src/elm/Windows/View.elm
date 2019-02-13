module Windows.View exposing (..)

import Html exposing (Attribute, Html, div, h2, text, textarea, input)
import Html.Attributes exposing (class, id, style, value)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput, onMouseDown, onMouseUp)
import Windows.Msgs exposing (..)
import Windows.Models exposing (..)
import Json.Decode
import List.Extra
import Markdown
import Maybe.Extra

view : WindowsModel -> Html Msg
view model =
    let
        windows_html_elements =
            model.windows
                |> List.map Windows.Models.normalizeWindowSize
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

        minification_class =
            case window.is_minified of
                True ->
                    "minified"

                False ->
                    ""
    in
    div ([ class "window", class window_mode_class, class minification_class ] ++ attributes)
        [ div [ class "window-body" ]
            [ div
                [ class "window-bar"
                , onMouseDown (WindowsMessage <| StartWindowMove window_id)
                , onDoubleClick (WindowsMessage <| ToggleMinification window_id)
                ]
                [ input [class "window-bar-title", value title, onInput (\str -> WindowsMessage <| ChangeCardTitle window.card_id str)] [] ]
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
