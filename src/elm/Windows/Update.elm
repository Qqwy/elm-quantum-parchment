module Windows.Update exposing (..)

import Json.Decode
import List.Extra
import Markdown
import Maybe.Extra
import Windows.Msgs exposing (Msg(..), WindowsMessage(..))
import Windows.Models exposing (..)
import Coord2D exposing (Coord2D)

update : Msg -> {a | windows_model : WindowsModel } -> ( {a | windows_model : WindowsModel }, Cmd Msg )
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

        NewWindow ->
            let
                windows_model =
                    model.windows_model

                cards =
                    windows_model.cards

                windows =
                    windows_model.windows

                new_card =
                    { title = "", content = "" }

                new_card_id =
                    List.length cards

                new_cards =
                    cards ++ [ new_card ]

                new_window =
                    { card_id = new_card_id, size = { x = 150, y = 150 }, position = { x = 100, y = 100 }, mode = Edit, is_minified = False }
                new_window_id = List.length windows

                new_windows =
                    windows ++ [new_window]
                new_window_orders = new_window_id :: windows_model.window_orders

                new_windows_model =
                    { windows_model | cards = new_cards, windows = new_windows, window_orders = new_window_orders }
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

        ToggleMinification window_id ->
            let
                new_windows =
                    model.windows
                        |> List.Extra.updateAt window_id (\window -> { window | is_minified = not window.is_minified })
            in
            ( { model | windows = new_windows }, Cmd.none )



moveToFront : Int -> List a -> List a
moveToFront index list =
    list

moveValueToFront : Int -> List Int -> List Int
moveValueToFront value list =
    let
        list_head =
            value

        list_tail =
            List.filter (\x -> x /= value) list
    in
    list_head :: list_tail

