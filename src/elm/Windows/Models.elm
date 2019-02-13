module Windows.Models exposing (Card, CardId, CurrentWindow, Window, WindowId, WindowManipulationType(..), WindowMode(..), WindowsModel, changeCardContent, changeCardTitle, encodeWindowsModel, initialWindows, normalizeWindowSize, toJSON)

import Coord2D exposing (Coord2D)
import Json.Encode


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
    , is_minified : Bool
    }


type WindowMode
    = Read
    | Edit


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


initialWindows : WindowsModel
initialWindows =
    { windows =
        [ { card_id = 0, size = { x = 100, y = 100 }, position = { x = 30, y = 40 }, mode = Read, is_minified = False }
        , { card_id = 1, size = { x = 150, y = 100 }, position = { x = 150, y = 30 }, mode = Read, is_minified = False }
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


changeCardContent content card =
    { card | content = content }


changeCardTitle title card =
    { card | title = title }


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


toJSON : WindowsModel -> String
toJSON model =
    Json.Encode.encode 4 (encodeWindowsModel model)


encodeWindowsModel model =
    Json.Encode.object
        [ ( "cards", Json.Encode.list encodeCard model.cards )
        , ( "windows", Json.Encode.list encodeWindow model.windows )
        , ( "window_orders", encodeWindowOrders model.window_orders )
        ]


encodeWindowOrders : List Int -> Json.Encode.Value
encodeWindowOrders window_orders =
    Json.Encode.list Json.Encode.int window_orders


encodeCard card =
    Json.Encode.object
        [ ( "title", Json.Encode.string card.title )
        , ( "content", Json.Encode.string card.content )
        ]


encodeWindow window =
    Json.Encode.object
        [ ( "card_id", Json.Encode.int window.card_id )
        , ( "position", encodeCoord2D window.position )
        , ( "size", encodeCoord2D window.size )
        , ( "is_minified", Json.Encode.bool window.is_minified )
        ]


encodeCoord2D coord2d =
    Json.Encode.object
        [ ( "x", Json.Encode.int coord2d.x )
        , ( "y", Json.Encode.int coord2d.y )
        ]
