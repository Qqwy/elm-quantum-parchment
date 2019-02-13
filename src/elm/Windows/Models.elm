module Windows.Models exposing (Card, CardId, CurrentWindow, Window, WindowId, WindowManipulationType(..), WindowMode(..), WindowsModel, changeCardContent, changeCardTitle, initialWindows, normalizeWindowSize)

import Coord2D exposing (Coord2D)
import Json.Decode
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


