module Windows.Decode exposing (..)

import Windows.Models exposing (..)
import Coord2D exposing (Coord2D)
import Json.Decode as JD

coord2D =
    JD.map2 Coord2D
        (JD.field "x" JD.int)
        (JD.field "y" JD.int)


card =
    JD.map2 Card
        (JD.field "title" JD.string)
        (JD.field "content" JD.string)


window =
    JD.map5 Window
        (JD.field "card_id" JD.int)
        (JD.field "position" coord2D)
        (JD.field "size" coord2D)
        (JD.succeed Read)
        (JD.field "is_minified" JD.bool)


windowOrders =
    JD.list JD.int


windowsModel =
    JD.map6 WindowsModel
        (JD.field "windows" (JD.list window))
        (JD.field "window_orders" windowOrders)
        (JD.field "cards" (JD.list card))
        (JD.succeed Nothing)
        (JD.succeed Coord2D.zero)
        (JD.succeed Coord2D.zero)

fromJSONString : String -> Result JD.Error WindowsModel
fromJSONString str = JD.decodeString windowsModel str
