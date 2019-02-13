module Windows.Encode exposing (..)
import Json.Encode as JE
import Windows.Models exposing (..)

toJSONString : WindowsModel -> String
toJSONString model =
    JE.encode 0 (windowsModel model)


windowsModel model =
    JE.object
        [ ( "windows", JE.list window model.windows )
        , ( "window_orders", windowOrders model.window_orders )
        , ( "cards", JE.list card model.cards )
        ]


windowOrders : List Int -> JE.Value
windowOrders window_orders =
    JE.list JE.int window_orders


card a =
    JE.object
        [ ( "title", JE.string a.title )
        , ( "content", JE.string a.content )
        ]


window a =
    JE.object
        [ ( "card_id", JE.int a.card_id )
        , ( "position", coord2D a.position )
        , ( "size", coord2D a.size )
        , ( "is_minified", JE.bool a.is_minified )
        ]


coord2D a =
    JE.object
        [ ( "x", JE.int a.x )
        , ( "y", JE.int a.y )
        ]

