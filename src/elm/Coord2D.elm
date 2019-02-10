module Coord2D exposing (..)


type alias Coord2D =
    {- 2d position in pixels. -}
    { x : Int
    , y : Int
    }

add a b =
    Coord2D (a.x + b.x) (a.y + b.y)

sub a b =
    Coord2D (a.x - b.x) (a.y - b.y)

zero =
    Coord2D 0 0

minXY minx miny a =
    Coord2D (min a.x minx) (min a.y miny)

maxXY maxx maxy a=
    Coord2D (max a.x maxx) (max a.y maxy)
