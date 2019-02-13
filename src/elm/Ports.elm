port module Ports exposing (..)

import Json.Decode
import Json.Encode

port persistToStorage : Json.Encode.Value -> Cmd msg


port storageUpdate : (Json.Decode.Value -> msg) -> Sub msg
