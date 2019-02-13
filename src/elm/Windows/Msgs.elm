module Windows.Msgs exposing (..)
import Windows.Models exposing (..)

type Msg
    = WindowsMessage WindowsMessage
    | MouseMove Int Int
    | NewWindow
    | Todo


type WindowsMessage
    = StartWindowMove WindowId
    | StartWindowResize WindowId
    | StopWindowManipulation
    | ChangeCardContent CardId String
    | ChangeCardTitle CardId String
    | MoveWindowToFront WindowId
    | ChangeWindowModeTo WindowId WindowMode
    | ToggleMinification WindowId


