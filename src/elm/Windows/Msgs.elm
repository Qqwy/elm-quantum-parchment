module Windows.Msgs exposing (..)
import Windows.Models exposing (..)
import File exposing (File)
import Json.Decode

type Msg
    = WindowsMessage WindowsMessage
    | MouseMove Int Int
    | NewWindow
    | DownloadWindowsModelAsFile
    | RequestLoadWindowsModelFile
    | LoadWindowsModelFile File
    | WindowsModelLoaded WindowsModel
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


