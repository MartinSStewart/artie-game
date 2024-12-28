module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Html.Events.Extra.Pointer
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , playerX : Float
    , playerXTarget : Float
    , mouseX : Float
    , mouseY : Float
    , time : Time.Posix
    , inventory : List Item
    , narrationText : String
    , hasOpenedChest : Bool
    , selectedInventoryItem : Maybe Item
    }


type Item
    = Key
    | Letter
    | Rock


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | MouseDown { mouseX : Float, mouseY : Float }
    | MouseMove { mouseX : Float, mouseY : Float }
    | AnimationFrame Time.Posix
    | ClickedSomething FrontendModel
    | ClickedInventoryItem Item


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
