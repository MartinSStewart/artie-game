module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Html.Events.Extra.Pointer
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , playerX : Int
    , playerXTarget : Int
    , mouseX : Int
    , mouseY : Int
    , time : Time.Posix
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | MouseDown { mouseX : Int, mouseY : Int }
    | MouseMove { mouseX : Int, mouseY : Int }
    | AnimationFrame Time.Posix


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
