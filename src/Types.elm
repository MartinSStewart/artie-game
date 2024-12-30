module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , playerX : Float
    , playerXTarget : Float
    , mouseX : Float
    , mouseY : Float
    , time : Time.Posix
    , inventory : Array Item
    , narrationText : String
    , hasOpenedChest : Bool
    , hasPickedUpKey : Bool
    , selectedInventoryItem : Maybe Int
    , hoverText : Maybe String
    , windowWidth : Float
    , windowHeight : Float
    }


{-| Items the player can have in their inventory
-}
type Item
    = Key
    | Letter
    | Rock
    | LetterWithRockInIt


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | MouseDown { mouseX : Float, mouseY : Float }
    | MouseMove { mouseX : Float, mouseY : Float }
    | AnimationFrame Time.Posix
    | ClickedSomething FrontendModel
    | ClickedInventoryItem Int
    | MouseEnteredClickableImage String
    | MouseExitedClickableImage String
    | GotWindowSize Float Float


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
