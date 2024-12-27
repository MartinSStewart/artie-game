module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Lamdera
import Time
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseDown (decodeMouseEvent MouseDown)
        , Browser.Events.onMouseMove (decodeMouseEvent MouseMove)
        , Browser.Events.onAnimationFrame AnimationFrame
        ]


decodeMouseEvent : ({ mouseX : Float, mouseY : Float } -> a) -> Json.Decode.Decoder a
decodeMouseEvent msg =
    Json.Decode.map2
        (\x y -> msg { mouseX = x, mouseY = y })
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , playerX = 100
      , playerXTarget = 100
      , mouseX = 0
      , mouseY = 0
      , time = Time.millisToPosix 0
      , inventory = [ Key, Letter ]
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        MouseDown { mouseX, mouseY } ->
            ( { model
                | playerXTarget = mouseX
                , mouseX = mouseX
                , mouseY = mouseY
              }
            , Cmd.none
            )

        MouseMove { mouseX, mouseY } ->
            ( { model
                | mouseX = mouseX
                , mouseY = mouseY
              }
            , Cmd.none
            )

        AnimationFrame newTime ->
            let
                millisecondsElapsed =
                    Time.posixToMillis newTime - Time.posixToMillis model.time |> toFloat
            in
            ( { model
                | time = newTime
                , playerX =
                    if model.playerXTarget > model.playerX then
                        min
                            model.playerXTarget
                            ((millisecondsElapsed * 0.3) + model.playerX)

                    else
                        max
                            model.playerXTarget
                            ((millisecondsElapsed * -0.3) + model.playerX)
              }
            , Cmd.none
            )


type alias ClickableRegion =
    { x : Float
    , y : Float
    , image : String
    }


clickableRegions : List ClickableRegion
clickableRegions =
    [ { x = 200
      , y = 380
      , image = "/chest.png"
      }
    ]


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


drawClickableRegions : ClickableRegion -> Html msg
drawClickableRegions item =
    Html.img
        [ Html.Attributes.src item.image
        , Html.Attributes.style "top" (String.fromFloat item.y ++ "px")
        , Html.Attributes.style "left" (String.fromFloat item.x ++ "px")
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "cursor" "pointer"
        ]
        []


drawImage : String -> Float -> Float -> Html msg
drawImage imageName x y =
    Html.img
        [ Html.Attributes.src imageName
        , Html.Attributes.style "top" (String.fromFloat y ++ "px")
        , Html.Attributes.style "left" (String.fromFloat x ++ "px")
        , Html.Attributes.style "position" "absolute"
        ]
        []


itemSize =
    50


itemImage : Item -> String
itemImage item =
    case item of
        Key ->
            "/key.png"

        Letter ->
            "/letter.png"

        Rock ->
            "/rock.png"


drawGroup : Float -> Float -> List (Html msg) -> Html msg
drawGroup x y thingsToDraw =
    Html.div
        [ Html.Attributes.style "top" (String.fromFloat y ++ "px")
        , Html.Attributes.style "left" (String.fromFloat x ++ "px")
        , Html.Attributes.style "position" "absolute"
        ]
        thingsToDraw


drawInventory : List Item -> Html msg
drawInventory inventory =
    drawGroup
        50
        50
        [ drawRectangle "#aaaa99" 0 20 (toFloat (List.length inventory * itemSize)) itemSize
        , drawRectangle "#aaaa99" 0 0 90 20
        , drawText "black" 20 5 0 "Inventory"
        , drawGroup 0
            20
            (List.indexedMap
                (\index item ->
                    drawGroup
                        (toFloat index * itemSize)
                        0
                        [ drawRectangle "#ddddcc" 2 2 (itemSize - 4) (itemSize - 4)
                        , drawImage (itemImage item) 0 0
                        ]
                )
                inventory
            )
        ]


drawText : String -> Float -> Float -> Float -> String -> Html msg
drawText color fontSize x y text =
    Html.div
        [ Html.Attributes.style "top" (String.fromFloat y ++ "px")
        , Html.Attributes.style "left" (String.fromFloat x ++ "px")
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "font-size" (String.fromFloat fontSize ++ "px")
        , Html.Attributes.style "font-family" "sans-serif"
        , Html.Attributes.style "color" color
        ]
        [ Html.text text ]


drawRectangle : String -> Float -> Float -> Float -> Float -> Html msg
drawRectangle color x y width height =
    Html.div
        [ Html.Attributes.style "background-color" color
        , Html.Attributes.style "top" (String.fromFloat y ++ "px")
        , Html.Attributes.style "left" (String.fromFloat x ++ "px")
        , Html.Attributes.style "width" (String.fromFloat width ++ "px")
        , Html.Attributes.style "height" (String.fromFloat height ++ "px")
        , Html.Attributes.style "position" "absolute"
        ]
        []


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div
            [ Html.Attributes.style "overflow" "clip"
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "width" "100vw"
            , Html.Attributes.style "height" "100vh"
            ]
            [ drawRectangle "gray" 0 420 2000 300
            , drawImage "/mario.png" (model.playerX - 30) 300
            , drawInventory model.inventory
            , drawGroup 0 0 (List.map drawClickableRegions clickableRegions)
            ]
        ]
    }
