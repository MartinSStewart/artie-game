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


decodeMouseEvent : ({ mouseX : Int, mouseY : Int } -> a) -> Json.Decode.Decoder a
decodeMouseEvent msg =
    Json.Decode.map2
        (\x y -> msg { mouseX = round x, mouseY = round y })
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , playerX = 0
      , playerXTarget = 0
      , mouseX = 0
      , mouseY = 0
      , time = Time.millisToPosix 0
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
            ( { model | playerXTarget = mouseX, mouseX = mouseX, mouseY = mouseY }
            , Cmd.none
            )

        MouseMove { mouseX, mouseY } ->
            ( { model | mouseX = mouseX, mouseY = mouseY }, Cmd.none )

        AnimationFrame newTime ->
            let
                millisecondsElapsed =
                    Time.posixToMillis newTime - Time.posixToMillis model.time
            in
            ( { model
                | time = newTime
                , playerX =
                    if model.playerXTarget > model.playerX then
                        min
                            model.playerXTarget
                            (round (toFloat millisecondsElapsed * 0.01) + model.playerX)

                    else
                        max
                            model.playerXTarget
                            (round (toFloat millisecondsElapsed * -0.01) + model.playerX)
              }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


drawImage : String -> Int -> Int -> Html msg
drawImage imageName x y =
    Html.img
        [ Html.Attributes.src imageName
        , Html.Attributes.style "top" (String.fromInt y ++ "px")
        , Html.Attributes.style "left" (String.fromInt x ++ "px")
        , Html.Attributes.style "position" "absolute"
        ]
        []


drawRectangle : String -> Int -> Int -> Int -> Int -> Html msg
drawRectangle color x y width height =
    Html.div
        [ Html.Attributes.style "background-color" color
        , Html.Attributes.style "top" (String.fromInt y ++ "px")
        , Html.Attributes.style "left" (String.fromInt x ++ "px")
        , Html.Attributes.style "width" (String.fromInt width ++ "px")
        , Html.Attributes.style "height" (String.fromInt height ++ "px")
        , Html.Attributes.style "position" "absolute"
        ]
        []


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ drawRectangle "red" 100 200 300 400
        , drawImage "/mario.png" model.playerX 100
        ]
    }
