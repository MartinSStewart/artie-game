module Frontend exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes
import Html.Events
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


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ --Browser.Events.onMouseDown (decodeMouseEvent MouseDown)
          Browser.Events.onMouseMove (decodeMouseEvent MouseMove)
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
      , inventory = Array.fromList [ Letter ]
      , narrationText = ""
      , hasOpenedChest = False
      , hasPickedUpKey = False
      , selectedInventoryItem = Nothing
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
                    toFloat (Time.posixToMillis newTime - Time.posixToMillis model.time)
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

        ClickedSomething newModel ->
            ( newModel, Cmd.none )

        ClickedInventoryItem itemIndex ->
            ( case model.selectedInventoryItem of
                Just previousItemIndex ->
                    if previousItemIndex == itemIndex then
                        { model | selectedInventoryItem = Nothing }

                    else
                        case ( Array.get itemIndex model.inventory, Array.get previousItemIndex model.inventory ) of
                            ( Just item, Just previousItem ) ->
                                combineTwoItems
                                    itemIndex
                                    item
                                    previousItemIndex
                                    previousItem
                                    { model | selectedInventoryItem = Nothing }

                            _ ->
                                { model | selectedInventoryItem = Nothing }

                Nothing ->
                    { model | selectedInventoryItem = Just itemIndex }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


drawImage : String -> Float -> Float -> Html msg
drawImage imageName x y =
    Html.img
        [ Html.Attributes.src imageName
        , Html.Attributes.style "top" (String.fromFloat y ++ "px")
        , Html.Attributes.style "left" (String.fromFloat x ++ "px")
        , Html.Attributes.style "position" "absolute"
        ]
        []


drawClickableImage : String -> Float -> Float -> FrontendModel -> Html FrontendMsg
drawClickableImage imageName x y newModel =
    Html.img
        [ Html.Attributes.src imageName
        , Html.Attributes.style "top" (String.fromFloat y ++ "px")
        , Html.Attributes.style "left" (String.fromFloat x ++ "px")
        , Html.Attributes.style "position" "absolute"
        , Html.Events.onClick (ClickedSomething newModel)
        , Html.Attributes.style "border" "0"
        , Html.Attributes.style "margin" "0"
        , Html.Attributes.style "padding" "0"
        , Html.Attributes.style "cursor" "pointer"
        ]
        []


itemSize : number
itemSize =
    50


drawGroup : Float -> Float -> List (Html msg) -> Html msg
drawGroup x y thingsToDraw =
    Html.div
        [ Html.Attributes.style "top" (String.fromFloat y ++ "px")
        , Html.Attributes.style "left" (String.fromFloat x ++ "px")
        , Html.Attributes.style "position" "absolute"
        ]
        thingsToDraw


drawInventory : FrontendModel -> Html FrontendMsg
drawInventory model =
    drawGroup
        50
        50
        [ drawRectangle
            "#aaaa99"
            0
            20
            (toFloat (Array.length model.inventory * itemSize) + 4)
            (itemSize + 4)
        , drawRectangle "#aaaa99" 0 0 90 20
        , drawText "black" 20 5 0 "Inventory"
        , drawGroup
            2
            22
            (List.indexedMap
                (\index item ->
                    Html.button
                        [ Html.Attributes.style "top" "0px"
                        , Html.Attributes.style "left" (String.fromFloat (toFloat index * itemSize) ++ "px")
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "margin" "0"
                        , Html.Attributes.style "padding" "0"
                        , Html.Attributes.style "border" "0"
                        , Html.Attributes.style "width" (String.fromInt itemSize ++ "px")
                        , Html.Attributes.style "height" (String.fromInt itemSize ++ "px")
                        , if model.selectedInventoryItem == Just index then
                            Html.Attributes.style "background-color" "pink"

                          else
                            Html.Attributes.style "" ""
                        , Html.Events.stopPropagationOn
                            "click"
                            (Json.Decode.succeed ( ClickedInventoryItem index, True ))
                        ]
                        [ drawImage (itemData item).imagePath 0 0
                        ]
                )
                (Array.toList model.inventory)
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
        , Html.Attributes.style "white-space" "pre"
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


drawNothing : Html msg
drawNothing =
    drawGroup 0 0 []


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
            , Html.Events.on "click" (decodeMouseEvent MouseDown)
            ]
            [ drawGroup 0 0 (roomView model)
            , drawInventory model
            , drawText "white" 20 20 500 model.narrationText
            ]
        , Html.text
            (String.fromInt (round model.mouseX)
                ++ ", "
                ++ String.fromInt (round model.mouseY)
            )
        ]
    }



-- Stuff you'll be working on is below this line


itemData : Item -> { imagePath : String, name : String }
itemData item =
    case item of
        Key ->
            { imagePath = "/key.png", name = "a key" }

        Letter ->
            { imagePath = "/letter.png", name = "a letter" }

        Rock ->
            { imagePath = "/rock.png", name = "a rock" }

        LetterWithRockInIt ->
            { imagePath = "/letterWithARockInIt.png", name = "a letter with a rock in it" }


combineTwoItems : Int -> Item -> Int -> Item -> FrontendModel -> FrontendModel
combineTwoItems itemIndexInInventory item previousItemIndex previousItemInInventory model =
    let
        hasSelectedItems : Item -> Item -> Bool
        hasSelectedItems item1 item2 =
            (item1 == item && item2 == previousItemInInventory)
                || (item2 == item && item1 == previousItemInInventory)

        inventoryWithBothItemsRemoved : Array Item
        inventoryWithBothItemsRemoved =
            Array.Extra.removeAt
                (min itemIndexInInventory previousItemIndex)
                (Array.Extra.removeAt (max itemIndexInInventory previousItemIndex) model.inventory)
    in
    if hasSelectedItems Rock Letter then
        { model
            | narrationText = "You put the rock in the letter envelope."
            , inventory = Array.push LetterWithRockInIt inventoryWithBothItemsRemoved
        }

    else
        { model
            | narrationText =
                "You tried combining "
                    ++ (itemData item).name
                    ++ " and "
                    ++ (itemData previousItemInInventory).name
                    ++ " but nothing happened."
        }


roomView : FrontendModel -> List (Html FrontendMsg)
roomView model =
    [ drawRectangle "gray" 0 420 2000 300
    , drawImage "/mario.png" (model.playerX - 30) 300
    , drawClickableImage
        "/chest.png"
        300
        380
        (if model.hasOpenedChest then
            { model | narrationText = "The chest has already been plundered." }

         else if Array.Extra.member Key model.inventory then
            { model
                | inventory = Array.push Rock model.inventory
                , narrationText = "You unlocked the chest and found a rock!"
                , hasOpenedChest = True
            }

         else
            { model
                | narrationText = "The chest is locked.\nYou need a key to open it!"
            }
        )
    , if model.hasPickedUpKey then
        drawNothing

      else
        drawClickableImage
            "/key.png"
            600
            350
            { model
                | hasPickedUpKey = True
                , inventory = Array.push Key model.inventory
                , narrationText = "You picked up a key. What could it be for?"
            }
    ]
