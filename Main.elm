module Main where

import Task exposing (Task)
import Time
import Date
import String
import Signal
import Json.Encode
import ReactNative.ReactNative as RN
import ReactNative.Style as Style exposing ( defaultTransform )


-- "main"
port viewTree : Signal Json.Encode.Value
port viewTree =
  start { model = model, view = view, update = update, init = init }


ticks : Signal CftTime
ticks =
  Time.every Time.second
  |> Signal.map timeToCft
  |> Signal.dropRepeats
  |> Signal.map (Debug.log "time")


port ticker : Signal (Task x ())
port ticker =
  ticks
  |> Signal.map (\time -> Signal.send address (TimeChange time))


secsInDay =
  60.0 * 60 * 24

grainsInDay =
  256.0

timeToCft : Float -> (Int, Int)
timeToCft time =
  let
    date =
      Date.fromTime time
    secsToday =
      (Date.millisecond date // 1000) +
      (Date.second date) +
      (Date.minute date * 60) +
      (Date.hour date * 60 * 60)
    grainsToday =
      (toFloat secsToday) / secsInDay * grainsInDay
  in
    grainsToday
    |> toCftTime


toCftTime : Float -> CftTime
toCftTime grainsFloat =
  let
    grains =
      floor grainsFloat
    centigrains =
      (floor (grainsFloat * 100)) - (grains * 100)
  in
    (grains, centigrains)


type alias CftTime =
  (Int, Int)


type alias Model =
  { time: CftTime
  , resolved: Bool
  }


model : Model
model =
  { time = (0,0)
  , resolved = False
  }


view : Signal.Address Action -> Model -> RN.VTree
view address model =
  block "column"
    [ block "row"
      [ heading "Chilicorn Friendly Time"
      ]
    , block "column"
      [ blockNoFlex "row"
        [ chilicorn
        , showTime model
        ]
      , footerText "grains"
      ]
    , block "column"
      [ footerText "https://chilicorn.org"
      ]
    ]


showTime model =
  let
    grains =
      if model.resolved
        then
          fst model.time
          |> toString
          |> String.padLeft 3 '0'
        else
          "---"
    centigrains =
      (if model.resolved
        then
          snd model.time
          |> toString
          |> String.padLeft 2 '0'
        else
          "--")
      |> (++) "."
  in
    RN.view
      [ Style.flex 1
      , Style.flexDirection "row"
      , Style.alignItems "flex-end"
      , Style.justifyContent "center"
      ]
      [ RN.text [Style.fontSize 64] Nothing grains
      , RN.text
        [ Style.fontSize 32
        , Style.color "#999"
        , Style.marginBottom 8
        ]
        Nothing centigrains
      ]


block direction =
  RN.view
    [ Style.flex 1
    , Style.flexDirection direction
    , Style.alignItems "center"
    , Style.justifyContent "center"
    ]


blockNoFlex direction =
  RN.view
    [ Style.flexDirection direction
    , Style.alignItems "center"
    , Style.justifyContent "center"
    ]


footerText =
  RN.text [ Style.fontSize 18 ] Nothing


heading =
  RN.text [ Style.fontSize 24 ] Nothing


chilicorn =
  RN.image
    [ Style.height 64
    , Style.width 64
    ]
    "https://raw.githubusercontent.com/futurice/spiceprogram/master/assets/img/logo/chilicorn_no_text-128.png"


type Action = TimeChange CftTime


update : Action -> Model -> Model
update action model =
  let newTime =
    case action of
      TimeChange time -> time
  in  { model
        | time = newTime
        , resolved = True
        }


-- for the first vtree
port init : Signal ()


type AppAction a = Init | ConfigAction a


type alias Config model action =
  { model : model
  , view : Signal.Address action -> model -> RN.VTree
  , update : action -> model -> model
  , init : Signal ()
  }


actions =
  Signal.mailbox Nothing


address =
  Signal.forwardTo actions.address Just


start : Config model action -> Signal Json.Encode.Value
start config =
  let
    merged =
      Signal.mergeMany
        [ Signal.map ConfigAction actions.signal
        , Signal.map (always Init) config.init
        ]

    update action model =
      case action of
        ConfigAction action' ->
          normalUpdate action' model

        Init ->
          model

    normalUpdate maybeAction model =
      case maybeAction of
        Just action ->
            config.update action model

        Nothing ->
            Debug.crash "This should never happen."

    model =
      Signal.foldp update config.model merged
  in
    model
    |> Signal.map (config.view address)
    |> Signal.map RN.encode
