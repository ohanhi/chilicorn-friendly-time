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
  start
    { model = model
    , view = view
    , update = update
    }


port timeZoneOffset : Signal Int


port offsetter : Signal (Task x ())
port offsetter =
  timeZoneOffset
  |> Signal.map (\offset -> Signal.send address (TimeZoneChange offset))


ticks : Signal Time.Time
ticks =
  Time.every (100 * Time.millisecond)
  |> Signal.dropRepeats
  |> Signal.map (Debug.log "time")


port ticker : Signal (Task x ())
port ticker =
  ticks
  |> Signal.map (\time -> Signal.send address (TimeChange time))


secsInDay : Float
secsInDay =
  60.0 * 60 * 24


grainsInDay : Float
grainsInDay =
  256.0


timeToCft : Float -> Int -> CftTime
timeToCft time offsetMinutes =
  let
    date =
      Date.fromTime time
    secsToday =
      (Date.millisecond date // 1000) +
      (Date.second date) +
      ((60 + offsetMinutes + Date.minute date) * 60) +
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
  { time : Time.Time
  , timeResolved : Bool
  , timeZoneResolved : Bool
  , timeZoneMinutes : Int
  }


model : Model
model =
  { time = 0
  , timeResolved = False
  , timeZoneMinutes = 0
  , timeZoneResolved = False
  }


update : Action -> Model -> Model
update action model =
  case action of
    TimeChange newTime ->
      { model
        | time = newTime
        , timeResolved = True
        }
    TimeZoneChange offset ->
      { model
        | timeZoneMinutes = offset
        , timeZoneResolved = True
        }


view : Signal.Address Action -> Model -> RN.VTree
view address model =
  block "column"
    [ block "row"
      [ heading "Chilicorn Friendly Time"
      ]
    , block "column"
      [ quoteText "“It's"
      , blockNoFlex "row"
        [ chilicorn
        , showTime model
        ]
      , quoteText "grains right now.”"
      ]
    , block "column"
      [ footerText "by Futurice"
      ]
    ]


showTime : Model -> RN.VTree
showTime model =
  let
    resolved =
      model.timeResolved && model.timeZoneResolved
    cftTime =
      timeToCft model.time model.timeZoneMinutes
    grains =
      grainsToString fst 3 resolved cftTime
    centigrains =
      grainsToString snd 2 resolved cftTime
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
        , Style.color "#ccc"
        , Style.marginBottom 8
        ]
        Nothing centigrains
      ]


grainsToString : (CftTime -> Int) -> Int -> Bool -> CftTime -> String
grainsToString extractor length resolved time =
  case resolved of
    False ->
      String.repeat length "-"
    True ->
      time
      |> extractor
      |> toString
      |> String.padLeft length '0'


block : String -> List RN.VTree -> RN.VTree
block direction =
  RN.view
    [ Style.flex 1
    , Style.flexDirection direction
    , Style.alignItems "center"
    , Style.justifyContent "center"
    ]


blockNoFlex : String -> List RN.VTree -> RN.VTree
blockNoFlex direction =
  RN.view
    [ Style.flexDirection direction
    , Style.alignItems "center"
    , Style.justifyContent "center"
    ]


quoteText : String -> RN.VTree
quoteText =
  RN.text [ Style.fontSize 18, Style.color "#999" ] Nothing


footerText : String -> RN.VTree
footerText =
  RN.text [ Style.fontSize 18 ] Nothing


heading : String -> RN.VTree
heading =
  RN.text [ Style.fontSize 24 ] Nothing


chilicorn : RN.VTree
chilicorn =
  RN.image
    [ Style.height 64
    , Style.width 64
    ]
    "https://raw.githubusercontent.com/futurice/spiceprogram/master/assets/img/logo/chilicorn_no_text-128.png"


type Action
  = TimeChange Time.Time
  | TimeZoneChange Int


type AppAction a = Init | ConfigAction a


type alias Config model action =
  { model : model
  , view : Signal.Address action -> model -> RN.VTree
  , update : action -> model -> model
  }


actions : Signal.Mailbox (Maybe a)
actions =
  Signal.mailbox Nothing


address : Signal.Address a
address =
  Signal.forwardTo actions.address Just


start : Config model action -> Signal Json.Encode.Value
start config =
  let
    merged =
      Signal.map ConfigAction actions.signal

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
