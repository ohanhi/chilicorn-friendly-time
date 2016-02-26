module Main where

import Time
import Signal
import Json.Encode
import ReactNative.ReactNative as RN
import ReactNative.NativeApp as NativeApp
import ReactNative.Style as Style exposing ( defaultTransform )


-- "main"
port viewTree : Signal Json.Encode.Value
port viewTree =
  NativeApp.start { model = model, view = view, update = update, init = init }


type alias Model = Int


model : Model
model = 209


view : Signal.Address Action -> Model -> RN.VTree
view address cftime =
  block "column"
    [ block "row"
      [ heading "Chilicorn Friendly Time"
      ]
    , block "column"
      [ blockNoFlex "row"
        [ chilicorn
        , RN.text [Style.fontSize 64] Nothing (toString cftime)
        ]
      , footerText "grains"
      ]
    , block "column"
      [ footerText "https://chilicorn.org"
      ]
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


type Action = Increment | Decrement


update : Action -> Model -> Model
update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1


button : Signal.Address Action -> Action -> String -> String -> RN.VTree
button address action color content =
  RN.text
    [ Style.color "white"
    , Style.textAlign "center"
    , Style.backgroundColor color
    , Style.paddingTop 5
    , Style.paddingBottom 5
    , Style.width 30
    , Style.fontWeight "bold"
    , Style.shadowColor "#000"
    , Style.shadowOpacity 0.25
    , Style.shadowOffset 1 1
    , Style.shadowRadius 5
    , Style.transform { defaultTransform | rotate = Just "10deg" }
    ]
    (Just <| RN.onPress address action)
    content


-- for the first vtree
port init : Signal ()
