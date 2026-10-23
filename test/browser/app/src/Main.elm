module Main exposing (main)

{-| Browser entry point for the test harness. See Harness.elm.
-}

import Browser
import Harness
import Html


main : Program () Harness.Model Harness.Msg
main =
    Browser.element
        { init = \() -> Harness.init
        , update = Harness.update
        , subscriptions = Harness.subscriptions
        , view = \_ -> Html.text "lamdera/websocket test harness"
        }
