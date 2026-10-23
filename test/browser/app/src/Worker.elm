module Worker exposing (main)

{-| Node entry point for the test harness, the way a Lamdera backend runs the
package. See Harness.elm.
-}

import Harness


main : Program () Harness.Model Harness.Msg
main =
    Platform.worker
        { init = \() -> Harness.init
        , update = Harness.update
        , subscriptions = Harness.subscriptions
        }
