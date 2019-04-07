import Browser as B
import Basics exposing (modBy)
import String
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)

-- This datatype describes the events that can happen in our UI.
type Msg = Toggle | Reset

-- This is the model that drives the UI
type alias Model = { on : Bool }

-- We initialise the model to our starting value
init : Model
init  = { on = True }

-- A function from model -> Html
view : Model -> H.Html Msg
view model = 
  let msg = if model.on then "Hello" else "World"
  in H.main_ []
    -- This button raises a toggle event that triggers an update
    [ H.button [ HA.class "big-button", HE.onClick Toggle ]
      [H.text msg] ]

-- Our update takes in our Msg and returns a new model (and optional side effect)
update : Msg -> Model -> Model
update msg model =
  case msg of
    -- We pattern match our two cases for Msg and act accordingly
    Toggle -> { model | on = not model.on }
    Reset -> init

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- Wire everything together into our program.
main : Program () Model Msg
main =
  B.document
    { init = \flags -> (init, Cmd.none)
    , subscriptions = subscriptions
    , view = \model -> { title = "Elm App"
                       , body = [view model]
                       }
    , update = \msg model -> (update msg model, Cmd.none)
    }