import Browser as B
import Basics exposing (modBy)
import String
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)

-- This datatype describes the events that can happen in our UI.
type Msg = Click | Reset

-- This is the model that drives the UI
type alias Model = { clicks : Int }

-- We initialise the model to our starting value
init : Model
init  = { clicks = 0 }

text : Int -> String
text i = 
  let fizz = modBy 3 i == 0
      buzz = modBy 5 i == 0
  in
    if      i == 0       then "0"
    else if fizz && buzz then "FizzBuzz"
    else if fizz         then "Fizz"
    else if buzz         then "Buzz"
    else                      String.fromInt i

-- A function from model -> Html 
view : Model -> H.Html Msg
view model = 
  let msg = text model.clicks
  in H.main_ []
    -- This button raises a toggle event that triggers an update
    [H.button 
      [HA.class "big-button", HE.onClick Click] 
      [H.text msg] 
    ]

-- Our update takes in our Msg and returns a new model (and optional side effect)
update : Msg -> Model -> Model
update msg model =
  case msg of
    -- We pattern match our two cases for Msg and act accordingly
    Click -> { model | clicks = model.clicks + 1 }
    Reset -> init

-- Wire everything together into our program.

main =
  B.sandbox
    { init = init
    , view = view
    , update = update
    }
