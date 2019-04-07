import Browser as B
import Basics exposing (modBy)
import String
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)

type Msg = Check Int Bool

type alias Todo = 
  { completed : Bool
  , title     : String
  , id        : Int
  }

type alias Model = 
  { todos : List Todo }

init : Model
init = 
  { 
    todos = 
      [ { completed = False , title = "Write Talk", id = 0 }
      , { completed = True  , title = "Propose Talk", id = 1 }
      ]
  }

todoView : Todo -> H.Html Msg
todoView t = 
  H.li (if t.completed then [HA.class "completed"] else []) 
  [ H.label []
    [ H.input 
      [ HA.type_ "checkbox"
      , HA.class "toggle"
      , HE.onCheck (Check t.id)
      , HA.checked t.completed
      ] []
    , H.text t.title
    ]
  ]

view : Model -> H.Html Msg
view model = 
  H.main_ [] 
    [ H.section [ HA.class "todo" ]
      [ H.ul [ HA.class "todo-list" ]
        (List.map todoView model.todos)
      ]
    ]

setTodoIf : Int -> Bool -> Todo -> Todo
setTodoIf i b t =
  if t.id == i 
    then { t | completed = b }
    else t

update : Msg -> Model -> Model
update msg model =
  case msg of
    (Check i b) -> { model | todos = List.map (setTodoIf i b) model.todos }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

main : Program () Model Msg
main = 
  B.document
    { init = \flags -> (init, Platform.Cmd.none)
    , subscriptions = subscriptions
    , view = \model -> { title = "Elm App"
                       , body = [view model]
                       }
    , update = \msg model -> (update msg model, Platform.Cmd.none)
    }