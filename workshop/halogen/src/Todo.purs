module Todos where

import Prelude

import Data.Lens (Lens', lens, over)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (checked)

data Query a = Check Int Boolean a

type Todo = 
  { completed :: Boolean
  , title     :: String
  , id        :: Int
  }

type State = 
  { todos :: Array Todo }

_todos :: forall t r. Lens' { todos :: t | r } t
_todos = lens _.todos $ _ { todos = _ }

_completed :: forall t r. Lens' { completed :: t | r } t
_completed = lens _.completed $ _ { completed = _ }

class__ :: forall r i. String -> HP.IProp (class :: String | r) i
class__ = HP.class_ <<< ClassName

todos :: forall m. H.Component HH.HTML Query Unit Void m
todos =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { todos : 
    [ { completed : false, title : "Write Talk", id : 0 }
    , { completed : true, title : "Propose Talk", id : 1 }
    ]}

  renderTodo :: Todo -> H.ComponentHTML Query
  renderTodo todo = 
    HH.li (if todo.completed then [class__ "completed"] else [])
    [ HH.label []
      [ HH.input 
        (
          [ HP.type_ HP.InputCheckbox
          , class__ "toggle"
          , HP.checked todo.completed
          , (HE.onChecked (HE.input $ Check todo.id))
          ]
        )
      , HH.text todo.title
      ]
    ]
  
  render :: State -> H.ComponentHTML Query
  render state = 
    HH.body_ 
      [ HH.section [ class__ "todo" ]
        [ HH.ul [ class__ "todo-list" ]
          (map renderTodo state.todos)
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Check i b next -> do
      _ <- H.modify <<< over _todos <<< map $ \t -> if i == t.id then t { completed = b } else t
      pure next
