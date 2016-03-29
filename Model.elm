module Model where

import Dict exposing ( Dict )

type AssertionState = Undisputed | Disputed | Controversial

type alias AssertionId = Int
type alias AssertionData a = 
  { a
  | id : AssertionId
  , state : AssertionState
  }

type alias StatementData = AssertionData { text : String }

type alias ConnectionData = AssertionData { assertionId : AssertionId , premiseId : AssertionId }

type Assertion = Statement StatementData | Dispute ConnectionData | Support ConnectionData

type alias DebateId = Int
type alias Debate =
  { id : DebateId
  , assertions : Dict AssertionId Assertion
  , nextAssertionId : AssertionId
  , topic : String
  , assertionDraft : Assertion
  }
  
type View = DebateMenuView Debate | DebateView DebateId

type alias Model =
  { debates : Dict DebateId Debate
  , nextDebateId : DebateId
  , view : View
  }

defaultModel : Model
defaultModel = { debates = Dict.empty, nextDebateId = 0, view = DebateMenuView defaultDebate}

defaultStatement : Assertion
defaultStatement = Statement { id = 0, state = Undisputed, text = "" }

defaultDispute : Assertion
defaultDispute = Dispute { id = 0, state = Undisputed, assertionId = 0, premiseId = 0 }

defaultSupport : Assertion
defaultSupport = Support { id = 0, state = Undisputed, assertionId = 0, premiseId = 0 }

defaultDebate : Debate
defaultDebate = 
  { id = 0
  , assertions = Dict.empty
  , topic = ""
  , nextAssertionId = 0
  , assertionDraft = defaultStatement
  }

createAssertionOfType : String -> Maybe Assertion
createAssertionOfType typeString = 
  if typeString == "statement" then Just defaultStatement
  else if typeString == "dispute" then Just defaultDispute
  else if typeString == "support" then Just defaultSupport
  else Nothing

insertAssertion : Assertion -> Debate -> Debate
insertAssertion assertion debate = 
    let id = debate.nextAssertionId
        a = case assertion of
          Statement data -> Statement { data | id = id }
          Dispute data -> Dispute { data | id = id }
          Support data -> Support { data | id = id }
    in 
        { debate
        | assertions = Dict.insert id a debate.assertions
        , nextAssertionId = id + 1 
        }

insertDebate model debate =
    let id = model.nextDebateId
    in 
        { model
        | debates = Dict.insert id { debate | id = id } model.debates
        , nextDebateId = id + 1 
        }

updateDebate model debateId update =
    { model
    | debates = Dict.update debateId (Maybe.map update) model.debates
    }

connectionTitle : Debate -> ConnectionData -> String -> String
connectionTitle debate connection separator =
  let idToTitle id = 
        if id == connection.id 
          then ("#" ++ (toString id)) 
          else Dict.get id debate.assertions 
        |> Maybe.map (assertionTitle debate) 
        |> Maybe.withDefault "unknown"
      a = idToTitle connection.assertionId
      p = idToTitle connection.premiseId
  in "(" ++ p ++ ") " ++ separator ++ " (" ++ a ++ ")"

assertionTitle : Debate -> Assertion -> String
assertionTitle debate assertion =
  case assertion of
    Statement s -> "\"" ++ s.text ++ "\""
    Dispute d -> connectionTitle debate d "-/->"
    Support s -> connectionTitle debate s "--->"

assertionIdOf : Assertion -> AssertionId
assertionIdOf assertion =
  case assertion of
    Statement s -> s.id
    Dispute d -> d.id
    Support s -> s.id

