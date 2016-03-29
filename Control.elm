module Control where

import Model exposing (..)
import Dict exposing ( Dict )

type Action = 
  ChangeView View
    | CreateDebate Debate
    | CreateAssertion DebateId Assertion
    | SetAssertionDraft DebateId Assertion


update action model =
  case action of
    ChangeView view ->
      { model | view = view }
    CreateDebate debate ->
      insertDebate model debate
    CreateAssertion debateId assertion ->
      updateDebate model debateId (insertAssertion assertion)
    SetAssertionDraft debateId assertion ->
      updateDebate model debateId (\d -> {d | assertionDraft = assertion })

