module View where

import Model exposing (..)
import Control exposing (..)

import Dict exposing ( Dict )
import String exposing ( toInt )
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json


viewAssertion : Signal.Address Action -> Debate -> Assertion -> Html.Html
viewAssertion address debate assertion =
  let title = assertionTitle debate assertion
  in 
      case assertion of
        Statement data -> div [] [ text ("Statement: " ++ title) ]
        Dispute data -> div [] [ text title ]
        Support data -> div [] [ text title ]


targetValue = Json.at ["target", "value"] Json.string

onSelect : Signal.Address a -> (String -> a) -> Attribute
onSelect address handler = on "change" targetValue
  (\value -> Signal.message address (handler value))

assertionTypeSelectHandler debateId value = 
  case (createAssertionOfType value) of
    Just assertion -> SetAssertionDraft debateId assertion
    Nothing -> Debug.crash ("Unrecognized assertion type: " ++ value)

viewAssertionSelect address debate handler =
  let asOption a = 
      option 
        [ value <| toString <| assertionIdOf a ] 
        [ text <| assertionTitle debate a ]
      assertions = Dict.values debate.assertions
  in select [ onSelect address handler ] <| List.map asOption assertions
      

viewAssertionInput address debate =
  let handleAssertionSelect da aid =
        let d = case (toInt aid) of 
          Ok id -> da id
          Err _ -> Debug.crash ("assertion id not an integer: " ++ aid)
        in (SetAssertionDraft debate.id d)
      content = case debate.assertionDraft of
    Statement s -> 
      [ input
        [ placeholder "My awesome argument"
        , value s.text
        , on "input" targetValue 
          (\v -> Signal.message address (SetAssertionDraft debate.id (Statement {s | text = v})))
        ] []
      ]
    Dispute d -> [ viewAssertionSelect address debate 
                   (handleAssertionSelect (\id -> Dispute { d | premiseId = id }))
                 , text "disputes"
                 , viewAssertionSelect address debate
                   (handleAssertionSelect (\id -> Dispute { d | assertionId = id }))
                 ]
    Support s -> [ viewAssertionSelect address debate
                   (handleAssertionSelect (\id -> Support { s | premiseId = id }))
                 , text "supports"
                 , viewAssertionSelect address debate
                   (handleAssertionSelect (\id -> Support { s | assertionId = id }))
                 ]
  in div [] content

viewAssertionForm address debate =
  div []
    [ select [ onSelect address (assertionTypeSelectHandler debate.id) ]
      [ option [ value "statement" ] [ text "Statement" ]
      , option [ value "dispute", disabled (Dict.isEmpty debate.assertions) ] 
        [ text "Dispute" ]
      , option [ value "support", disabled (Dict.isEmpty debate.assertions) ] 
        [ text "Support" ]
      ]
    , viewAssertionInput address debate
    ] 

viewDebate : Signal.Address Action -> Debate -> Html.Html
viewDebate address debate =
  div []
    [ text debate.topic
    , viewAssertionForm address debate
    , button 
        [ onClick address (CreateAssertion debate.id debate.assertionDraft) ]
        [ text "Create Assertion" ]
    , div [] (List.map (viewAssertion address debate) <| Dict.values debate.assertions)
    ]

view address model =
  case model.view of
    DebateMenuView draft ->
      div []
        [ input
          [ placeholder "Debate topic"
          , value draft.topic
          , on "input" targetValue 
            (\v -> Signal.message address (ChangeView (DebateMenuView {draft | topic = v})))
          ] []
        , button 
          [ disabled <| String.isEmpty draft.topic
          , onClick address (CreateDebate draft) 
          ]
          [ text "Create Debate" ]
        , div [] (List.map (\d -> button [onClick address (ChangeView (DebateView d.id))] [text d.topic]) <| Dict.values model.debates)
        ]
    DebateView debateId ->
      case Dict.get debateId model.debates of
        Nothing -> text "Invalid debate id"
        Just debate ->
          div []
            [ button [ onClick address (ChangeView (DebateMenuView defaultDebate)) ] [ text "Back to Menu" ]
            , viewDebate address debate
            ]

