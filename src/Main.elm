module Main exposing (..)

import Set exposing (Set)
import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, button, div, text, pre, footer, p)
import Html.Events exposing (onClick)
import Random
import Json.Decode as Decode exposing (Decoder, field)



-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- LIB

letters s =
  String.toList s |> Set.fromList

randomWord : Random.Generator Word
randomWord =
  Random.uniform "chicken" ["test"]

type Key
  = Character Char
  | Control String

keyDecoder : Decoder Msg
keyDecoder =
  Decode.map (toKey >> KeyPressed) (field "key" Decode.string)

toKey : String -> Key
toKey string =
  case String.uncons string of
    Just (char, "") ->
      Character char

    _ ->
      Control string

-- MODEL

type alias Word = String
type alias TriesLeft = Int
type alias LettersLeft = Int

type alias CorrectLetters = Set Char

type alias IncorrectLetters = Set Char

type Result = Won | Lost

type Model
  = Start
  | Playing Word CorrectLetters IncorrectLetters HangmanState
  | End Result


init : () -> (Model, Cmd Msg)
init _ =
  (Start, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  onKeyPress keyDecoder

-- UPDATE


type Msg
  = KeyPressed Key
  | Begin
  | Restart
  | WordSelected Word

withCmd model =
  (model, Cmd.none)

initialPlayingModel word =
  Playing word Set.empty Set.empty (Transitionable Nothing) |> withCmd

generateWordCmd =
  Random.generate WordSelected randomWord


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Start ->
      case msg of
        KeyPressed (Control "Enter") ->
          (model, generateWordCmd)
        Begin ->
          (model, generateWordCmd)
        WordSelected word ->
          initialPlayingModel word
        _ ->
          model |> withCmd

    Playing word incorrect correct hangmanState ->
      case msg of
        KeyPressed (Character c) ->
          let
              isInWord = (String.any ((==) c) word)
              lettersLeft = (Set.size (letters word)) - (Set.size correct)
          in
          case (isInWord, hangmanState, lettersLeft) of
            (True, _, 1) ->
              End Won |> withCmd
            (True, _, _) ->
              Playing word incorrect (Set.insert c correct) hangmanState |> withCmd
            (False, Final _, _) ->
              End Lost |> withCmd
            (False, Transitionable s, _) ->
              Playing word (Set.insert c incorrect) correct (nextHangmanState s) |> withCmd
        _ -> model |> withCmd

    End result ->
      case msg of
        KeyPressed (Control "Enter") ->
          (model, generateWordCmd)
        Restart ->
          (model, generateWordCmd)
        WordSelected word ->
          initialPlayingModel word
        _ -> model |> withCmd


setToString s =
  Set.toList s |> String.fromList

type HangmanState
  = Transitionable TransitionableHangmanState
  | Final FinalHangmanState

type TransitionableHangmanState
  = Nothing
  | Pole
  | Head
  | Torso
  | LeftArm
  | RightArm
  | LeftLeg

type FinalHangmanState = RightLeg

nextHangmanState : TransitionableHangmanState -> HangmanState
nextHangmanState s =
  case s of
    Nothing -> Transitionable Pole
    Pole -> Transitionable Head
    Head -> Transitionable Torso
    Torso -> Transitionable LeftArm
    LeftArm -> Transitionable RightArm
    RightArm -> Transitionable LeftLeg
    LeftLeg -> Final RightLeg

hangmanAscii state =
  case state of
    Transitionable s ->
      case s of
        Nothing ->
          """






          """
        Pole ->
          """
            +---+
                |
                |
                |
                |
                |
          """
        Head ->
          """
            +---+
            |   |
            O   |
                |
                |
                |
          """
        Torso ->
          """
            +---+
            |   |
            O   |
            |   |
                |
                |
          """
        LeftArm ->
          """
            +---+
            |   |
            O   |
           /|   |
                |
                |
          """
        RightArm ->
          """
            +---+
            |   |
            O   |
           /|\\  |
                |
                |
          """
        LeftLeg ->
          """
            +---+
            |   |
            O   |
           /|\\  |
           /    |
                |
          """
    Final s ->
      case s of
        RightLeg ->
          """
            +---+
            |   |
            O   |
           /|\\  |
           / \\  |
                |
          """

letterOrHidden b c =
  case b of
    True ->
      c
    False ->
      '_'

intertwine separator string  =
  string
  |> String.toList
  |> List.map String.fromChar
  |> String.join separator

hiddenWordForm word correctLetters =
  let
    hideIfNotGuessed = (\c -> letterOrHidden (Set.member c correctLetters) c)
  in
  word |> String.map hideIfNotGuessed |> intertwine " "

-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Start ->
      button [ onClick Begin ] [ text "Start" ]
    Playing word incorrect correct hangmanState ->
      div [] [
        pre [] [ text (hangmanAscii hangmanState) ],
        pre [] [ text (hiddenWordForm word correct)],
        div [] [ text ("Letters left: " ++ (String.fromInt ((Set.size (letters word)) - (Set.size correct))))],
        footer [] [ text "Ascii art by https://ascii.co.uk/art/hangman" ]
      ]
    End result ->
      case result of
        Won ->
          div [] [
            text "You won!",
            button [ onClick Restart ] [ text "Again?" ]
          ]
        Lost ->
          div [] [
            text "You lost!",
            button [ onClick Restart ] [ text "Again?" ]
          ]


