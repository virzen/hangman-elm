module Main exposing (..)

import Set exposing (Set)
import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Json.Decode as Decode exposing (Decoder, field)



-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- LIB

tries = 10

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
  | Playing Word CorrectLetters IncorrectLetters
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Start ->
      case msg of
        KeyPressed (Control "Enter") ->
          (model, Random.generate WordSelected randomWord)
        Begin ->
          (model, Random.generate WordSelected randomWord)
        WordSelected word ->
          Playing word Set.empty Set.empty |> withCmd
        _ ->
          model |> withCmd

    Playing word incorrect correct ->
      case msg of
        KeyPressed (Character c) ->
          let
              isInWord = (String.any ((==) c) word)
              triesLeft = tries - (Set.size incorrect)
              lettersLeft = (Set.size (letters word)) - (Set.size correct)
          in
          case (isInWord, triesLeft, lettersLeft) of
            (True, _, 1) -> End Won |> withCmd
            (True, _, _) -> Playing word incorrect (Set.insert c correct) |> withCmd
            (False, 1, _) -> End Lost |> withCmd
            (False, _, _) -> Playing word (Set.insert c incorrect) correct |> withCmd
        _ -> model |> withCmd

    End result ->
      case msg of
        KeyPressed (Control "Enter") ->
          (model, Random.generate WordSelected randomWord)
        Restart ->
          (model, Random.generate WordSelected randomWord)
        WordSelected word ->
          Playing word Set.empty Set.empty |> withCmd
        _ -> model |> withCmd


setToString s =
  Set.toList s |> String.fromList



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Start ->
      button [ onClick Begin ] [ text "Start" ]
    Playing word incorrect correct ->
      div [] [
        div [] [ text ("Selected word: " ++ word) ],
        div [] [ text ("Incorrect letters: " ++ (setToString incorrect)) ],
        div [] [ text ("Correct letters: " ++ (setToString correct)) ],
        div [] [ text ("Tries left: " ++ (String.fromInt (tries - (Set.size incorrect))))],
        div [] [ text ("Letters left: " ++ (String.fromInt ((Set.size (letters word)) - (Set.size correct))))]
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


