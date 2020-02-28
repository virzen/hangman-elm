module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, a, br, button, div, footer, p, pre, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder, field)
import Random
import Set exposing (Set)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- LIB


letters s =
    String.toList s |> Set.fromList


randomWord : Random.Generator Word
randomWord =
    Random.uniform "chicken" [ "test", "stuff", "rotfl", "lmao", "hangman", "meta" ]


type Key
    = Character Char
    | Control String


keyDecoder : Decoder Msg
keyDecoder =
    Decode.map (toKey >> KeyPressed) (field "key" Decode.string)


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


setToString s =
    Set.toList s |> String.fromList


intertwine separator string =
    string
        |> String.toList
        |> List.map String.fromChar
        |> String.join separator


hiddenWordForm word correctLetters =
    let
        hideIfNotGuessed =
            \c -> letterOrHidden (Set.member c correctLetters) c
    in
    word |> String.map hideIfNotGuessed |> intertwine " "



-- MODEL


type alias Word =
    String


type alias CorrectLetters =
    Set Char


type alias IncorrectLetters =
    Set Char


type Result
    = Won Word
    | Lost


type Model
    = Start
    | Playing Word CorrectLetters IncorrectLetters HangmanState
    | End Result


init : () -> ( Model, Cmd Msg )
init _ =
    ( Start, Cmd.none )


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
    ( model, Cmd.none )


initialPlayingModel word =
    Playing word Set.empty Set.empty (Transitionable Nothing) |> withCmd


generateWordCmd =
    Random.generate WordSelected randomWord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Start ->
            case msg of
                KeyPressed (Control "Enter") ->
                    ( model, generateWordCmd )

                Begin ->
                    ( model, generateWordCmd )

                WordSelected word ->
                    initialPlayingModel word

                _ ->
                    model |> withCmd

        Playing word incorrect correct hangmanState ->
            case msg of
                KeyPressed (Character c) ->
                    let
                        isInWord =
                            String.any ((==) c) word

                        wasGuessed =
                            Set.member c (Set.union correct incorrect)

                        isCorrect =
                            isInWord && not wasGuessed

                        lettersLeft =
                            Set.size (letters word) - Set.size correct
                    in
                    case ( isCorrect, hangmanState, lettersLeft ) of
                        ( True, _, 1 ) ->
                            End (Won word) |> withCmd

                        ( True, _, _ ) ->
                            Playing word incorrect (Set.insert c correct) hangmanState |> withCmd

                        ( False, Final _, _ ) ->
                            End Lost |> withCmd

                        ( False, Transitionable s, _ ) ->
                            Playing word (Set.insert c incorrect) correct (nextHangmanState s) |> withCmd

                _ ->
                    model |> withCmd

        End result ->
            case msg of
                KeyPressed (Control "Enter") ->
                    ( model, generateWordCmd )

                Restart ->
                    ( model, generateWordCmd )

                WordSelected word ->
                    initialPlayingModel word

                _ ->
                    model |> withCmd


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


type FinalHangmanState
    = RightLeg


nextHangmanState : TransitionableHangmanState -> HangmanState
nextHangmanState s =
    case s of
        Nothing ->
            Transitionable Pole

        Pole ->
            Transitionable Head

        Head ->
            Transitionable Torso

        Torso ->
            Transitionable LeftArm

        LeftArm ->
            Transitionable RightArm

        RightArm ->
            Transitionable LeftLeg

        LeftLeg ->
            Final RightLeg


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



-- VIEW


view : Model -> Html Msg
view model =
    viewContainer
        [ viewGame model
        ]


viewGame model =
    case model of
        Start ->
            div []
                [ text "Wanna play some hangman?"
                , br [] []
                , button [ onClick Begin, style "margin-top" "1em" ] [ text "Yes!" ]
                , footer [ style "font-size" "12px", style "padding-top" "1em" ]
                    [ text "Wiktor Czajkowski 2019"
                    , br [] []
                    , text "Ascii art by "
                    , a [ href "https://ascii.co.uk/art/hangman" ] [ text "ascii.co.uk" ]
                    ]
                ]

        Playing word incorrect correct hangmanState ->
            div []
                [ pre [ style "font-size" "18px" ] [ text (hangmanAscii hangmanState) ]
                , pre [ style "font-size" "18px" ] [ text (hiddenWordForm word correct) ]
                ]

        End result ->
            viewEnd result


viewContainer children =
    div [ style "height" "100vh", style "display" "flex", style "justify-content" "center", style "align-items" "center", style "text-align" "center" ] children


viewResult message =
    div []
        [ text message
        , br [] []
        , button [ onClick Restart, style "margin-top" "1em" ] [ text "Another one?" ]
        ]


viewEnd result =
    case result of
        Won word ->
            viewResult ("You gussed it, the word is \"" ++ word ++ "\"!")

        Lost ->
            viewResult "You lost!"
