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
    s |> noSpaces |> String.toList |> Set.fromList


noSpaces s =
    String.replace " " "" s


randomWord : Random.Generator Word
randomWord =
    Random.uniform "komu w drogę temu cukier do szafy" [ "baba z wozu nie ma co jeść", "kto pod kim dołki kopie ten nosił wilka", "gość w dom razy kilka ", "w matematyce nie ma dróg królewskich" ]


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


type Game
    = Start
    | Playing Word CorrectLetters IncorrectLetters HangmanState
    | End Result


type Lang
    = PL
    | ENG


type Model
    = Model Lang Game


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model PL Start, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress keyDecoder



-- UPDATE


type Msg
    = KeyPressed Key
    | Begin
    | Restart
    | WordSelected Word
    | LangChanged Lang


withCmd model =
    ( model, Cmd.none )


initialPlayingModel word =
    Playing word Set.empty Set.empty (Transitionable Nothing) |> withCmd


generateWordCmd =
    Random.generate WordSelected randomWord


updateGame msg game =
    case game of
        Start ->
            case msg of
                KeyPressed (Control "Enter") ->
                    ( game, generateWordCmd )

                Begin ->
                    ( game, generateWordCmd )

                WordSelected word ->
                    initialPlayingModel word

                _ ->
                    game |> withCmd

        Playing word incorrect correct hangmanState ->
            case msg of
                KeyPressed (Character c) ->
                    let
                        isInWord =
                            String.any ((==) c) (noSpaces word)

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
                    game |> withCmd

        End result ->
            case msg of
                KeyPressed (Control "Enter") ->
                    ( game, generateWordCmd )

                Restart ->
                    ( game, generateWordCmd )

                WordSelected word ->
                    initialPlayingModel word

                _ ->
                    game |> withCmd


updateLang msg lang =
    case lang of
        PL ->
            case msg of
                LangChanged ENG ->
                    ENG

                _ ->
                    lang

        ENG ->
            case msg of
                LangChanged PL ->
                    PL

                _ ->
                    lang


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model lang game) =
    let
        nextLang =
            updateLang msg lang

        ( nextGame, cmds ) =
            updateGame msg game
    in
    ( Model nextLang nextGame, cmds )


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
            case c of
                ' ' ->
                    c

                _ ->
                    '_'



-- VIEW


type TranslationKey
    = Intro
    | ImReadyForAGame
    | GameWon
    | GameLost
    | SamePhraseAgain
    | AnotherPhrase
    | AsciiSource


type alias TranslationSnippet =
    { pl : String, eng : String }


chooseTranslation : Lang -> String -> String -> String
chooseTranslation l pl eng =
    case l of
        PL ->
            pl

        ENG ->
            eng


getTranslation : Lang -> TranslationKey -> String
getTranslation lang tk =
    case tk of
        Intro ->
            chooseTranslation lang "Co ty na partyjkę w wisielca?" "Up for a game of hangman?"

        ImReadyForAGame ->
            chooseTranslation lang "Tak!" "Yes!"

        GameWon ->
            chooseTranslation lang "Wygrałeś/-aś! Hasło to \"" "You won! The password is \""

        GameLost ->
            chooseTranslation lang "Przegrałeś/-aś!" "You lost!"

        SamePhraseAgain ->
            chooseTranslation lang "Dawaj jeszcze raz!" "Lemme try again!"

        AnotherPhrase ->
            chooseTranslation lang "Inne hasło? ( ͡° ͜ʖ ͡°)" "Different phrase? ( ͡° ͜ʖ ͡°)"

        AsciiSource ->
            chooseTranslation lang "Ascii art z " "Ascii art from "


view : Model -> Html Msg
view (Model lang game) =
    viewContainer
        [ viewGame lang game
        ]


viewLanguage t lang =
    case lang of
        PL ->
            button [ onClick (LangChanged ENG) ] [ text "Switch to English" ]

        ENG ->
            button [ onClick (LangChanged PL) ] [ text "Zmień na polski" ]


viewGame lang game =
    let
        t =
            getTranslation lang
    in
    case game of
        Start ->
            div []
                [ text (t Intro)
                , br [] []
                , button [ onClick Begin, style "margin-top" "1em" ] [ text (t ImReadyForAGame) ]
                , div [ style "margin-top" "1em" ]
                    [ viewLanguage t lang
                    ]
                , footer [ style "font-size" "12px", style "padding-top" "1em" ]
                    [ text "Wiktor Czajkowski 2019"
                    , br [] []
                    , text (t AsciiSource)
                    , a [ href "https://ascii.co.uk/art/hangman" ] [ text "ascii.co.uk" ]
                    ]
                ]

        Playing word incorrect correct hangmanState ->
            div []
                [ pre [ style "font-size" "18px" ] [ text (hangmanAscii hangmanState) ]
                , pre [ style "font-size" "18px" ] [ text (hiddenWordForm word correct) ]
                ]

        End result ->
            viewEnd t result


viewContainer children =
    div [ style "height" "100vh", style "display" "flex", style "justify-content" "center", style "align-items" "center", style "text-align" "center" ] children


viewResult t message =
    div []
        [ text message
        , br [] []
        , button [ onClick Restart, style "margin-top" "1em" ] [ text (t AnotherPhrase) ]
        ]


viewEnd t result =
    case result of
        Won word ->
            viewResult t (t GameWon ++ word ++ "\"!")

        Lost ->
            viewResult t (t GameLost)
