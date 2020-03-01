module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, a, br, button, div, footer, input, label, p, pre, text)
import Html.Attributes exposing (href, style, type_, value)
import Html.Events exposing (on, onClick)
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


randomWord : Lang -> Random.Generator Word
randomWord lang =
    case lang of
        PL ->
            Random.uniform "komu w drogę temu cukier do szafy" [ "baba z wozu nie ma co jeść", "kto pod kim dołki kopie ten nosił wilka", "gość w dom razy kilka ", "w matematyce nie ma dróg królewskich" ]

        ENG ->
            Random.uniform "tis but a scratch" [ "its just a flesh wound", "ministry of silly walks", "no royal road to mathematics" ]


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
    | Lost Word


type Game
    = Start
    | Playing Word CorrectLetters IncorrectLetters HangmanState
    | End Result


type Lang
    = PL
    | ENG


type alias IsTouchDevice =
    Bool


type Model
    = Model Lang Game IsTouchDevice


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model PL Start False, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions (Model _ _ isTouchDevice) =
    case isTouchDevice of
        False ->
            onKeyPress keyDecoder

        True ->
            Sub.none



-- UPDATE


type Msg
    = KeyPressed Key
    | Begin
    | AnotherPhrase
    | TryAgain
    | WordSelected Word
    | LangChanged Lang
    | StartButtonTouched


withCmd model =
    ( model, Cmd.none )


initialPlayingModel word =
    Playing word Set.empty Set.empty (Transitionable Nothing) |> withCmd


generateWordCmd lang =
    Random.generate WordSelected (randomWord lang)


updateGame msg (Model lang game _) =
    case game of
        Start ->
            case msg of
                Begin ->
                    ( game, generateWordCmd lang )

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
                            End (Lost word) |> withCmd

                        ( False, Transitionable s, _ ) ->
                            Playing word (Set.insert c incorrect) correct (nextHangmanState s) |> withCmd

                _ ->
                    game |> withCmd

        End result ->
            case msg of
                AnotherPhrase ->
                    ( game, generateWordCmd lang )

                WordSelected word ->
                    initialPlayingModel word

                _ ->
                    case result of
                        Lost word ->
                            case msg of
                                TryAgain ->
                                    initialPlayingModel word

                                _ ->
                                    game |> withCmd

                        _ ->
                            game |> withCmd


updateLang msg (Model lang _ _) =
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


updateIsTouchDevice msg (Model _ _ isTouchDevice) =
    case msg of
        StartButtonTouched ->
            True

        _ ->
            isTouchDevice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatedLang =
            updateLang msg model

        ( updatedGame, cmds ) =
            updateGame msg model

        updatedIsTouchDevice =
            updateIsTouchDevice msg model
    in
    ( Model updatedLang updatedGame updatedIsTouchDevice, cmds )


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
    | GiveMeSamePhraseAgain
    | GiveMeAnotherPhrase
    | AsciiSource
    | TypeHereToPlay


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

        GiveMeSamePhraseAgain ->
            chooseTranslation lang "Dawaj jeszcze raz!" "Lemme try again!"

        GiveMeAnotherPhrase ->
            chooseTranslation lang "Inne hasło? ( ͡° ͜ʖ ͡°)" "Different phrase? ( ͡° ͜ʖ ͡°)"

        AsciiSource ->
            chooseTranslation lang "Ascii art z " "Ascii art from "

        TypeHereToPlay ->
            chooseTranslation lang "Pisz tutaj: " "Type here: "


view : Model -> Html Msg
view model =
    viewContainer
        [ viewGame model
        ]


viewLanguage t lang =
    case lang of
        PL ->
            button [ onClick (LangChanged ENG) ] [ text "Switch to English" ]

        ENG ->
            button [ onClick (LangChanged PL) ] [ text "Zmień na polski" ]


viewInputForMobile t isTouchDevice =
    case isTouchDevice of
        True ->
            div []
                [ label []
                    [ text (t TypeHereToPlay)
                    , input
                        [ type_ "text", value "", on "keydown" keyDecoder ]
                        []
                    ]
                ]

        False ->
            text ""


viewGame (Model lang game isTouchDevice) =
    let
        t =
            getTranslation lang
    in
    case game of
        Start ->
            div []
                [ text (t Intro)
                , br [] []
                , button [ onClick Begin, on "touchstart" (Decode.succeed StartButtonTouched), style "margin-top" "1em" ] [ text (t ImReadyForAGame) ]
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
                , viewInputForMobile t isTouchDevice
                ]

        End result ->
            viewEnd t result


viewContainer children =
    div [ style "height" "100vh", style "display" "flex", style "justify-content" "center", style "align-items" "center", style "text-align" "center" ] children


viewEnd t result =
    case result of
        Won word ->
            div
                []
                [ text (t GameWon ++ word ++ "\"!")
                , br [] []
                , button [ onClick AnotherPhrase, style "margin-top" "1em" ] [ text (t GiveMeAnotherPhrase) ]
                ]

        Lost word ->
            div
                []
                [ text (t GameLost)
                , br [] []
                , button [ onClick AnotherPhrase, style "margin-top" "1em" ] [ text (t GiveMeAnotherPhrase) ]
                , button [ onClick TryAgain, style "margin-top" "1em" ] [ text (t GiveMeSamePhraseAgain) ]
                ]
