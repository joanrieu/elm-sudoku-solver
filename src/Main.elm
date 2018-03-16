module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Sudoku exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { sudoku : Sudoku
    }


type Msg
    = ResetSudoku
    | SolveSudoku


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetSudoku ->
            init

        SolveSudoku ->
            ( { sudoku = model.sudoku |> solve }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        wrap str =
            if String.length str == 0 then
                ""
            else
                let
                    left =
                        str |> String.toList |> List.take 9 |> String.fromList

                    right =
                        str |> String.toList |> List.drop 9 |> String.fromList |> wrap
                in
                    left ++ "\n" ++ right
    in
        div
            [ style
                [ ( "width", "30em" )
                , ( "margin", "auto" )
                ]
            ]
            [ h1 [] [ text "Sudoku Solver" ]
            , br [] []
            , pre
                [ style
                    [ ( "font-size", "2em" )
                    , ( "letter-spacing", "1em" )
                    , ( "line-height", "1.5" )
                    , ( "display", "inline-block" )
                    , ( "padding", ".5em" )
                    , ( "background"
                      , "linear-gradient(90deg, transparent 30%, rgba(0, 0, 0, .2) 30%, rgba(0, 0, 0, .2) 63%, transparent 63%), "
                            ++ "linear-gradient(transparent 33%, rgba(0, 0, 0, .2) 33%, rgba(0, 0, 0, .2) 66%, transparent 66%)"
                      )
                    ]
                ]
                [ model.sudoku |> sudokuToString |> wrap |> text
                ]
            , br [] []
            , button
                [ onClick SolveSudoku
                , style
                    [ ( "margin", "1em" )
                    , ( "padding", ".2em .5em" )
                    , ( "font-size", "1.5em" )
                    ]
                ]
                [ text "Solve" ]
            , button [ onClick ResetSudoku ] [ text "Reset" ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    let
        sampleSudoku =
            "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
    in
        ( { sudoku = sudokuFromString sampleSudoku }, Cmd.none )
