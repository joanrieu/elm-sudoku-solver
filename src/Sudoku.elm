module Sudoku exposing (..)

import Dict exposing (..)
import Set exposing (..)


type alias Sudoku =
    Dict Position (Maybe Int)


type alias Position =
    ( Int, Int, Int )


rows : List Int
rows =
    [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9 ]


columns : List Int
columns =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]


squares : List Int
squares =
    [ 1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 4, 4, 4, 5, 5, 5, 6, 6, 6, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9, 7, 7, 7, 8, 8, 8, 9, 9, 9, 7, 7, 7, 8, 8, 8, 9, 9, 9 ]


sudokuFromString : String -> Sudoku
sudokuFromString string =
    let
        value char =
            case char of
                '1' ->
                    Just 1

                '2' ->
                    Just 2

                '3' ->
                    Just 3

                '4' ->
                    Just 4

                '5' ->
                    Just 5

                '6' ->
                    Just 6

                '7' ->
                    Just 7

                '8' ->
                    Just 8

                '9' ->
                    Just 9

                _ ->
                    Nothing

        cell row column square value =
            ( ( row, column, square ), value )
    in
        string
            |> String.toList
            |> List.map value
            |> List.map4 cell rows columns squares
            |> Dict.fromList


solve : Sudoku -> Sudoku
solve sudoku =
    sudoku
        |> Dict.map
            (\pos ->
                \val ->
                    case val of
                        Just x ->
                            Just x

                        Nothing ->
                            let
                                vals =
                                    sudoku
                                        |> Dict.filter
                                            (\pos2 ->
                                                \val2 ->
                                                    neighbors pos pos2
                                            )
                                        |> Dict.values
                                        |> List.filterMap identity
                                        |> List.map Set.singleton
                                        |> List.foldl Set.union Set.empty
                                        |> Set.diff digits
                            in
                                if Set.size vals == 1 then
                                    vals |> Set.toList |> List.head
                                else if Set.size vals == 0 then
                                    Nothing |> Debug.log ("no possible value at " ++ (toString pos))
                                else
                                    Nothing
            )


digits : Set Int
digits =
    Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]


neighbors : Position -> Position -> Bool
neighbors ( row1, col1, sq1 ) ( row2, col2, sq2 ) =
    row1 == row2 || col1 == col2 || sq1 == sq2


sudokuToString : Sudoku -> String
sudokuToString sudoku =
    List.map3 (\row -> \col -> \sq -> ( row, col, sq )) rows columns squares
        |> List.map (\pos -> Dict.get pos sudoku)
        |> List.filterMap identity
        |> List.map
            (Maybe.andThen
                (\val ->
                    case val of
                        1 ->
                            Just '1'

                        2 ->
                            Just '2'

                        3 ->
                            Just '3'

                        4 ->
                            Just '4'

                        5 ->
                            Just '5'

                        6 ->
                            Just '6'

                        7 ->
                            Just '7'

                        8 ->
                            Just '8'

                        9 ->
                            Just '9'

                        _ ->
                            Nothing
                )
            )
        |> List.map (Maybe.withDefault '.')
        |> String.fromList
