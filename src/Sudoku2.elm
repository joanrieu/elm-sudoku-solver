module Sudoku exposing (..)

import Set exposing (Set)


sampleGrid : Grid
sampleGrid =
    -- "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
    "003900001020305806600001400008700006102000708900008200002800005609203010500009300"
        |> stringToGrid


type alias Grid =
    List Cell


stringToGrid : String -> Grid
stringToGrid str =
    str
        |> String.toList
        |> List.map charToCell


type alias Cell =
    Maybe Int


charToCell : Char -> Cell
charToCell char =
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


cellToChar : Cell -> Char
cellToChar cell =
    case cell of
        Just 1 ->
            '1'

        Just 2 ->
            '2'

        Just 3 ->
            '3'

        Just 4 ->
            '4'

        Just 5 ->
            '5'

        Just 6 ->
            '6'

        Just 7 ->
            '7'

        Just 8 ->
            '8'

        Just 9 ->
            '9'

        _ ->
            '.'


gridToString : List Cell -> String
gridToString grid =
    let
        insertLineBreaks chars =
            if List.isEmpty chars then
                []
            else
                List.concat
                    [ List.take 9 chars
                    , [ '\n' ]
                    , List.drop 9 chars |> insertLineBreaks
                    ]
    in
        grid
            |> List.map cellToChar
            |> insertLineBreaks
            |> String.fromList


solve : Grid -> Grid
solve grid =
    grid
        |> mapOnGroups expandCellOptions
        |> mapOnGroups reduceUniqueOptions
        |> foldCellOptions


mapOnGroups : (List cell -> List (Set Int)) -> List cell -> List (Set Int)
mapOnGroups op grid =
    let
        rows =
            grid |> splitRows |> List.map op |> joinRows

        columns =
            grid |> splitColumns |> List.map op |> joinColumns

        squares =
            grid |> splitSquares |> List.map op |> joinSquares
    in
        List.map3 (\r c s -> r |> Set.intersect c |> Set.intersect s)
            rows
            columns
            squares
            |> List.map
                (\options ->
                    let
                        crash =
                            if Set.isEmpty options then
                                Debug.crash "early crash"
                            else
                                "ok"
                    in
                        options
                )


splitRows : List cell -> List (List cell)
splitRows grid =
    case List.isEmpty grid of
        True ->
            []

        False ->
            (List.take 9 grid) :: (List.drop 9 grid |> splitRows)


joinRows : List (List cell) -> List cell
joinRows rows =
    List.concat rows


splitColumns : List cell -> List (List cell)
splitColumns grid =
    let
        extractColumn : List cell -> List cell
        extractColumn alignedGrid =
            List.head alignedGrid
                |> Maybe.map (\cell -> cell :: extractColumn (List.drop 9 alignedGrid))
                |> Maybe.withDefault []
    in
        [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
            |> List.map (\skip -> List.drop skip grid)
            |> List.map extractColumn


joinColumns : List (List cell) -> List cell
joinColumns columns =
    let
        column1 : List cell
        column1 =
            List.head columns |> Maybe.withDefault []

        cell1 : Maybe cell
        cell1 =
            List.head column1
    in
        case cell1 of
            Just x ->
                let
                    columnsWithoutColumn1 =
                        List.drop 1 columns

                    column1WithoutCell1 =
                        List.drop 1 column1

                    columnsRotated =
                        List.concat [ columnsWithoutColumn1, [ column1WithoutCell1 ] ]
                in
                    x :: (joinColumns columnsRotated)

            Nothing ->
                []


splitSquares : List cell -> List (List cell)
splitSquares grid =
    let
        extractSquare : List cell -> List cell
        extractSquare alignedGrid =
            [ 0, 1, 2 ]
                |> List.concatMap (\n -> alignedGrid |> List.drop (n * 9) |> List.take 3)
    in
        [ 0, 1, 2 ]
            |> List.map (\n -> n * 3 * 9)
            |> List.concatMap (\n -> [ n, n + 3, n + 6 ])
            |> List.map (\skip -> List.drop skip grid)
            |> List.map extractSquare


joinSquares : List (List cell) -> List cell
joinSquares squares =
    let
        triplet : ( Int, Int ) -> List cell
        triplet ( square, row ) =
            squares
                |> List.drop square
                |> List.head
                |> Maybe.withDefault []
                |> List.drop (3 * row)
                |> List.take 3

        row ( firstSquare, row ) =
            [ 0, 1, 2 ]
                |> List.map (\n -> ( firstSquare + n, row ))
                |> List.concatMap triplet

        rows3 firstSquare =
            [ 0, 1, 2 ]
                |> List.map (\n -> ( firstSquare, n ))
                |> List.concatMap row
    in
        [ 0, 3, 6 ]
            |> List.concatMap rows3


type alias CellOptions =
    Set Int


expandCellOptions : List Cell -> List CellOptions
expandCellOptions cells =
    let
        allDigits : Set Int
        allDigits =
            Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

        usedDigits : Set Int
        usedDigits =
            cells
                |> List.filterMap identity
                |> Set.fromList

        remainingDigits : Set Int
        remainingDigits =
            Set.diff allDigits usedDigits

        cellsAsSets : List (Set Int)
        cellsAsSets =
            cells
                |> List.map
                    (\cell ->
                        case cell of
                            Just x ->
                                Set.singleton x

                            Nothing ->
                                remainingDigits
                    )
    in
        cellsAsSets


reduceUniqueOptions : List (Set Int) -> List (Set Int)
reduceUniqueOptions cellOptions =
    let
        comm3 : List (Set Int) -> Set Int
        comm3 sets =
            let
                watcher : Int -> ( Set Int, Set Int ) -> ( Set Int, Set Int )
                watcher value ( seen, seenAgain ) =
                    if Set.member value seen then
                        ( seen, Set.insert value seenAgain )
                    else
                        ( Set.insert value seen, seenAgain )

                ( seen, seenAgain ) =
                    sets
                        |> List.concatMap Set.toList
                        |> List.foldr watcher ( Set.empty, Set.empty )
            in
                Set.diff seen seenAgain

        digitsWithOnePossiblePlace : Set Int
        digitsWithOnePossiblePlace =
            comm3 cellOptions

        chooseUniqueOptionIfAvailable : Set Int -> Set Int
        chooseUniqueOptionIfAvailable options =
            let
                unique =
                    Set.intersect options digitsWithOnePossiblePlace
            in
                if Set.isEmpty unique then
                    options
                else
                    unique
    in
        List.map chooseUniqueOptionIfAvailable cellOptions


foldCellOptions : List CellOptions -> List Cell
foldCellOptions cellOptions =
    cellOptions
        |> List.map
            (\set ->
                case Set.size set of
                    0 ->
                        Debug.crash "dead end"

                    1 ->
                        set
                            |> Set.toList
                            |> List.head

                    _ ->
                        Nothing
            )
