module CrossGrid exposing (..)

import HexGrid exposing (HexGrid)
import Cons exposing (Cons, cons, uncons)
import Set exposing (Set)
import Puzzle


type alias Hint =
    List ( Int, Bool )


type Axis
    = X
    | Z


type alias Row a =
    Cons ( HexGrid.Point, a )


rowNumbers : HexGrid a -> { x : Set Int, z : Set Int }
rowNumbers grid =
    HexGrid.foldl
        (\( x, z ) _ rows ->
            { x = Set.insert x rows.x
            , z = Set.insert z rows.z
            }
        )
        { x = Set.empty, z = Set.empty }
        grid


rows : HexGrid a -> ( List (Row a), List (Row a) )
rows grid =
    let
        numbers =
            rowNumbers grid
    in
        ( (Set.toList numbers.x |> List.filterMap (rowAt grid X)), (Set.toList numbers.z |> List.filterMap (rowAt grid Z)) )


rowAt : HexGrid a -> Axis -> Int -> Maybe (Row a)
rowAt grid axis rowNumber =
    let
        rowFilter : ( HexGrid.Point, a ) -> Bool
        rowFilter ( ( x, z ), _ ) =
            case axis of
                X ->
                    x == rowNumber

                Z ->
                    z == rowNumber
    in
        case HexGrid.filter rowFilter grid of
            [] ->
                Nothing

            head :: tail ->
                Just (cons head tail)



{--
This needs to be rewritten so that guesses get their own groupfold that is aware of empty cells and crosses
--}


hint : Row a -> Row Puzzle.Guess -> (a -> Bool) -> Hint
hint gridRow guessRow isFilled =
    let
        guessIsFilled =
            (\guess ->
                case guess of
                    Puzzle.Filled ->
                        True

                    _ ->
                        False
            )

        groupFold isFilled_ =
            (\( point, whatever ) hint ->
                let
                    ( head, tail ) =
                        uncons hint
                in
                    if isFilled_ whatever then
                        case head of
                            Just number ->
                                cons (Just (number + 1)) tail

                            Nothing ->
                                cons (Just (1)) (head :: tail)
                    else
                        cons Nothing (head :: tail)
            )

        getGroupings isFilled_ row =
            (Cons.foldl
                (groupFold isFilled_)
                (cons Nothing [])
                row
            )
                |> Cons.filterMap identity
                |> List.reverse

        gridGroupings =
            getGroupings isFilled gridRow

        guessGroupings =
            getGroupings guessIsFilled guessRow
    in
        if guessGroupings < gridGroupings then
            let
                diff =
                    List.length gridGroupings - List.length guessGroupings

                guessGroupingsExtended =
                    guessGroupings ++ List.repeat diff 0
            in
                List.map2 (\hintGroup guessGroup -> ( hintGroup, hintGroup == guessGroup )) (gridGroupings) (guessGroupingsExtended)
        else
            List.map2 (\hintGroup guessGroup -> ( hintGroup, hintGroup == guessGroup )) (gridGroupings) (guessGroupings)
