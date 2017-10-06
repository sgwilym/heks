module CrossGrid exposing (..)

import HexGrid exposing (HexGrid)
import Cons exposing (Cons, cons, uncons)
import Set exposing (Set)


type alias Hint =
    List Int


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


hint : Row a -> (a -> Bool) -> Hint
hint row isFilled =
    (Cons.foldl
        (\( point, item ) hint ->
            let
                ( head, tail ) =
                    uncons hint
            in
                if isFilled item then
                    case head of
                        Just number ->
                            cons (Just (number + 1)) tail

                        Nothing ->
                            cons (Just (1)) (head :: tail)
                else
                    cons Nothing (head :: tail)
        )
        (cons Nothing [])
        row
    )
        |> Cons.filterMap identity
        |> List.reverse
