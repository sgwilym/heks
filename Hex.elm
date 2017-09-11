module Hex exposing (..)


type alias Hex =
    { q : Int, r : Int }


s : Hex -> Int
s hex =
    -hex.q - hex.r



-- Arithmetic


add : Hex -> Hex -> Hex
add hexA hexB =
    Hex (hexA.q + hexB.q) (hexA.r + hexB.r)


subtract : Hex -> Hex -> Hex
subtract hexA hexB =
    Hex (hexA.q - hexB.q) (hexA.r - hexB.r)


multiply : Hex -> Int -> Hex
multiply hexA int =
    Hex (hexA.q * int) (hexA.r * int)



-- Distance


length : Hex -> Int
length hex =
    round
        (toFloat
            ((abs hex.q) + (abs hex.r) + (abs (s hex)))
            / 2
        )


distance : Hex -> Hex -> Int
distance hexA hexB =
    length (subtract hexA hexB)



-- Neighbours
-- I might need to find a way to convert any integer to a Direction later, as I created this type to limit the options that go into the direction function


type Direction
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


direction : Direction -> Hex
direction direction =
    case direction of
        One ->
            Hex 1 0

        Two ->
            Hex 1 -1

        Three ->
            Hex 0 -1

        Four ->
            Hex -1 0

        Five ->
            Hex -1 1

        Six ->
            Hex 0 1


neighbour : Hex -> Direction -> Hex
neighbour hex dir =
    add hex (direction dir)
