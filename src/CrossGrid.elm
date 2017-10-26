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


type Group
    = Group GroupType Int


type GroupType
    = Filled
    | Crossed
    | Empty


groupTypeIsEqual : GroupType -> GroupType -> Bool
groupTypeIsEqual grouptypeA grouptypeB =
    let
        toInt groupType =
            case groupType of
                Filled ->
                    1

                Crossed ->
                    2

                Empty ->
                    3
    in
        toInt grouptypeA == toInt grouptypeB


guessToGroupType : Puzzle.Guess -> GroupType
guessToGroupType guess =
    case guess of
        Puzzle.Filled ->
            Filled

        Puzzle.Cross ->
            Crossed

        Puzzle.NoGuess ->
            Empty


toGroups : (a -> GroupType) -> Row a -> Cons Group
toGroups toGroupType row =
    let
        groupTypes =
            Cons.map
                (\( point, contents ) ->
                    toGroupType contents
                )
                row
    in
        Cons.foldl
            (\grouptype groups ->
                let
                    ( head, tail ) =
                        uncons groups

                    (Group headGroupType groupSize) =
                        head
                in
                    if groupTypeIsEqual headGroupType grouptype then
                        cons (Group headGroupType (groupSize + 1)) tail
                    else
                        cons (Group grouptype 1) (head :: tail)
            )
            (cons (Group (Cons.head groupTypes) 0) [])
            groupTypes
            |> Cons.reverse


groupsToFilledCounts : Cons Group -> List Int
groupsToFilledCounts groupCons =
    Cons.filter
        (\(Group groupType int) ->
            case groupType of
                Filled ->
                    True

                _ ->
                    False
        )
        groupCons
        |> List.map (\(Group _ int) -> int)


largestFilledCount : Cons Group -> Int
largestFilledCount groupCons =
    Cons.foldl
        (\(Group groupType size) largest ->
            case groupType of
                Filled ->
                    if size > largest then
                        size
                    else
                        largest

                _ ->
                    largest
        )
        0
        groupCons


groupsLength : Cons Group -> Int
groupsLength groupCons =
    Cons.foldl (\(Group _ size) length -> length + size) 0 groupCons


groupDrop : Int -> List Group -> List Group
groupDrop toDrop groups =
    List.foldl
        (\(Group groupType groupSize) ( leftovers, dropRemaining ) ->
            if dropRemaining > 0 then
                if groupSize > dropRemaining then
                    ( leftovers ++ [ (Group groupType (groupSize - dropRemaining)) ], 0 )
                else
                    ( leftovers ++ [ (Group groupType groupSize) ], dropRemaining - groupSize )
            else
                ( leftovers, dropRemaining )
        )
        ( [], toDrop )
        groups
        |> Tuple.first


enoughSpace : List Group -> List Group -> Bool -> Bool
enoughSpace solutionGroups guessGroups followsFilledGroup =
    let
        spaceAvailable =
            case Cons.fromList solutionGroups of
                Just groupCons ->
                    if followsFilledGroup then
                        (groupsLength groupCons) - 1
                    else
                        groupsLength groupCons

                Nothing ->
                    0

        firstFilledGroupNeeds =
            List.foldl
                (\(Group groupType size) ( occupies, isDone ) ->
                    if isDone then
                        ( occupies, isDone )
                    else
                        case groupType of
                            Filled ->
                                ( occupies + size, True )

                            _ ->
                                ( occupies + size, False )
                )
                ( 0, False )
                guessGroups
                |> Tuple.first
    in
        if spaceAvailable >= firstFilledGroupNeeds then
            enoughSpace (groupDrop firstFilledGroupNeeds solutionGroups) (groupDrop firstFilledGroupNeeds guessGroups) True
        else
            False



{--
all hints should be satisfied
 - if the groupings for solutions and guesses are equivalent
--}


allHintsSatisfied : (a -> GroupType) -> Row a -> Row Puzzle.Guess -> Bool
allHintsSatisfied toGroupType solutionRow guessRow =
    let
        solutionFilledGroupSizes =
            toGroups toGroupType solutionRow |> groupsToFilledCounts

        guessFilledGroupSizes =
            toGroups guessToGroupType guessRow |> groupsToFilledCounts
    in
        solutionFilledGroupSizes == guessFilledGroupSizes



{--
no hints should be satisfied
  - if there is not enough space for all groups left by current groups
  - if any of the groupings are too large
--}


zeroHintsSatisfied : (a -> GroupType) -> Row a -> Row Puzzle.Guess -> Bool
zeroHintsSatisfied toGroupType solutionRow guessRow =
    let
        solutionLargestFilledGroup =
            largestFilledCount (toGroups toGroupType solutionRow)

        guessLargestFilledGroup =
            largestFilledCount (toGroups guessToGroupType guessRow)
    in
        if guessLargestFilledGroup > 0 && guessLargestFilledGroup > solutionLargestFilledGroup then
            True
        else
            enoughSpace (toGroups toGroupType solutionRow |> Cons.toList) (toGroups guessToGroupType guessRow |> Cons.toList) False == False |> Debug.log "enoughSpace"



{--

each hint can be satisfied if
  - it matches a grouping
  - has crosses or grid boundaries as neighbours
--}


neighbourFold : (( Maybe a, a, Maybe a ) -> b -> b) -> b -> List a -> b
neighbourFold fold accumulator list =
    let
        foldForNeighbours : a -> ( ( Maybe a, Maybe (List a) ), b ) -> ( ( Maybe a, Maybe (List a) ), b )
        foldForNeighbours item ( ( prev, next ), acc ) =
            case next of
                Just tail ->
                    case List.head tail of
                        Just head ->
                            case List.tail tail of
                                Just tailOfTail ->
                                    ( ( Just item, Just tailOfTail ), fold ( prev, item, Just head ) acc )

                                Nothing ->
                                    ( ( Just item, Nothing ), fold ( prev, item, Just head ) acc )

                        Nothing ->
                            ( ( Just item, Nothing ), fold ( prev, item, Nothing ) acc )

                Nothing ->
                    ( ( Just item, Nothing ), fold ( prev, item, Nothing ) acc )
    in
        List.foldl
            foldForNeighbours
            ( ( Nothing, List.tail list ), accumulator )
            list
            |> Tuple.second


hintsForRow : (a -> Bool) -> Row a -> Row Puzzle.Guess -> Hint
hintsForRow isFilled solutionRow guessRow =
    let
        toGroupType item =
            if isFilled item then
                Filled
            else
                Empty
    in
        if allHintsSatisfied toGroupType solutionRow guessRow then
            toGroups toGroupType solutionRow
                |> groupsToFilledCounts
                |> List.map (\count -> ( count, True ))
        else if zeroHintsSatisfied toGroupType solutionRow guessRow then
            toGroups toGroupType solutionRow
                |> groupsToFilledCounts
                |> List.map (\count -> ( count, False ))
        else
            let
                solutionFilledGroupSizes =
                    toGroups toGroupType solutionRow |> groupsToFilledCounts

                addHint hints remainingSolutions hintSize isSatisfied =
                    case List.tail remainingSolutions of
                        Just tail ->
                            ( hints ++ [ ( hintSize, isSatisfied ) ]
                            , if isSatisfied then
                                tail
                              else
                                remainingSolutions
                            )

                        Nothing ->
                            ( hints ++ [ ( hintSize, isSatisfied ) ], [] )
            in
                neighbourFold
                    (\( maybePrevious, currentGroup, maybeNext ) ( hint, remainingSolutions ) ->
                        case List.head remainingSolutions of
                            Just solution ->
                                let
                                    hintAdder =
                                        addHint hint remainingSolutions

                                    (Group currentGroupType currentSize) =
                                        currentGroup
                                in
                                    if solution == currentSize then
                                        case currentGroupType of
                                            Filled ->
                                                case maybePrevious of
                                                    Just (Group groupType size) ->
                                                        case groupType of
                                                            Crossed ->
                                                                hintAdder size True

                                                            _ ->
                                                                hintAdder size False

                                                    Nothing ->
                                                        case maybeNext of
                                                            Just (Group groupType size) ->
                                                                case groupType of
                                                                    Crossed ->
                                                                        hintAdder size True

                                                                    _ ->
                                                                        hintAdder size False

                                                            Nothing ->
                                                                ( hint, remainingSolutions )

                                            _ ->
                                                ( hint, remainingSolutions )
                                    else
                                        ( hint, remainingSolutions )

                            Nothing ->
                                ( hint, remainingSolutions )
                    )
                    ( [], solutionFilledGroupSizes )
                    (Cons.toList (toGroups guessToGroupType guessRow))
                    |> Tuple.first
