module Beast exposing (..)

import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Set exposing (Set)
import InterestingVariables


type alias Beast =
    { location : HexGrid.Point
    }



-- The beast should be able to pathfind towards anything within its viewrange


beastTowardsPasture : Beast -> HexGrid Terrain -> Beast
beastTowardsPasture beast grid =
    let
        pointsInViewRange =
            HexGrid.range InterestingVariables.beastViewRange beast.location
                |> Set.fromList

        obstacles : Set HexGrid.Point
        obstacles =
            Set.union
                (Terrain.pointsOfTerrains Terrain.Sea grid)
                (Terrain.pointsOfTerrains Terrain.Mountain grid)

        viewBlockers : Set HexGrid.Point
        viewBlockers =
            Terrain.pointsOfTerrains Terrain.Mountain grid
                |> Set.intersect pointsInViewRange

        unseenPoints : Set HexGrid.Point
        unseenPoints =
            HexGrid.fogOfWar beast.location viewBlockers grid

        pasturesInRange : Set HexGrid.Point
        pasturesInRange =
            Terrain.pointsOfTerrains (Terrain.Pasture 0) grid |> Set.intersect pointsInViewRange

        pasturesPointsWithDistances : List ( HexGrid.Point, Int )
        pasturesPointsWithDistances =
            List.map (\point -> ( point, HexGrid.distance beast.location point )) (Set.toList pasturesInRange)
                |> List.filter
                    (\( point, _ ) ->
                        case HexGrid.countSteps beast.location point obstacles 10 of
                            Just steps ->
                                True

                            Nothing ->
                                False
                    )
                |> List.sortBy (Tuple.second)

        getNextPoint : HexGrid.Point -> HexGrid.Point
        getNextPoint destination =
            let
                path =
                    HexGrid.pathfind beast.location destination obstacles grid
            in
                case List.tail path of
                    Just tail ->
                        case tail of
                            [] ->
                                beast.location

                            [ head ] ->
                                head

                            head :: tail ->
                                head

                    Nothing ->
                        beast.location
    in
        case pasturesPointsWithDistances of
            [] ->
                beast

            [ head ] ->
                { beast | location = getNextPoint (Tuple.first head) }

            head :: tail ->
                { beast | location = getNextPoint (Tuple.first head) }
