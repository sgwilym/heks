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
        obstacles : Set HexGrid.Point
        obstacles =
            Set.union
                (Terrain.pointsOfTerrains Terrain.Sea grid)
                (Terrain.pointsOfTerrains Terrain.Mountain grid)

        viewBlockers =
            Terrain.pointsOfTerrains Terrain.Mountain grid

        unseenPoints : Set HexGrid.Point
        unseenPoints =
            HexGrid.fogOfWar beast.location viewBlockers grid

        pastures : List HexGrid.Point
        pastures =
            Terrain.pointsOfTerrains (Terrain.Pasture 0) grid |> Set.toList

        pasturesPointsWithDistances : List ( HexGrid.Point, Int )
        pasturesPointsWithDistances =
            List.map (\point -> ( point, HexGrid.distance beast.location point )) pastures
                |> List.filter (\( _, distance ) -> distance < InterestingVariables.beastViewRange + 1)
                |> List.filter (\( point, _ ) -> Set.member point unseenPoints == False)
                |> List.filter
                    (\( point, _ ) ->
                        case HexGrid.countSteps beast.location point obstacles 50 of
                            Just steps ->
                                True

                            Nothing ->
                                False
                    )
                |> List.sortBy (\( _, distance ) -> distance)

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
