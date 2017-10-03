module Beast exposing (..)

import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Set exposing (Set)


type alias Beast =
    { location : HexGrid.Point
    }


beastTowardsPasture : Beast -> HexGrid Terrain -> Beast
beastTowardsPasture beast grid =
    let
        obstacles : Set HexGrid.Point
        obstacles =
            HexGrid.filter
                (\( point, terrain ) ->
                    case terrain of
                        Terrain.Sea ->
                            True

                        _ ->
                            False
                )
                grid
                |> List.map Tuple.first
                |> Set.fromList

        reachablePastures : List HexGrid.Point
        reachablePastures =
            let
                reachables =
                    HexGrid.reachable beast.location 5 obstacles
            in
                HexGrid.filter
                    (\( point, terrain ) ->
                        case terrain of
                            Terrain.Pasture _ ->
                                Set.member point reachables

                            _ ->
                                False
                    )
                    grid
                    |> List.map Tuple.first

        pasturesPointsWithDistances : List ( HexGrid.Point, Int )
        pasturesPointsWithDistances =
            List.map (\point -> ( point, HexGrid.distance beast.location point )) reachablePastures
                |> List.sortBy (\( point, distance ) -> distance)

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
