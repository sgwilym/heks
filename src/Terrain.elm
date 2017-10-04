module Terrain exposing (..)

import HexGrid exposing (HexGrid)
import Set exposing (Set)


type Terrain
    = Earth
    | Sea
    | Pasture Int


grazedGrid : HexGrid Terrain -> Int -> { location : HexGrid.Point } -> ( HexGrid Terrain, Int )
grazedGrid grid landAvailable beast =
    let
        pastures : Set HexGrid.Point
        pastures =
            HexGrid.filter
                (\( point, terrain ) ->
                    case terrain of
                        Pasture _ ->
                            True

                        _ ->
                            False
                )
                grid
                |> List.map Tuple.first
                |> Set.fromList
    in
        case Set.member beast.location pastures of
            True ->
                let
                    ( nextTerrain, nextLandAvailable ) =
                        case HexGrid.valueAt beast.location grid of
                            Just terrain ->
                                case terrain of
                                    Pasture grassLeft ->
                                        if grassLeft > 1 then
                                            ( Pasture (grassLeft - 1), landAvailable + 1 )
                                        else
                                            ( Earth, landAvailable + 1 )

                                    _ ->
                                        ( terrain, landAvailable )

                            Nothing ->
                                ( Sea, landAvailable )
                in
                    ( HexGrid.insert beast.location nextTerrain grid, nextLandAvailable )

            False ->
                ( grid, landAvailable )
