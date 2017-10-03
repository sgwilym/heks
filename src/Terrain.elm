module Terrain exposing (..)

import HexGrid exposing (HexGrid)
import Set exposing (Set)


type Grassiness
    = Untouched
    | Grazed


type Terrain
    = Earth
    | Sea
    | Pasture Grassiness


grazedGrid : HexGrid Terrain -> { location : HexGrid.Point } -> HexGrid Terrain
grazedGrid grid beast =
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
                    nextTerrain =
                        case HexGrid.valueAt beast.location grid of
                            Just terrain ->
                                case terrain of
                                    Pasture grassiness ->
                                        case grassiness of
                                            Untouched ->
                                                Pasture Grazed

                                            Grazed ->
                                                Earth

                                    _ ->
                                        terrain

                            Nothing ->
                                Sea
                in
                    HexGrid.insert beast.location nextTerrain grid

            False ->
                grid
