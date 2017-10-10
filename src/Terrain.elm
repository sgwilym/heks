module Terrain exposing (..)

import HexGrid exposing (HexGrid)
import Set exposing (Set)
import Random
import Dict


type Terrain
    = Earth
    | Sea
    | Mountain
    | Pasture Int


type alias TerrainSpec =
    { earth : Int
    , sea : Int
    , mountain : Int
    , pasture : Int
    }


randomTerrain : TerrainSpec -> Random.Generator Terrain
randomTerrain spec =
    let
        total =
            spec.earth + spec.sea + spec.mountain + spec.pasture

        oneOrThree int =
            if int % 2 == 0 then
                1
            else
                3

        terrainFromInt : Int -> Terrain
        terrainFromInt int =
            let
                earthUpper =
                    spec.earth

                seaUpper =
                    earthUpper + spec.sea

                mountainUpper =
                    seaUpper + spec.mountain

                pastureUpper =
                    mountainUpper + spec.pasture
            in
                if int <= earthUpper then
                    Earth
                else if int > earthUpper && int <= seaUpper then
                    Sea
                else if int > seaUpper && int <= mountainUpper then
                    Mountain
                else
                    Pasture (oneOrThree int)
    in
        Random.map (terrainFromInt) (Random.int 0 total)


randomTerrainList : TerrainSpec -> Int -> Random.Generator (List Terrain)
randomTerrainList spec cellNumber =
    let
        total =
            spec.earth + spec.sea + spec.mountain + spec.pasture
    in
        Random.list cellNumber (randomTerrain spec)


randomGrid : TerrainSpec -> Int -> Random.Generator (HexGrid Terrain)
randomGrid spec radius =
    let
        (HexGrid.HexGrid gridRadius dict) =
            HexGrid.empty radius Sea

        gridSize =
            Dict.size dict

        points =
            Dict.keys dict

        fromList terrains =
            let
                zippedPointsAndTerrains =
                    List.map2 (,) points terrains
            in
                HexGrid.fromList radius Sea zippedPointsAndTerrains
                    |> HexGrid.insert ( 0, 0 ) Earth
    in
        Random.map (fromList) (randomTerrainList spec gridSize)


terrainIsEqual : Terrain -> Terrain -> Bool
terrainIsEqual terrain terrain2 =
    let
        toInt terr =
            case terr of
                Earth ->
                    0

                Sea ->
                    1

                Mountain ->
                    2

                Pasture _ ->
                    3
    in
        (toInt terrain) == (toInt terrain2)


pointsOfTerrains : Terrain -> HexGrid Terrain -> Set HexGrid.Point
pointsOfTerrains terrainPredicate grid =
    HexGrid.filter
        (\( point, terrain ) ->
            terrainIsEqual terrainPredicate terrain
        )
        grid
        |> List.map Tuple.first
        |> Set.fromList


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
