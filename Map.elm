module Map exposing (..)

import Dict exposing (Dict)
import Hex exposing (Hex)


type Terrain
    = Earth
    | Sea


type alias Map =
    Dict Hash Terrain


type alias Hash =
    ( Int, Int )


hexToHash : Hex -> Hash
hexToHash hex =
    ( hex.q, hex.r )


update : Map -> Hash -> Terrain -> Map
update map hash newTerrain =
    Dict.update hash (\maybeTerrain -> Just newTerrain) map


neighboursTerrain : Map -> Hex -> Hex.Direction -> Maybe Terrain
neighboursTerrain map hex direction =
    let
        neighbourHex =
            Hex.neighbour hex direction
    in
        Dict.get (hexToHash neighbourHex) map


allNeighboursTerrain :
    Map
    -> Hex
    -> { five : Maybe Terrain
       , four : Maybe Terrain
       , one : Maybe Terrain
       , six : Maybe Terrain
       , three : Maybe Terrain
       , two : Maybe Terrain
       }
allNeighboursTerrain map hex =
    let
        getTerrain =
            neighboursTerrain map hex
    in
        { one = getTerrain Hex.One
        , two = getTerrain Hex.Two
        , three = getTerrain Hex.Three
        , four = getTerrain Hex.Four
        , five = getTerrain Hex.Five
        , six = getTerrain Hex.Six
        }


hexagonOfSea : Int -> Map
hexagonOfSea radius =
    let
        listOfCells =
            List.range -radius radius
                |> List.map
                    (\q ->
                        let
                            r1 =
                                max -radius (-q - radius)

                            r2 =
                                min radius (-q + radius)
                        in
                            List.range r1 r2
                                |> List.map
                                    (\r ->
                                        ( ( q, r ), Sea )
                                    )
                    )
                |> List.concat
    in
        Dict.fromList listOfCells
