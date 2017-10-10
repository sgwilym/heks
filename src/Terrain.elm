module Terrain exposing (..)

import Random


type Terrain
    = Earth
    | Sea


randomTerrain : Random.Generator Terrain
randomTerrain =
    let
        terrainFromInt int =
            if int <= 6 then
                Earth
            else
                Sea
    in
        Random.map terrainFromInt (Random.int 0 10)


randomTerrainList : Int -> Random.Generator (List Terrain)
randomTerrainList length =
    Random.list length (randomTerrain)
