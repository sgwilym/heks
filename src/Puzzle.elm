module Puzzle exposing (..)

import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Random
import Dict


type Guess
    = Filled
    | Cross
    | NoGuess


type Puzzle
    = Puzzle (HexGrid Terrain) (HexGrid Guess)


randomPuzzle : Int -> Random.Generator Puzzle
randomPuzzle radius =
    let
        (HexGrid.HexGrid gridRadius dict) =
            HexGrid.empty radius Terrain.Sea

        gridSize =
            Dict.size dict

        points =
            Dict.keys dict

        fromList terrains =
            let
                zippedPointsAndTerrains =
                    List.map2 (,) points terrains
            in
                Puzzle
                    (HexGrid.fromList radius Terrain.Sea zippedPointsAndTerrains)
                    (HexGrid.empty radius NoGuess)
    in
        Random.map (fromList) (Terrain.randomTerrainList gridSize)
