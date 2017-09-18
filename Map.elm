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
