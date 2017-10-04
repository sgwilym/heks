module View exposing (..)

import Collage
import Text
import Color
import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Beast exposing (Beast)
import InterestingVariables
import Set


beastToForm : HexGrid.Layout -> Beast -> Collage.Form
beastToForm layout beast =
    Collage.circle 5 |> Collage.filled Color.white |> Collage.move (HexGrid.hexToPixel layout beast.location)


hexToForm : HexGrid.Layout -> HexGrid Terrain -> Beast -> HexGrid.Point -> List Collage.Form
hexToForm layout grid beast point =
    let
        isInViewOfBeast =
            if HexGrid.distance beast.location point < InterestingVariables.beastViewRange then
                Set.member point (HexGrid.fogOfWar beast.location (Terrain.pointsOfTerrains Terrain.Mountain grid) grid) == False
            else
                False

        hexTerrain =
            HexGrid.valueAt point grid

        hexagonShape =
            Collage.polygon (HexGrid.polygonCorners layout point)

        overlayForm =
            if isInViewOfBeast then
                Collage.filled (Color.rgba 0 0 0 0) hexagonShape
            else
                Collage.filled (Color.rgba 0 0 0 0.3) hexagonShape

        hexagonForm =
            case hexTerrain of
                Just terrain ->
                    case terrain of
                        Terrain.Earth ->
                            Collage.filled Color.lightBlue hexagonShape

                        Terrain.Sea ->
                            Collage.filled Color.blue hexagonShape

                        Terrain.Pasture _ ->
                            Collage.filled Color.lightBlue hexagonShape

                        Terrain.Mountain ->
                            Collage.filled Color.gray hexagonShape

                Nothing ->
                    Collage.filled Color.gray hexagonShape

        islandShape =
            Collage.oval 40 30

        islandForm =
            let
                form =
                    case hexTerrain of
                        Just terrain ->
                            case terrain of
                                Terrain.Earth ->
                                    Collage.filled Color.lightBrown islandShape

                                Terrain.Pasture _ ->
                                    Collage.filled Color.green islandShape

                                Terrain.Sea ->
                                    Collage.filled Color.blue islandShape

                                Terrain.Mountain ->
                                    Collage.filled Color.gray islandShape

                        Nothing ->
                            Collage.filled Color.gray islandShape
            in
                Collage.move (HexGrid.hexToPixel layout point) form

        neighbours =
            HexGrid.neighbors point

        directionToPolygon direction =
            let
                range =
                    case direction of
                        0 ->
                            List.range 5 6

                        1 ->
                            List.range 4 5

                        2 ->
                            List.range 3 4

                        3 ->
                            List.range 2 3

                        4 ->
                            List.range 1 2

                        _ ->
                            List.range 0 1

                getCorner : Int -> ( Float, Float )
                getCorner corner =
                    let
                        ( x, y ) =
                            HexGrid.hexCornerOffset layout (toFloat corner)

                        ( cX, cY ) =
                            HexGrid.hexToPixel layout point
                    in
                        ( x + cX, y + cY )
            in
                HexGrid.hexToPixel layout point :: (List.map getCorner range)

        neighbourToMaybeForm ( direction, maybeTerrain ) =
            case hexTerrain of
                Just reallyTerrain ->
                    case reallyTerrain of
                        Terrain.Earth ->
                            case maybeTerrain of
                                Just terrain ->
                                    case terrain of
                                        Terrain.Sea ->
                                            Nothing

                                        _ ->
                                            Just (Collage.filled Color.lightBrown (Collage.polygon (directionToPolygon direction)))

                                Nothing ->
                                    Nothing

                        Terrain.Pasture _ ->
                            case maybeTerrain of
                                Just terrain ->
                                    case terrain of
                                        Terrain.Sea ->
                                            Nothing

                                        _ ->
                                            Just (Collage.filled Color.green (Collage.polygon (directionToPolygon direction)))

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        directionPairs =
            List.indexedMap
                (\index point ->
                    ( index, HexGrid.valueAt point grid )
                )
                neighbours

        neighbourForms =
            List.filterMap
                neighbourToMaybeForm
                directionPairs

        numberForms =
            case hexTerrain of
                Just terrain ->
                    case terrain of
                        Terrain.Pasture grassiness ->
                            [ Text.fromString (toString grassiness) |> Collage.text |> Collage.move (HexGrid.hexToPixel layout point) |> Collage.moveY 10.0 ]

                        _ ->
                            []

                Nothing ->
                    []
    in
        hexagonForm :: (islandForm :: neighbourForms) ++ [ overlayForm ] ++ numberForms
