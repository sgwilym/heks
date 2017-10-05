module View exposing (..)

import Collage
import Text
import Color
import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Beast exposing (Beast)
import InterestingVariables
import Set
import Dict


beastToForm : HexGrid.Layout -> Beast -> Collage.Form
beastToForm layout beast =
    Collage.circle 5 |> Collage.filled Color.white |> Collage.move (HexGrid.hexToPixel layout beast.location)


gridToForms : HexGrid.Layout -> HexGrid Terrain -> Beast -> List Collage.Form
gridToForms layout grid beast =
    let
        (HexGrid.HexGrid a gridDict) =
            grid

        pointsInViewRange =
            HexGrid.range InterestingVariables.beastViewRange beast.location
                |> Set.fromList

        outOfViewPoints =
            HexGrid.fogOfWar beast.location (Terrain.pointsOfTerrains Terrain.Mountain grid |> Set.intersect pointsInViewRange) grid

        pointsWithInfo =
            Dict.map
                (\point terrain ->
                    { terrain = terrain
                    , isInView =
                        if HexGrid.distance beast.location point < InterestingVariables.beastViewRange then
                            Set.member point outOfViewPoints == False
                        else
                            False
                    , neighbours =
                        List.indexedMap
                            (\index point ->
                                ( index, HexGrid.valueAt point grid )
                            )
                            (HexGrid.neighbors
                                point
                            )
                            |> List.filterMap
                                (\( direction, maybeTerrain ) ->
                                    case maybeTerrain of
                                        Just terrain ->
                                            Just ( direction, terrain )

                                        Nothing ->
                                            Nothing
                                )
                    }
                )
                gridDict
    in
        Dict.toList pointsWithInfo
            |> List.map (pointToForm layout)
            |> List.concat


pointToForm : HexGrid.Layout -> ( HexGrid.Point, { terrain : Terrain, isInView : Bool, neighbours : List ( Int, Terrain ) } ) -> List Collage.Form
pointToForm layout ( point, terrainInfo ) =
    let
        hexagonShape =
            Collage.polygon (HexGrid.polygonCorners layout point)

        overlayForm =
            if terrainInfo.isInView then
                Collage.filled (Color.rgba 0 0 0 0) hexagonShape
            else
                Collage.filled (Color.rgba 0 0 0 0.5) hexagonShape

        hexagonForm =
            case terrainInfo.terrain of
                Terrain.Earth ->
                    Collage.filled Color.lightBlue hexagonShape

                Terrain.Sea ->
                    Collage.filled Color.blue hexagonShape

                Terrain.Pasture _ ->
                    Collage.filled Color.lightBlue hexagonShape

                Terrain.Mountain ->
                    Collage.filled Color.gray hexagonShape

        islandShape =
            Collage.oval (InterestingVariables.cellWidth * 0.6) (InterestingVariables.cellHeight * 0.6)

        islandForm =
            let
                form =
                    case terrainInfo.terrain of
                        Terrain.Earth ->
                            Collage.filled Color.lightBrown islandShape

                        Terrain.Pasture _ ->
                            Collage.filled Color.green islandShape

                        Terrain.Sea ->
                            Collage.filled Color.blue islandShape

                        Terrain.Mountain ->
                            Collage.filled Color.gray islandShape
            in
                Collage.move (HexGrid.hexToPixel layout point) form

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

        neighbourToForm ( direction, terrain ) =
            case terrainInfo.terrain of
                Terrain.Earth ->
                    case terrain of
                        Terrain.Sea ->
                            Nothing

                        _ ->
                            Just (Collage.filled Color.lightBrown (Collage.polygon (directionToPolygon direction)))

                Terrain.Pasture _ ->
                    case terrain of
                        Terrain.Sea ->
                            Nothing

                        _ ->
                            Just (Collage.filled Color.green (Collage.polygon (directionToPolygon direction)))

                _ ->
                    Nothing

        neighbourForms =
            List.filterMap
                neighbourToForm
                terrainInfo.neighbours

        numberForms =
            case terrainInfo.terrain of
                Terrain.Pasture grassiness ->
                    [ Text.fromString (toString grassiness) |> Collage.text |> Collage.move (HexGrid.hexToPixel layout point) |> Collage.moveY 10.0 ]

                _ ->
                    []
    in
        hexagonForm :: (islandForm :: neighbourForms) ++ [ overlayForm ] ++ numberForms
