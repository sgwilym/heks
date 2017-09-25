module View exposing (..)

import Collage
import Layout
import Hex exposing (Hex)
import Color
import Map
import Dict


hexToForm : Layout.Layout -> Map.Map -> ( Int, Int ) -> List Collage.Form
hexToForm layout map ( q, r ) =
    let
        hex =
            Hex q r

        hexTerrain =
            Dict.get (Map.hexToHash hex) map

        hexagonShape =
            Collage.polygon (Layout.polygonCorners layout hex)

        hexagonForm =
            case hexTerrain of
                Just terrain ->
                    case terrain of
                        Map.Earth ->
                            Collage.filled Color.lightBlue hexagonShape

                        Map.Sea ->
                            Collage.filled Color.blue hexagonShape

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
                                Map.Earth ->
                                    Collage.filled Color.lightGreen islandShape

                                Map.Sea ->
                                    Collage.filled Color.blue islandShape

                        Nothing ->
                            Collage.filled Color.gray islandShape
            in
                Collage.move (Layout.hexToPoint layout hex) form

        neighbours =
            Map.allNeighboursTerrain map hex

        directionToPolygon direction =
            let
                range =
                    case direction of
                        Hex.One ->
                            List.range 5 6

                        Hex.Two ->
                            List.range 4 5

                        Hex.Three ->
                            List.range 3 4

                        Hex.Four ->
                            List.range 2 3

                        Hex.Five ->
                            List.range 1 2

                        Hex.Six ->
                            List.range 0 1

                getCorner corner =
                    let
                        ( x, y ) =
                            Layout.hexCornerOffset layout corner

                        ( cX, cY ) =
                            Layout.hexToPoint layout hex
                    in
                        ( x + cX, y + cY )
            in
                Layout.hexToPoint layout hex :: (List.map getCorner range)

        neighbourToMaybeForm direction maybeTerrain =
            case hexTerrain of
                Just reallyTerrain ->
                    case reallyTerrain of
                        Map.Earth ->
                            case maybeTerrain of
                                Just terrain ->
                                    case terrain of
                                        Map.Earth ->
                                            Just (Collage.filled Color.lightGreen (Collage.polygon (directionToPolygon direction)))

                                        _ ->
                                            Nothing

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        directionPairs =
            [ ( Hex.One, neighbours.one )
            , ( Hex.Two, neighbours.two )
            , ( Hex.Three, neighbours.three )
            , ( Hex.Four, neighbours.four )
            , ( Hex.Five, neighbours.five )
            , ( Hex.Six, neighbours.six )
            ]

        neighbourForms =
            List.filterMap
                (\( direction, maybeTerrain ) -> neighbourToMaybeForm direction maybeTerrain)
                directionPairs
    in
        hexagonForm :: (islandForm :: neighbourForms)
