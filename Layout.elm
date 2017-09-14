module Layout exposing (..)

import Hex exposing (Hex)


type alias Point =
    ( Float, Float )


type alias TwoByTwoMatrix =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    }


type alias Orientation =
    { forwardMatrix : TwoByTwoMatrix
    , inverseMatrix : TwoByTwoMatrix
    , startAngle : Float
    }


type alias Layout =
    { orientation : Orientation
    , size : Point
    , origin : Point
    }



-- Orientations


pointyOrientation : Orientation
pointyOrientation =
    { forwardMatrix = TwoByTwoMatrix (sqrt 3.0) (sqrt 3.0 / 2.0) 0.0 (3.0 / 2.0)
    , inverseMatrix = TwoByTwoMatrix ((sqrt 3.0) / 3.0) (-1.0 / 3.0) 0.0 (2.0 / 3.0)
    , startAngle = 0.5
    }


flatOrientation : Orientation
flatOrientation =
    { forwardMatrix = TwoByTwoMatrix (3.0 / 2.0) 0.0 ((sqrt 3.0) / 2.0) (sqrt 3.0)
    , inverseMatrix = TwoByTwoMatrix (2.0 / 3.0) 0.0 (-1.0 / 3.0) ((sqrt 3.0) / 3.0)
    , startAngle = 0.0
    }



-- Conversions


hexToPoint : Layout -> Hex -> Point
hexToPoint { orientation, size, origin } hex =
    let
        ( sizeX, sizeY ) =
            size

        ( originX, originY ) =
            origin

        { forwardMatrix } =
            orientation

        x =
            (forwardMatrix.a * (toFloat hex.q) + forwardMatrix.b * (toFloat hex.r)) * sizeX

        y =
            (forwardMatrix.c * (toFloat hex.q) + forwardMatrix.d * (toFloat hex.r)) * sizeY
    in
        ( x + originX, y + originY )


pointToHex : Layout -> Point -> Hex
pointToHex { orientation, size, origin } ( pointX, pointY ) =
    let
        ( sizeX, sizeY ) =
            size

        ( originX, originY ) =
            origin

        { inverseMatrix } =
            orientation

        pt =
            ( (pointX - originX) / sizeX, (pointY - originY) / sizeY )

        q =
            inverseMatrix.a * pointX + inverseMatrix.b * pointY

        r =
            inverseMatrix.c * pointX + inverseMatrix.d * pointY
    in
        -- We need FractionalHex really
        Hex (round q) (round r)



-- Drawing a hex


hexCornerOffset : Layout -> Int -> Point
hexCornerOffset { orientation, size } corner =
    let
        ( sizeX, sizeY ) =
            size

        angle =
            2.0 * pi * (orientation.startAngle + (toFloat corner)) / 6
    in
        ( sizeX * cos angle, sizeY * sin angle )


polygonCorners : Layout -> Hex -> List Point
polygonCorners layout hex =
    let
        ( centreX, centreY ) =
            hexToPoint layout hex

        offsetHex ( x, y ) ( x_, y_ ) =
            ( x + x_, y + y_ )
    in
        List.map (offsetHex ( centreX, centreY )) <|
            List.map (hexCornerOffset layout) (List.range 0 5)
