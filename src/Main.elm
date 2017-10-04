module Main exposing (..)

import Collage
import Element
import View
import Html
import Html.Events
import Dict
import Mouse
import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Beast exposing (Beast)
import InterestingVariables


-- MODEL


type alias Model =
    { mousePosition : { x : Int, y : Int }
    , mouseIsDown : Bool
    , grid : HexGrid Terrain
    , beast : Beast
    , landAvailable : Int
    }


freshGrid : HexGrid Terrain
freshGrid =
    HexGrid.empty 6 Terrain.Sea
        |> HexGrid.insert ( -3, -2 ) (Terrain.Pasture 1)
        |> HexGrid.insert ( -4, 0 ) (Terrain.Pasture 2)
        |> HexGrid.insert ( 1, -2 ) (Terrain.Pasture 1)
        |> HexGrid.insert ( 2, 0 ) (Terrain.Pasture 2)
        |> HexGrid.insert ( 4, 2 ) (Terrain.Pasture 1)
        |> HexGrid.insert ( -3, 1 ) (Terrain.Pasture 3)
        |> HexGrid.insert ( 2, 4 ) (Terrain.Pasture 1)
        |> HexGrid.insert ( 0, -1 ) (Terrain.Mountain)
        |> HexGrid.insert ( -2, -2 ) (Terrain.Mountain)
        |> HexGrid.insert ( 0, 0 ) Terrain.Earth


init : Model
init =
    { mousePosition = { x = 0, y = 0 }
    , mouseIsDown = False
    , grid = freshGrid
    , beast =
        { location = HexGrid.toPoint 0 0
        }
    , landAvailable = InterestingVariables.defaultLandAvailable
    }



-- MESSAGES


type Msg
    = MoveMsg Mouse.Position
    | DownMsg Mouse.Position
    | UpMsg Mouse.Position
    | Advance
    | Reset



-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MoveMsg position ->
            let
                ( newGrid, newLandAvailable ) =
                    if model.mouseIsDown then
                        updateTerrain position model.grid model.landAvailable
                    else
                        ( model.grid, model.landAvailable )
            in
                ( { model
                    | mousePosition = position
                    , grid = newGrid
                    , landAvailable = newLandAvailable
                  }
                , Cmd.none
                )

        DownMsg position ->
            let
                ( newGrid, newLandAvailable ) =
                    updateTerrain position model.grid model.landAvailable
            in
                ( { model
                    | mouseIsDown = True
                    , grid = newGrid
                    , landAvailable = newLandAvailable
                  }
                , Cmd.none
                )

        UpMsg position ->
            ( { model | mouseIsDown = False }
            , Cmd.none
            )

        Advance ->
            let
                ( newGrid, newLandAvailable ) =
                    Terrain.grazedGrid model.grid model.landAvailable model.beast
            in
                ( { model
                    | beast = Beast.beastTowardsPasture model.beast model.grid
                    , grid = newGrid
                    , landAvailable = newLandAvailable
                  }
                , Cmd.none
                )

        Reset ->
            ( { model | grid = freshGrid, beast = { location = ( 0, 0 ) }, landAvailable = InterestingVariables.defaultLandAvailable }
            , Cmd.none
            )


updateTerrain : Mouse.Position -> HexGrid Terrain -> Int -> ( HexGrid Terrain, Int )
updateTerrain mousePosition grid landAvailable =
    case landAvailable <= 0 of
        True ->
            ( grid, 0 )

        False ->
            let
                point =
                    HexGrid.pixelToHex layout (remapPosition mousePosition)
            in
                case HexGrid.valueAt point grid of
                    Just terrain ->
                        case terrain of
                            Terrain.Sea ->
                                ( HexGrid.insert point Terrain.Earth grid, landAvailable - 1 )

                            _ ->
                                ( grid, landAvailable )

                    Nothing ->
                        ( grid, landAvailable )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MoveMsg, Mouse.downs DownMsg, Mouse.ups UpMsg ]



-- VIEW


remapPosition : Mouse.Position -> ( Float, Float )
remapPosition { x, y } =
    let
        originX =
            500

        originY =
            350

        newX =
            toFloat x - originX

        newY =
            -(toFloat y - originY)
    in
        ( newX, newY )


layout : HexGrid.Layout
layout =
    { orientation = HexGrid.PointyTop
    , screenX = 40
    , screenY = 30
    , originX = 0.0
    , originY = 0.0
    }


view : Model -> Html.Html Msg
view { grid, beast, landAvailable } =
    let
        (HexGrid.HexGrid a dict) =
            grid

        mapForms =
            (List.map (View.hexToForm layout grid beast) (Dict.keys dict) |> List.concat)

        beastForm =
            View.beastToForm layout beast
    in
        Html.div []
            [ Collage.collage 1000 700 (mapForms ++ [ beastForm ])
                |> Element.toHtml
            , Html.div [] [ Html.text ("Land available: " ++ toString landAvailable) ]
            , Html.button [ Html.Events.onClick Advance ] [ Html.text "Advance" ]
            , Html.button [ Html.Events.onClick Reset ] [ Html.text "Reset" ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
