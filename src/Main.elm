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


-- MODEL


type alias Model =
    { mousePosition : { x : Int, y : Int }
    , mouseIsDown : Bool
    , grid : HexGrid Terrain
    , beast : Beast
    , landAvailable : Int
    }


freshGrid =
    HexGrid.empty 6 Terrain.Sea
        |> HexGrid.insert ( -3, -2 ) (Terrain.Pasture Terrain.Untouched)
        |> HexGrid.insert ( 2, 0 ) (Terrain.Pasture Terrain.Untouched)
        |> HexGrid.insert ( 4, 2 ) (Terrain.Pasture Terrain.Untouched)
        |> HexGrid.insert ( -3, 1 ) (Terrain.Pasture Terrain.Untouched)
        |> HexGrid.insert ( 0, 0 ) Terrain.Earth


init : Model
init =
    { mousePosition = { x = 0, y = 0 }
    , mouseIsDown = False
    , grid = freshGrid
    , beast =
        { location = HexGrid.toPoint 0 0
        }
    , landAvailable = 3
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
            ( { model
                | mousePosition = position
                , grid =
                    if model.mouseIsDown then
                        updateTerrain position model.grid
                    else
                        model.grid
              }
            , Cmd.none
            )

        DownMsg position ->
            ( { model
                | mouseIsDown = True
                , grid = updateTerrain position model.grid
                , landAvailable = updateLandAvailable model.landAvailable position model.grid
              }
            , Cmd.none
            )

        UpMsg position ->
            ( { model | mouseIsDown = False }
            , Cmd.none
            )

        Advance ->
            ( { model
                | beast = Beast.beastTowardsPasture model.beast model.grid
                , grid = Terrain.grazedGrid model.grid model.beast
              }
            , Cmd.none
            )

        Reset ->
            ( { model | grid = freshGrid, beast = { location = ( 0, 0 ) }, landAvailable = 3 }
            , Cmd.none
            )


updateTerrain : Mouse.Position -> HexGrid Terrain -> HexGrid Terrain
updateTerrain mousePosition grid =
    let
        point =
            HexGrid.pixelToHex layout (remapPosition mousePosition)
    in
        case HexGrid.valueAt point grid of
            Just terrain ->
                case terrain of
                    Terrain.Sea ->
                        HexGrid.insert point Terrain.Earth grid

                    _ ->
                        grid

            Nothing ->
                grid


updateLandAvailable : Int -> Mouse.Position -> HexGrid Terrain -> Int
updateLandAvailable landAvailable mousePosition grid =
    let
        point =
            HexGrid.pixelToHex layout (remapPosition mousePosition)
    in
        case HexGrid.valueAt point grid of
            Just terrain ->
                case terrain of
                    Terrain.Sea ->
                        landAvailable - 1

                    _ ->
                        landAvailable

            Nothing ->
                landAvailable



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
            500

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
            (List.map (View.hexToForm layout grid) (Dict.keys dict) |> List.concat)

        beastForm =
            View.beastToForm layout beast
    in
        Html.div []
            [ Collage.collage 1000 1000 (mapForms ++ [ beastForm ])
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
