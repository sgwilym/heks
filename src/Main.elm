module Main exposing (..)

import Collage
import Element
import View
import Html
import Dict
import Mouse
import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)


-- MODEL


type alias Model =
    { mousePosition : { x : Int, y : Int }
    , mouseIsDown : Bool
    , grid : HexGrid Terrain
    }


init : Model
init =
    { mousePosition = { x = 0, y = 0 }
    , mouseIsDown = False
    , grid =
        HexGrid.empty 6 Terrain.Sea
    }



-- MESSAGES


type Msg
    = MoveMsg Mouse.Position
    | DownMsg Mouse.Position
    | UpMsg Mouse.Position



-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MoveMsg position ->
            ( { mousePosition = position
              , mouseIsDown = model.mouseIsDown
              , grid =
                    if model.mouseIsDown then
                        updateTerrain position model.grid
                    else
                        model.grid
              }
            , Cmd.none
            )

        DownMsg position ->
            ( { mousePosition = model.mousePosition
              , mouseIsDown = True
              , grid = updateTerrain position model.grid
              }
            , Cmd.none
            )

        UpMsg position ->
            ( { mousePosition = model.mousePosition
              , mouseIsDown = False
              , grid = model.grid
              }
            , Cmd.none
            )


updateTerrain : Mouse.Position -> HexGrid Terrain -> HexGrid Terrain
updateTerrain mousePosition grid =
    let
        point =
            HexGrid.pixelToHex layout (remapPosition mousePosition)

        d =
            Debug.log "point" point
    in
        case HexGrid.valueAt point grid of
            Just terrain ->
                HexGrid.insert point Terrain.Earth grid

            Nothing ->
                grid



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
    , screenY = 40
    , originX = 0.0
    , originY = 0.0
    }


view : Model -> Html.Html msg
view { grid } =
    let
        (HexGrid.HexGrid a dict) =
            grid

        hexForms =
            (List.map (View.hexToForm layout grid) (Dict.keys dict) |> List.concat)

        labelForms =
            View.gridToHintLabels layout grid
    in
        Collage.collage 1000 1000 (hexForms ++ labelForms)
            |> Element.toHtml


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
