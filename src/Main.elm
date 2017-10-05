module Main exposing (..)

import Collage
import Element
import View
import Html
import Html.Events
import Mouse
import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Beast exposing (Beast)
import InterestingVariables
import Time
import Random
import Keyboard


-- MODEL


type alias Model =
    { mousePosition : { x : Int, y : Int }
    , mouseIsDown : Bool
    , grid : HexGrid Terrain
    , beast : Beast
    , landAvailable : Int
    , modifier : Bool
    }


freshGrid : HexGrid Terrain
freshGrid =
    HexGrid.empty InterestingVariables.mapSize Terrain.Sea


init : Model
init =
    { mousePosition = { x = 0, y = 0 }
    , mouseIsDown = False
    , grid = freshGrid
    , beast =
        { location = HexGrid.toPoint 0 0
        }
    , landAvailable = InterestingVariables.defaultLandAvailable
    , modifier = False
    }



-- MESSAGES


type Msg
    = MoveMsg Mouse.Position
    | DownMsg Mouse.Position
    | UpMsg Mouse.Position
    | Advance
    | Reset
    | Generate
    | NewGrid (HexGrid Terrain)
    | KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveMsg position ->
            let
                ( newGrid, newLandAvailable ) =
                    if model.mouseIsDown then
                        updateTerrain position model.grid model.landAvailable model.modifier
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
                    updateTerrain position model.grid model.landAvailable model.modifier
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

        Generate ->
            ( model, Random.generate NewGrid (Terrain.randomGrid InterestingVariables.spec InterestingVariables.mapSize) )

        NewGrid newGrid ->
            ( { model | grid = newGrid, beast = { location = ( 0, 0 ) }, landAvailable = InterestingVariables.defaultLandAvailable }, Cmd.none )

        KeyDown code ->
            if code == 18 then
                ( { model | modifier = True }, Cmd.none )
            else
                ( model, Cmd.none )

        KeyUp code ->
            if code == 18 then
                ( { model | modifier = False }, Cmd.none )
            else
                ( model, Cmd.none )


updateTerrain : Mouse.Position -> HexGrid Terrain -> Int -> Bool -> ( HexGrid Terrain, Int )
updateTerrain mousePosition grid landAvailable modifier =
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
                                if modifier then
                                    ( grid, landAvailable )
                                else
                                    ( HexGrid.insert point Terrain.Earth grid, landAvailable - 1 )

                            Terrain.Pasture grassAmount ->
                                ( grid, landAvailable )

                            _ ->
                                if modifier then
                                    ( HexGrid.insert point Terrain.Sea grid, landAvailable + 1 )
                                else
                                    ( grid, landAvailable )

                    Nothing ->
                        ( grid, landAvailable )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MoveMsg
        , Mouse.downs DownMsg
        , Mouse.ups UpMsg
        , Time.every Time.second (\_ -> Advance)
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]



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
    , screenX = InterestingVariables.cellWidth
    , screenY = InterestingVariables.cellHeight
    , originX = 0.0
    , originY = 0.0
    }


view : Model -> Html.Html Msg
view { grid, beast, landAvailable } =
    let
        mapForms =
            View.gridToForms layout grid beast

        beastForm =
            View.beastToForm layout beast
    in
        Html.div []
            [ Collage.collage 1000 700 (mapForms ++ [ beastForm ])
                |> Element.toHtml
            , Html.div [] [ Html.text ("Land available: " ++ toString landAvailable) ]
            , Html.button [ Html.Events.onClick Generate ] [ Html.text "Generate" ]
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
