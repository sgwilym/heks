module Main exposing (..)

import Collage
import Element
import View
import Html
import Html.Events
import Mouse
import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Puzzle exposing (Puzzle)
import Random
import Keyboard


-- MODEL


puzzleRadius : number
puzzleRadius =
    4


type alias Model =
    { puzzle : Puzzle
    , isModifierKeyDown : Bool
    }


init : Model
init =
    { puzzle =
        Puzzle.Puzzle (HexGrid.empty puzzleRadius Terrain.Sea) (HexGrid.empty puzzleRadius Puzzle.NoGuess)
    , isModifierKeyDown = False
    }



-- MESSAGES


type Msg
    = ClickMsg Mouse.Position
    | Generate
    | NewPuzzle (Puzzle.Puzzle)
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickMsg mousePosition ->
            ( { model
                | puzzle = toggleCell mousePosition model.isModifierKeyDown model.puzzle
              }
            , Cmd.none
            )

        KeyDown code ->
            if code == 18 then
                ( { model | isModifierKeyDown = True }, Cmd.none )
            else
                ( model, Cmd.none )

        KeyUp code ->
            if code == 18 then
                ( { model | isModifierKeyDown = False }, Cmd.none )
            else
                ( model, Cmd.none )

        Generate ->
            ( model, Random.generate NewPuzzle (Puzzle.randomPuzzle puzzleRadius) )

        NewPuzzle puzzle ->
            ( { model | puzzle = puzzle }, Cmd.none )


toggleCell : Mouse.Position -> Bool -> Puzzle.Puzzle -> Puzzle.Puzzle
toggleCell mousePosition modifier (Puzzle.Puzzle grid guesses) =
    let
        point =
            HexGrid.pixelToHex layout (remapPosition mousePosition)
    in
        case HexGrid.valueAt point grid of
            Just _ ->
                case HexGrid.valueAt point guesses of
                    Just guess ->
                        case guess of
                            Puzzle.NoGuess ->
                                if modifier then
                                    Puzzle.Puzzle grid (HexGrid.insert point Puzzle.Cross guesses)
                                else
                                    Puzzle.Puzzle grid (HexGrid.insert point Puzzle.Filled guesses)

                            _ ->
                                Puzzle.Puzzle grid (HexGrid.insert point Puzzle.NoGuess guesses)

                    Nothing ->
                        Puzzle.Puzzle grid guesses

            Nothing ->
                Puzzle.Puzzle grid guesses


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
        [ Mouse.clicks ClickMsg
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


view : Model -> Html.Html Msg
view { puzzle } =
    let
        (Puzzle.Puzzle grid guesses) =
            puzzle

        gridForms =
            View.gridToForms layout grid

        guessForms =
            View.guessesToForms layout guesses

        labelForms =
            View.gridToHintLabels layout grid guesses
    in
        Html.div []
            [ Collage.collage 1000 1000 (gridForms ++ guessForms ++ labelForms)
                |> Element.toHtml
            , Html.button
                [ Html.Events.onClick Generate ]
                [ Html.text "New puzzle!" ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
