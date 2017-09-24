module Main exposing (..)

import Collage
import Layout
import Element
import View
import Html
import Map
import Dict
import Mouse


-- MODEL


type alias Model =
    { mousePosition : { x : Int, y : Int }
    , mouseIsDown : Bool
    , map : Map.Map
    }


init : Model
init =
    { mousePosition = { x = 0, y = 0 }
    , mouseIsDown = False
    , map =
        Map.hexagonOfSea 5
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
              , map =
                    if model.mouseIsDown then
                        updateTerrain position model.map
                    else
                        model.map
              }
            , Cmd.none
            )

        DownMsg position ->
            ( { mousePosition = model.mousePosition
              , mouseIsDown = True
              , map = updateTerrain position model.map
              }
            , Cmd.none
            )

        UpMsg position ->
            ( { mousePosition = model.mousePosition
              , mouseIsDown = False
              , map = model.map
              }
            , Cmd.none
            )


updateTerrain : Mouse.Position -> Map.Map -> Map.Map
updateTerrain mousePosition map =
    let
        hash =
            Layout.pointToHex layout (remapPosition mousePosition) |> Map.hexToHash

        d =
            Debug.log "hash" hash
    in
        case Dict.get hash map of
            Just terrain ->
                Map.update map hash Map.Earth

            Nothing ->
                map



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
            400

        originY =
            400

        newX =
            toFloat x - originX

        newY =
            -(toFloat y - originY)

        d =
            Debug.log "remapped coords" ( newX, newY )
    in
        ( newX / 40, newY / 40 )


layout :
    { orientation : Layout.Orientation
    , origin : Layout.Point
    , size : Layout.Point
    }
layout =
    { orientation = Layout.pointyOrientation
    , size = ( 40.0, 40.0 )
    , origin = ( 0.0, 0.0 )
    }


view : Model -> Html.Html msg
view model =
    let
        seaWithLand =
            model.map |> Dict.toList
    in
        Collage.collage 800 800 (List.map (View.hexToForm layout) seaWithLand |> List.concat)
            |> Element.toHtml


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
