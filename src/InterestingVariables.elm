module InterestingVariables exposing (..)

import Terrain


cellHeight : number
cellHeight =
    20


cellWidth : number
cellWidth =
    25


mapSize : Int
mapSize =
    10


beastViewRange : number
beastViewRange =
    5


defaultLandAvailable : number
defaultLandAvailable =
    3


spec : Terrain.TerrainSpec
spec =
    { earth = 4
    , sea = 20
    , mountain = 1
    , pasture = 2
    }
