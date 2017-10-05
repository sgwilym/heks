module InterestingVariables exposing (..)

import Terrain


cellHeight : number
cellHeight =
    20


cellWidth : number
cellWidth =
    25


beastViewRange : number
beastViewRange =
    5


defaultLandAvailable : number
defaultLandAvailable =
    3


spec : Terrain.TerrainSpec
spec =
    { earth = 5
    , sea = 2
    , mountain = 10
    , pasture = 2
    }
