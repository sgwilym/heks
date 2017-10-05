module InterestingVariables exposing (..)

import Terrain


beastViewRange : number
beastViewRange =
    5


defaultLandAvailable : number
defaultLandAvailable =
    5


spec : Terrain.TerrainSpec
spec =
    { earth = 2
    , sea = 10
    , mountain = 1
    , pasture = 1
    }
