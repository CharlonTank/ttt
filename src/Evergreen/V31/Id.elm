module Evergreen.V31.Id exposing (..)

import UUID


type GameId
    = GameId Never


type Id a
    = Id UUID.UUID
