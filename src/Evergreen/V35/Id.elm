module Evergreen.V35.Id exposing (..)

import UUID


type UserId
    = UserId Never


type Id a
    = Id UUID.UUID


type GameId
    = GameId Never
