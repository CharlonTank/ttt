module Id exposing
    ( GameId(..)
    , Id(..)
    , UserId(..)
    )

import UUID exposing (UUID)


type GameId
    = GameId Never


type Id a
    = Id UUID


type UserId
    = UserId Never
