module Id exposing
    ( GameId(..)
    , Id(..)
    , UserId(..)
    , display4CharsFromSessionId
    , display4CharsFromUUID
    )

import Effect.Lamdera exposing (SessionId)
import UUID exposing (UUID)


type GameId
    = GameId Never


type Id a
    = Id UUID


type UserId
    = UserId Never


display4CharsFromSessionId : SessionId -> String
display4CharsFromSessionId =
    String.left 4 << Effect.Lamdera.sessionIdToString


display4CharsFromUUID : UUID -> String
display4CharsFromUUID uuid =
    String.left 4 (UUID.toString uuid)
