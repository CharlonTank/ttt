module Id exposing
    ( GameId(..)
    , Id(..)
    , UserId(..)
    , display4CharsFromClientId
    , display4CharsFromId
    , display4CharsFromSessionId
    , display4CharsFromUUID
    )

import Effect.Lamdera exposing (ClientId, SessionId)
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


display4CharsFromClientId : ClientId -> String
display4CharsFromClientId =
    String.left 4 << Effect.Lamdera.clientIdToString


display4CharsFromUUID : UUID -> String
display4CharsFromUUID uuid =
    String.left 4 (UUID.toString uuid)


display4CharsFromId : Id a -> String
display4CharsFromId (Id uuid) =
    display4CharsFromUUID uuid
