module Tests exposing (..)

import Backend
import Effect.Browser.Dom as Dom
import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http exposing (Response(..))
import Effect.Lamdera
import Effect.Subscription as Subscription
import Effect.Test exposing (FileUpload(..), HttpResponse(..), MultipleFilesUpload(..))
import Frontend
import Html
import Time
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Url


unsafeUrl =
    case Url.fromString "https://my-test.app" of
        Just url ->
            url

        Nothing ->
            Debug.todo "Invalid url"


config : Effect.Test.Config ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
config =
    { frontendApp = Frontend.app_
    , backendApp = Backend.app_
    , handleHttpRequest = always NetworkErrorResponse
    , handlePortToJs = always Nothing
    , handleFileUpload = always CancelFileUpload
    , handleMultipleFilesUpload = always CancelMultipleFilesUpload
    , domain = unsafeUrl
    }


sessionId0 : Effect.Lamdera.SessionId
sessionId0 =
    Effect.Lamdera.sessionIdFromString "sessionId0"


test =
    Effect.Test.start
        "A test"
        (Time.millisToPosix 100)
        config
        [ Effect.Test.connectFrontend
            0
            sessionId0
            "/"
            { width = 1000, height = 800 }
            (\client ->
                [ client.click 100 (Dom.id "gameWithFriend")
                , client.click 100 (Frontend.cellId 4 4)
                , client.click 100 (Frontend.cellId 4 5)
                , client.click 100 (Frontend.cellId 5 1)
                ]
            )
        ]


main : Program () (Effect.Test.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (Effect.Test.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    Effect.Test.viewer [ test ]
