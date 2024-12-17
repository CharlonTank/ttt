module Tests exposing (..)

import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http exposing (Response(..))
import Effect.Subscription as Subscription
import Effect.Test exposing (FileUpload(..), HttpResponse(..), MultipleFilesUpload(..))
import Html
import Time
import Url


frontendApp : Effect.Test.FrontendApp {} {} {} {}
frontendApp =
    { init = \_ _ -> ( {}, Command.none )
    , onUrlRequest = \_ -> {}
    , onUrlChange = \_ -> {}
    , update = \_ _ -> ( {}, Command.none )
    , updateFromBackend = \_ _ -> ( {}, Command.none )
    , view = \_ -> { title = "Hi", body = [ Html.text "Hi" ] }
    , subscriptions = \_ -> Subscription.none
    }


backendApp : Effect.Test.BackendApp {} {} {} {}
backendApp =
    { init = ( {}, Command.none )
    , update = \_ _ -> ( {}, Command.none )
    , updateFromFrontend = \_ _ _ _ -> ( {}, Command.none )
    , subscriptions = \_ -> Subscription.none
    }


unsafeUrl =
    case Url.fromString "https://my-test.app" of
        Just url ->
            url

        Nothing ->
            Debug.todo "Invalid url"


config : Effect.Test.Config {} {} {} {} {} {}
config =
    { frontendApp = frontendApp
    , backendApp = backendApp
    , handleHttpRequest = always NetworkErrorResponse
    , handlePortToJs = always Nothing
    , handleFileUpload = always CancelFileUpload
    , handleMultipleFilesUpload = always CancelMultipleFilesUpload
    , domain = unsafeUrl
    }


test =
    Effect.Test.start
        "A test"
        (Time.millisToPosix 0)
        config
        []


main : Program () (Effect.Test.Model {} {} {} {} {} {}) (Effect.Test.Msg {} {} {} {} {} {})
main =
    Effect.Test.viewer [ test ]
