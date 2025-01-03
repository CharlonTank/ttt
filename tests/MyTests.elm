module MyTests exposing (..)

import Duration
import Expect
import Test exposing (..)
import Time
-- import TimeExtra
-- import UserConfig


hour =
    60 * 60 * 1000


tests =
    describe "Time extra tests"
        [
        --      test "5 hours" <|
        --     \_ ->
        --         UserConfig.diffToStringEnglish (Time.millisToPosix 0) (Time.millisToPosix (5 * hour))
        --             |> Expect.equal "5\u{00A0}hours"
        -- , test "4.9 hours" <|
        --     \_ ->
        --         Duration.hours 4.9
        --             |> Duration.inMilliseconds
        --             |> round
        --             |> Time.millisToPosix
        --             |> UserConfig.diffToStringEnglish (Time.millisToPosix 0)
        --             |> Expect.equal "4.9\u{00A0}hours"
        -- , test "5 hours ago" <|
        --     \_ ->
        --         UserConfig.diffToStringEnglish (Time.millisToPosix (5 * hour)) (Time.millisToPosix 0)
        --             |> Expect.equal "5\u{00A0}hours ago"
        -- , test "1 day ago" <|
        --     \_ ->
        --         UserConfig.diffToStringEnglish (Time.millisToPosix (24 * hour)) (Time.millisToPosix 0)
        --             |> Expect.equal "1\u{00A0}day ago"
        -- , test "1 day" <|
        --     \_ ->
        --         UserConfig.diffToStringEnglish (Time.millisToPosix 0) (Time.millisToPosix (24 * hour))
        --             |> Expect.equal "1\u{00A0}day"
        -- , test "2 days" <|
        --     \_ ->
        --         UserConfig.diffToStringEnglish (Time.millisToPosix 0) (Time.millisToPosix (36 * hour))
        --             |> Expect.equal "2\u{00A0}days"
        -- , test "1 day close to rounding point" <|
        --     \_ ->
        --         UserConfig.diffToStringEnglish (Time.millisToPosix 0) (Time.millisToPosix (35 * hour))
        --             |> Expect.equal "1\u{00A0}day"
        -- , test "2 days ago" <|
        --     \_ ->
        --         UserConfig.diffToStringEnglish (Time.millisToPosix (36 * hour)) (Time.millisToPosix 0)
        --             |> Expect.equal "2\u{00A0}days ago"
        -- , test "Don't remove last 0" <|
        --     \_ -> TimeExtra.removeTrailing0s 2 0 |> Expect.equal "0"
        ]




-- module MyTests exposing (tests)

-- import Backend
-- import Bytes exposing (Bytes)
-- import Dict exposing (Dict)
-- import Effect.Browser.Dom as Dom
-- import Effect.Lamdera
-- import Effect.Test as T exposing (FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..), PointerOptions(..))
-- import Frontend
-- import Json.Decode
-- import Json.Encode
-- import Test.Html.Query
-- import Test.Html.Selector as Selector
-- import Time
-- import Types exposing (ToBackend, FrontendMsg, FrontendModel, ToFrontend, BackendMsg, BackendModel)
-- import Url exposing (Url)


-- setup : T.ViewerWith (List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel))
-- setup =
--     T.viewerWith tests
--         |> T.addBytesFiles (Dict.values httpRequests)


-- main : Program () (T.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (T.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
-- main =
--     T.startViewer setup


-- domain : Url
-- domain =
--     { protocol = Url.Http, host = "localhost", port_ = Just 8000, path = "", query = Nothing, fragment = Nothing }


-- stringToJson : String -> Json.Encode.Value
-- stringToJson json =
--     Result.withDefault Json.Encode.null (Json.Decode.decodeString Json.Decode.value json)


-- handlePortToJs : { currentRequest : T.PortToJs, data : T.Data FrontendModel BackendModel } -> Maybe ( String, Json.Decode.Value )
-- handlePortToJs { currentRequest } =
--     Dict.get currentRequest.portName portRequests


-- {-| Please don't modify or rename this function -}
-- portRequests : Dict String (String, Json.Encode.Value)
-- portRequests =
--     [ ( "getLocalStorageValue_", ( "receiveLocalStorageValue_", stringToJson """{"key":"darkMode","value":""}""" ) )
--     ]
--         |> Dict.fromList


-- {-| Please don't modify or rename this function -}
-- httpRequests : Dict String String
-- httpRequests =
--     [ 
--     ]
--         |> Dict.fromList


-- handleHttpRequests : Dict String Bytes -> { currentRequest : HttpRequest, data : T.Data FrontendModel BackendModel } -> HttpResponse
-- handleHttpRequests httpData { currentRequest } =
--     case Dict.get (currentRequest.method ++ "_" ++ currentRequest.url) httpRequests of
--         Just filepath ->
--             case Dict.get filepath httpData of
--                 Just data ->
--                     BytesHttpResponse { url = currentRequest.url, statusCode = 200, statusText = "OK", headers = Dict.empty } data

--                 Nothing ->
--                     UnhandledHttpRequest

--         Nothing ->
--             UnhandledHttpRequest


-- {-| You can change parts of this function represented with `...`.
-- The rest needs to remain unchanged in order for the test generator to be able to add new tests.

--     tests : ... -> List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
--     tests ... =
--         let
--             config = ...

--             ...
--         in
--         [ ...
--         ]
-- -}
-- tests : Dict String Bytes -> List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
-- tests httpData =
--     let
--         config =
--             T.Config
--                 Frontend.app_
--                 Backend.app_
--                 (handleHttpRequests httpData)
--                 handlePortToJs
--                 (\_ -> UnhandledFileUpload)
--                 (\_ -> UnhandledMultiFileUpload)
--                 domain
--     in
--     [ T.start
--         "test0"
--         (Time.millisToPosix 1734522091492)
--         config
--         [ T.connectFrontend
--             0
--             (Effect.Lamdera.sessionIdFromString "189691c04b8f7b594cdeedebc2a8029b82943b0a")
--             "http://localhost:8000/"
--             { width = 756, height = 420 }
--             (\tab1 ->
--                 [ T.connectFrontend
--                     26
--                     (Effect.Lamdera.sessionIdFromString "745478c04b8f7b594cdeedebc2a8029b82943b0a")
--                     "http://localhost:8000/"
--                     { width = 756, height = 420 }
--                     (\tab2 ->
--                         [ tab1.click 12343 (Dom.id "cell_4_5")
--                         , tab2.click 3856 (Dom.id "cell_5_4")
--                         , tab1.click 2875 (Dom.id "cell_4_2")
--                         , tab2.click 2758 (Dom.id "cell_2_7")
--                         , tab1.click 1882 (Dom.id "cell_7_1")
--                         , tab2.click 1468 (Dom.id "cell_1_7")
--                         , tab1.click 2233 (Dom.id "cell_7_2")
--                         , tab2.click 1542 (Dom.id "cell_2_4")
--                         , tab1.click 3432 (Dom.id "cell_4_8")
--                         , tab2.click 2126 (Dom.id "cell_8_2")
--                         , tab1.click 1849 (Dom.id "cell_2_1")
--                         , tab2.click 2734 (Dom.id "cell_1_5")
--                         , tab1.click 1911 (Dom.id "cell_5_0")
--                         , tab2.click 1423 (Dom.id "cell_0_8")
--                         , tab1.click 2168 (Dom.id "cell_8_0")
--                         , tab2.click 1549 (Dom.id "cell_0_4")
--                         , tab1.click 3950 (Dom.id "cell_8_4")
--                         , tab2.click 3758 (Dom.id "cell_8_8")
--                         , tab1.click 6293 (Dom.id "cell_8_6")
--                         , tab2.click 1940 (Dom.id "cell_6_4")
--                         , tab2.click 4090 (Dom.id "back-to-menu-button")
--                         , tab1.click 1382 (Dom.id "back-to-menu-button")
--                         ]
--                     )
--                 ]
--             )
--         ]
    
--     ]