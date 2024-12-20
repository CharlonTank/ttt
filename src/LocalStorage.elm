port module LocalStorage exposing
    ( LocalStorage
    , LocalStorageUpdate(..)
    , getLocalStorage
    , localStorageDecoder
    , receiveLocalStorage
    , storeValue
    )

import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Subscription as Subscription exposing (Subscription)
import I18n exposing (Language(..), languageToString, stringToLanguage)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Lamdera.Json as Json
import Theme exposing (..)


port storeLocalStorageValue_ : Json.Value -> Cmd msg


port receiveLocalStorage_ : (Json.Value -> msg) -> Sub msg


port getLocalStorage_ : Json.Value -> Cmd ms


type alias LocalStorage =
    { language : Language
    , userPreference : UserPreference
    , systemMode : Mode
    }


type LocalStorageUpdate
    = LanguageUpdate Language
    | ThemePreferenceUpdate UserPreference


storeValue : LocalStorageUpdate -> Command FrontendOnly toMsg msg
storeValue update =
    Command.sendToJs "storeLocalStorageValue_"
        storeLocalStorageValue_
        (E.object <|
            case update of
                LanguageUpdate lang ->
                    [ ( "key", E.string "language" )
                    , ( "value", E.string (languageToString (Just lang)) )
                    ]

                ThemePreferenceUpdate preference ->
                    [ ( "key", E.string "darkMode" )
                    , ( "value"
                      , E.string
                            (case preference of
                                DarkMode ->
                                    "dark"

                                LightMode ->
                                    "light"

                                SystemMode ->
                                    "system"
                            )
                      )
                    ]
        )


receiveLocalStorage : (LocalStorage -> msg) -> Subscription FrontendOnly msg
receiveLocalStorage msg =
    Subscription.fromJs "receiveLocalStorage_"
        receiveLocalStorage_
        (Json.decodeValue (D.field "localStorage" localStorageDecoder)
            >> Result.withDefault defaultLocalStorage
            >> msg
        )


defaultLocalStorage : LocalStorage
defaultLocalStorage =
    { language = EN
    , userPreference = SystemMode -- Start with system mode by default
    , systemMode = Light
    }


localStorageDecoder : Json.Decoder LocalStorage
localStorageDecoder =
    D.succeed LocalStorage
        |> D.required "language" (D.string |> D.map stringToLanguage)
        |> D.required "userPreference" (D.string |> D.map stringToUserPreference)
        |> D.required "systemMode" (D.string |> D.map stringToMode)


stringToMode : String -> Mode
stringToMode str =
    case str of
        "dark" ->
            Dark

        _ ->
            Light


getLocalStorage : Command FrontendOnly toMsg msg
getLocalStorage =
    Command.sendToJs "getLocalStorage_" getLocalStorage_ E.null
