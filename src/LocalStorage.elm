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
import I18n exposing (Language, languageToString, stringToLanguage)
import Json.Decode as D
import Json.Encode as E
import Theme exposing (DarkOrLight, boolToDarkOrLight, darkModeToString)


port storeLocalStorageValue_ : E.Value -> Cmd msg


port receiveLocalStorage_ : (D.Value -> msg) -> Sub msg


port getLocalStorage_ : E.Value -> Cmd ms


type alias LocalStorage =
    { language : Language
    , darkMode : DarkOrLight
    }


type LocalStorageUpdate
    = LanguageUpdate Language
    | DarkModeUpdate DarkOrLight


storeValue : LocalStorageUpdate -> Command FrontendOnly toMsg msg
storeValue update =
    Command.sendToJs
        "storeLocalStorageValue_"
        storeLocalStorageValue_
        (E.object <|
            case update of
                LanguageUpdate lang ->
                    [ ( "key", E.string "language" )
                    , ( "value", E.string (languageToString lang) )
                    ]

                DarkModeUpdate darkMode ->
                    [ ( "key", E.string "darkMode" )
                    , ( "value", E.string (darkModeToString darkMode) )
                    ]
        )


receiveLocalStorage : (D.Value -> msg) -> Subscription FrontendOnly msg
receiveLocalStorage msg =
    Subscription.fromJs "receiveLocalStorage_" receiveLocalStorage_ msg


localStorageDecoder : D.Decoder LocalStorage
localStorageDecoder =
    D.map2
        (\lang dark ->
            { language = stringToLanguage <| Debug.log "language" lang
            , darkMode = boolToDarkOrLight <| Debug.log "darkMode" dark
            }
        )
        (D.field "language" D.string)
        (D.field "darkMode" D.bool)


getLocalStorage : Command FrontendOnly toMsg msg
getLocalStorage =
    Command.sendToJs "getLocalStorage_" getLocalStorage_ E.null
