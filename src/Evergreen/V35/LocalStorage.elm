module Evergreen.V35.LocalStorage exposing (..)

import Evergreen.V35.I18n
import Evergreen.V35.Theme


type alias LocalStorage =
    { language : Evergreen.V35.I18n.Language
    , userPreference : Evergreen.V35.Theme.UserPreference
    , systemMode : Evergreen.V35.Theme.Mode
    , soundEnabled : Bool
    }
