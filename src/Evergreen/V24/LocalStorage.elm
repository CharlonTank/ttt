module Evergreen.V24.LocalStorage exposing (..)

import Evergreen.V24.I18n
import Evergreen.V24.Theme


type alias LocalStorage =
    { language : Evergreen.V24.I18n.Language
    , userPreference : Evergreen.V24.Theme.UserPreference
    , systemMode : Evergreen.V24.Theme.Mode
    , soundEnabled : Bool
    }
