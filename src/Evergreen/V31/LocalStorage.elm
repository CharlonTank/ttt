module Evergreen.V31.LocalStorage exposing (..)

import Evergreen.V31.I18n
import Evergreen.V31.Theme


type alias LocalStorage =
    { language : Evergreen.V31.I18n.Language
    , userPreference : Evergreen.V31.Theme.UserPreference
    , systemMode : Evergreen.V31.Theme.Mode
    , soundEnabled : Bool
    }
