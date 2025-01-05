module Evergreen.V32.LocalStorage exposing (..)

import Evergreen.V32.I18n
import Evergreen.V32.Theme


type alias LocalStorage =
    { language : Evergreen.V32.I18n.Language
    , userPreference : Evergreen.V32.Theme.UserPreference
    , systemMode : Evergreen.V32.Theme.Mode
    , soundEnabled : Bool
    }
