module Evergreen.V27.LocalStorage exposing (..)

import Evergreen.V27.I18n
import Evergreen.V27.Theme


type alias LocalStorage =
    { language : Evergreen.V27.I18n.Language
    , userPreference : Evergreen.V27.Theme.UserPreference
    , systemMode : Evergreen.V27.Theme.Mode
    , soundEnabled : Bool
    }
