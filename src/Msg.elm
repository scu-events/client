module Msg exposing (..)

import Http
import Time exposing (Posix)
import Data.Event exposing (Event)
import Data.Major as Major
import Data.Organization as Organization
import Data.Feature exposing (Feature)


type SearchFilter
    = FeatureFilter
    | OrganizationFilter
    | MajorFilter
    | None


type Msg
    = Initialize Posix
    | PopulateCalendar (List String)
    | ChangeCalendar Int
    | NewEvents (Result Http.Error (List Event))
    | ShowEvent Event
    | HideEvent
    | ToggleNavbar
    | ShowSearchFilter SearchFilter
    | ToggleFeature Feature
    | OrganizationMsg Organization.Msg
    | MajorMsg Major.Msg
    | ToggleMainView
    | NoOp
