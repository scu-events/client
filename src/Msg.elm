module Msg exposing (..)

import Http
import Date exposing (Date, day, month, year, fromString)
import Data.Event exposing (Event)
import Data.Major exposing (Major)
import Data.Organization exposing (Organization)
import Data.Feature exposing (Feature)


type SearchFilter
    = FeatureFilter
    | OrganizationFilter
    | MajorFilter
    | None


type Msg
    = AddMajor Major
    | RemoveMajor Major
    | UpdateCurrentMajor Major
    | Initialize Date
    | PopulateCalendar (List String)
    | ChangeCalendar Int
    | NewEvents (Result Http.Error (List Event))
    | AddOrganization Organization
    | RemoveOrganization Organization
    | UpdateCurrentOrganization Organization
    | ShowEvent Event
    | HideEvent
    | ToggleNavbar
    | ShowSearchFilter SearchFilter
    | ToggleFeature Feature
    | NoOp
