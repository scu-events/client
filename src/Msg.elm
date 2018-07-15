module Msg exposing (..)

import Http
import Date exposing (Date, day, month, year, fromString)
import Data.Event exposing (Event)
import Data.Major exposing (Major)
import Data.Organization exposing (Organization)


type Msg
    = AddMajor Major
    | RemoveMajor Major
    | UpdateCurrentMajor Major
    | Initialize Date
    | PopulateCalendar (List String)
    | ChangeCalendar Int
    | NewEvents (Result Http.Error (List Event))
    | ToggleFreeFood
    | ToggleVolunteer
    | AddOrganization Organization
    | RemoveOrganization Organization
    | UpdateCurrentOrganization Organization
    | ShowEvent Event
    | HideEvent
    | NoOp
