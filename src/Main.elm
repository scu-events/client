module Main exposing (..)

import Date exposing (Date, day, month, year, fromString)
import Task
import Ports
import Html
    exposing
        ( Html
        , text
        , div
        , h3
        , nav
        , a
        , span
        , i
        , button
        )
import Html.Attributes
    exposing
        ( class
        , href
        , disabled
        )
import Html.Events exposing (onClick)
import Http
import Data.Event exposing (Event, eventsDecoder)
import Data.Major exposing (Major)
import Data.Organization exposing (Organization)
import Views.Calendar exposing (calendarView)
import Views.Events exposing (eventsView)
import Views.Filter exposing (filterView)
import Msg exposing (..)
import Utils.Time exposing (toMonthString)


---- MODEL ----
-- majors, freeFood, volunteer are search filters


type alias Model =
    { majors : List Major
    , currentMajor : Major
    , majorOptions : List Major
    , events : List Event
    , dates : List (Maybe Date) -- annoying Maybe
    , now : Maybe Date
    , offset : Int
    , freeFood : Bool
    , volunteer : Bool
    , organizations : List Organization
    , currentOrganization : String
    , organizationOptions : List Organization
    , backendURL : String
    , modalEvent : Maybe Event
    , navbarToggle : Bool
    , featureToggle : Bool
    , majorsToggle : Bool
    , organizationsToggle : Bool
    , searchFilter : SearchFilter
    }


type alias Flags =
    { backendURL : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { majors = []
      , currentMajor = ""
      , majorOptions =
            [ "Philosophy"
            , "Computer Science"
            , "Math"
            , "Physics"
            , "Chemistry"
            , "Biology"
            , "Biochemistry"
            , "Mechanical Engineering"
            , "Aerospace"
            , "English"
            ]
      , events = []
      , dates = []
      , now = Nothing
      , offset = 0
      , freeFood = False
      , volunteer = False
      , organizations = []
      , currentOrganization = ""
      , organizationOptions =
            [ "APB"
            , "ASG"
            , "Into the Wild"
            , "KSCU"
            , "MCC"
            , "SCCAP"
            , "Santa Clara Review"
            , "The Redwood"
            , "The Santa Clara"
            , "Aerospace and Rocketry Club"
            , "American Society of Civil Engineers"
            , "American Society of Mechanical Engineers"
            , "Associated General Contactors"
            , "Association of Computing Machinery (ACM)"
            , "Association for Computing and Machinery - Women's Chapter (ACM-W)"
            , "Biomedical Engineering Society"
            , "Engineers Without Borders"
            , "Institute of Electrical and Electronics Engineers"
            , "Maker Club"
            , "National Society of Black Engineers"
            , "Santa Clara Innovation and Design"
            , "Santa Clara Theta Tau"
            , "Society of Women Engineers"
            ]
      , backendURL = flags.backendURL
      , modalEvent = Nothing
      , navbarToggle = True
      , featureToggle = False
      , majorsToggle = False
      , organizationsToggle = False
      , searchFilter = None
      }
    , Task.perform Initialize Date.now
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddMajor major ->
            ( { model
                | majors = model.majors ++ [ major ]
                , currentMajor = ""
                , majorOptions = model.majorOptions |> List.filter ((/=) major)
              }
            , Cmd.none
            )

        RemoveMajor major ->
            ( { model
                | majors = model.majors |> List.filter ((/=) major)
                , majorOptions = model.majorOptions ++ [ major ]
              }
            , Cmd.none
            )

        UpdateCurrentMajor major ->
            ( { model | currentMajor = major }, Cmd.none )

        Initialize date ->
            ( { model | now = Just date }
            , Ports.populateCalendar
                ([ year >> toString, month >> toString, day >> toString ]
                    |> List.map ((|>) date)
                    |> String.join " "
                )
            )

        PopulateCalendar res ->
            let
                dates =
                    res |> List.map ((<|) (fromString >> Result.toMaybe))
            in
                ( { model
                    | dates = dates
                  }
                , Http.send NewEvents
                    (Http.get
                        ([ model.backendURL ++ "/api/events/?month="
                         , (dates
                                |> List.drop 7
                                |> List.head
                                |> Maybe.withDefault model.now
                                |> toMonthString
                                |> String.toLower
                                |> String.split " "
                                |> String.join "&year="
                           )
                         ]
                            |> String.join ""
                        )
                        eventsDecoder
                    )
                )

        ChangeCalendar n ->
            ( { model | offset = model.offset + n }
            , Ports.repopulateCalendar
                ( ([ year >> toString, month >> toString, day >> toString ]
                    |> List.map
                        (\x -> model.now |> Maybe.map x |> Maybe.withDefault "")
                    |> String.join " "
                  )
                , model.offset + n
                )
            )

        NewEvents result ->
            case result of
                Ok data ->
                    ( { model | events = data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ToggleFreeFood ->
            ( { model | freeFood = not model.freeFood }, Cmd.none )

        ToggleVolunteer ->
            ( { model | volunteer = not model.volunteer }, Cmd.none )

        AddOrganization organization ->
            ( { model
                | organizations = model.organizations ++ [ organization ]
                , currentOrganization = ""
                , organizationOptions = model.organizationOptions |> List.filter ((/=) organization)
              }
            , Cmd.none
            )

        RemoveOrganization organization ->
            ( { model
                | organizations =
                    model.organizations
                        |> List.filter
                            ((/=)
                                organization
                            )
                , organizationOptions =
                    model.organizationOptions
                        ++ [ organization ]
              }
            , Cmd.none
            )

        UpdateCurrentOrganization organization ->
            ( { model | currentOrganization = organization }, Cmd.none )

        ShowEvent event ->
            ( { model | modalEvent = Just event }, Cmd.none )

        HideEvent ->
            ( { model | modalEvent = Nothing }, Cmd.none )

        ToggleNavbar ->
            ( { model | navbarToggle = not model.navbarToggle }, Cmd.none )

        ShowSearchFilter filter ->
            ( { model
                | searchFilter =
                    if model.searchFilter == filter then
                        None
                    else
                        filter
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ headerView model
        , div [ class "columns" ]
            [ div [ class "column is-offset-1 is-7" ] [ eventsView model.events ]
            , div [ class "column is-2" ] [ calendarView model.dates model.events model.modalEvent ]
            ]
        ]


headerView : Model -> Html Msg
headerView model =
    nav [ class "navbar is-fixed-top is-transparent" ]
        [ div [ class "navbar-brand" ]
            [ a [ href "#", class "navbar-item" ] [ text "SCU Events" ]
            , div
                [ class
                    ("navbar-burger burger "
                        ++ if not model.navbarToggle then
                            "is-active"
                           else
                            ""
                    )
                , onClick ToggleNavbar
                ]
                [ span []
                    []
                , span []
                    []
                , span []
                    []
                ]
            ]
        , div
            [ class
                ("navbar-menu "
                    ++ if not model.navbarToggle then
                        "is-active"
                       else
                        ""
                )
            ]
            [ div [ class "navbar-start" ]
                [ span [ class "navbar-item" ]
                    [ filterView
                        model.majors
                        model.currentMajor
                        model.majorOptions
                        model.organizations
                        model.currentOrganization
                        model.organizationOptions
                        model.searchFilter
                    ]
                ]
            , div
                [ class "navbar-end" ]
                [ span [ class "navbar-item" ]
                    [ button [ class "button is-info is-inverted", disabled True ]
                        [ span [ class "icon" ]
                            [ i [ class "fa fa-user" ] [] ]
                        , span [] [ text "Login" ]
                        ]
                    ]
                ]
            ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.onPopulateCalendar PopulateCalendar



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
