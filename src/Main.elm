module Main exposing (..)

import Task
import Ports
import Browser
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
        , ul
        )
import Html.Attributes
    exposing
        ( class
        , href
        , disabled
        )
import Html.Events exposing (onClick)
import Http
import Time exposing (Posix, utc, now, toDay, toMonth, toYear)
import Iso8601 exposing (toTime)
import Data.Event exposing (Event, eventsDecoder)
import Data.Major as Major
import Data.Organization as Organization
import Data.Feature exposing (Feature)
import Views.Calendar exposing (calendarView)
import Views.Events exposing (eventsView)
import Views.Filter exposing (filterView)
import Msg exposing (..)
import Utils.Time exposing (toMonthString, toFirstThreeCharMonth)


---- MODEL ----


type DataProcess
    = Loading
    | Loaded


type MainView
    = Calendar
    | Events


type alias Model =
    { majorModel : Major.Model
    , events : List Event
    , dates : List (Maybe Posix) -- annoying Maybe
    , now : Maybe Posix
    , offset : Int
    , organizationModel : Organization.Model
    , backendURL : String
    , modalEvent : Maybe Event
    , navbarToggle : Bool
    , featureToggle : Bool
    , majorsToggle : Bool
    , organizationsToggle : Bool
    , searchFilter : SearchFilter
    , error : String
    , features : List Feature
    , selectedFeatures : List Feature
    , eventsProcess : DataProcess
    , mainView : MainView
    }


type alias Flags =
    String


init : Flags -> ( Model, Cmd Msg )
init backendURL =
    ( { majorModel =
            { selected = []
            , searching = ""
            , list =
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
            }
      , events = []
      , dates = []
      , now = Nothing
      , offset = 0
      , organizationModel =
            { selected = []
            , searching = ""
            , list =
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
            }
      , backendURL = backendURL
      , modalEvent = Nothing
      , navbarToggle = True
      , featureToggle = False
      , majorsToggle = False
      , organizationsToggle = False
      , searchFilter = None
      , error = ""
      , features = [ "free food", "volunteer" ]
      , selectedFeatures = []
      , eventsProcess = Loaded
      , mainView = Events
      }
    , Task.perform Initialize now
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialize date ->
            ( { model | now = Just date }
            , Ports.populateCalendar
                ([ (toYear utc) >> String.fromInt
                 , (toMonth utc)
                    >> toFirstThreeCharMonth
                 , (toDay utc) >> String.fromInt
                 ]
                    |> List.map ((|>) date)
                    |> String.join " "
                )
            )

        PopulateCalendar res ->
            let
                dates =
                    res |> List.map ((<|) (toTime >> Result.toMaybe))
            in
                ( { model
                    | dates = dates
                    , eventsProcess = Loading
                  }
                , Http.send NewEvents
                    (Http.get
                        ([ model.backendURL ++ "/api/events?month="
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
                ( ([ (toYear utc) >> String.fromInt
                   , (toMonth utc)
                        >> toFirstThreeCharMonth
                   , (toDay utc) >> String.fromInt
                   ]
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
                    ( { model | events = data, eventsProcess = Loaded }, Cmd.none )

                Err _ ->
                    ( { model | error = "something went wrong", eventsProcess = Loaded }, Cmd.none )

        ToggleFeature feature ->
            case List.member feature model.selectedFeatures of
                True ->
                    ( { model
                        | selectedFeatures =
                            model.selectedFeatures
                                |> List.filter ((/=) feature)
                      }
                    , Cmd.none
                    )

                False ->
                    ( { model
                        | selectedFeatures =
                            model.selectedFeatures
                                ++ [ feature
                                   ]
                      }
                    , Cmd.none
                    )

        OrganizationMsg subMsg ->
            let
                ( orgModel, orgCmd ) =
                    Organization.update subMsg model.organizationModel
            in
                ( { model | organizationModel = orgModel }
                , Cmd.map
                    OrganizationMsg
                    orgCmd
                )

        MajorMsg subMsg ->
            let
                ( majorModel, majorCmd ) =
                    Major.update subMsg model.majorModel
            in
                ( { model | majorModel = majorModel }
                , Cmd.map
                    MajorMsg
                    majorCmd
                )

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

        ToggleMainView ->
            case model.mainView of
                Calendar ->
                    ( { model | mainView = Events }, Cmd.none )

                Events ->
                    ( { model | mainView = Calendar }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        pageLoader =
            if model.eventsProcess == Loading then
                div [] []
            else
                div [] []
    in
        div []
            [ headerView model
            , pageLoader
            , div [ class "hero is-primary" ]
                [ div [ class "columns is-hidden-touch" ]
                    [ div [ class "column is-10 is-offset-1 hero-body" ]
                        [ filterView
                            model.majorModel
                            model.organizationModel
                            model.searchFilter
                            model.features
                            model.selectedFeatures
                        ]
                    ]
                ]
            , div [ class "hero is-light is-hidden-touch" ]
                [ div
                    [ class "columns hero-body" ]
                    [ div [ class "column is-offset-1 is-7" ] [ eventsView model.events ]
                    , div [ class "column is-2" ] [ calendarView model.dates model.events model.modalEvent ]
                    ]
                ]
            , div [ class "hero is-light is-hidden-desktop" ]
                [ div
                    [ class "columns hero-body" ]
                    [ div [ class "column" ]
                        [ if model.mainView == Events then
                            eventsView model.events
                          else
                            calendarView model.dates model.events model.modalEvent
                        ]
                    ]
                ]
            , footerView model
            ]


headerView : Model -> Html Msg
headerView model =
    let
        navbarState =
            if not model.navbarToggle then
                " is-active"
            else
                ""
    in
        nav [ class "navbar is-transparent is-primary" ]
            [ div [ class "navbar-brand" ]
                [ a [ href "#", class "navbar-item" ] [ text "SCU Events" ]
                , div
                    [ class ("navbar-burger burger" ++ navbarState)
                    , onClick ToggleNavbar
                    ]
                    (List.repeat 3 (span [] []))
                ]
            , div
                [ class ("navbar-menu" ++ navbarState) ]
                [ div [ class "navbar-start is-hidden-desktop" ]
                    [ filterView
                        model.majorModel
                        model.organizationModel
                        model.searchFilter
                        model.features
                        model.selectedFeatures
                    ]
                ]
            ]


footerView : Model -> Html Msg
footerView model =
    let
        btn_txt =
            case model.mainView of
                Events ->
                    (model.dates
                        |> List.drop 7
                        |> List.head
                        |> Maybe.withDefault Nothing
                        |> toMonthString
                    )
                        ++ " (Calendar)"

                Calendar ->
                    "Show Events"
    in
        nav [ class "is-hidden-desktop navbar is-primary is-fixed-bottom" ]
            [ div [ class "is-centered" ]
                [ button [ class "button is-primary is-pulled-left", onClick (ChangeCalendar -1) ]
                    [ i [ class "fa fa-chevron-left" ] [] ]
                , button [ class "button is-primary", onClick ToggleMainView ]
                    [ text btn_txt ]
                , button [ class "button is-primary is-pulled-right", onClick (ChangeCalendar 1) ]
                    [ i [ class "fa fa-chevron-right" ] [] ]
                ]
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.onPopulateCalendar PopulateCalendar



---- PROGRAM ----


main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
