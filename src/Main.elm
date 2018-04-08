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
        , img
        , header
        , a
        , span
        , i
        , button
        , input
        , ul
        , hr
        , label
        )
import Html.Attributes
    exposing
        ( src
        , class
        , href
        , placeholder
        , value
        , id
        , attribute
        , type_
        )
import Html.Events
    exposing
        ( onInput
        , onWithOptions
        , keyCode
        , defaultOptions
        , onClick
        )
import Json.Decode as Json
import Fuzzy exposing (match)
import Http


---- MODEL ----


type alias Major =
    String


type alias Event =
    { startDateTime : Maybe Date
    , endDateTime : Maybe Date
    , description : String
    , title : String
    , summary : String
    }


type alias Organization =
    String



-- majors, freeFood, volunteer are search filters


type alias Model =
    { majors : List Major
    , currentMajor : String
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
    }


init : ( Model, Cmd Msg )
init =
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
      }
    , Task.perform Initialize Date.now
    )



---- UPDATE ----


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
    | NoOp


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
                        ([ "http://localhost:4000?month="
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
                        ++ [ organization
                           ]
              }
            , Cmd.none
            )

        UpdateCurrentOrganization organization ->
            ( { model | currentOrganization = organization }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


date : Json.Decoder (Maybe Date)
date =
    let
        convert : String -> Json.Decoder Date
        convert raw =
            case Date.fromString raw of
                Ok date ->
                    Json.succeed date

                Err error ->
                    Json.fail error
    in
        Json.string |> Json.andThen convert |> Json.maybe


eventsDecoder : Json.Decoder (List Event)
eventsDecoder =
    Json.map5 Event
        (Json.at [ "startDateTime" ] date)
        (Json.at [ "endDateTime" ] date)
        (Json.at [ "description" ] Json.string)
        (Json.at [ "title" ] Json.string)
        (Json.at [ "summary" ] Json.string)
        |> Json.list



---- VIEW ----


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        onWithOptions "keydown" { defaultOptions | preventDefault = True } (Json.andThen isEnter keyCode)


view : Model -> Html Msg
view model =
    div []
        [ headerView
        , div [ class "columns" ]
            [ div [ class "column is-one-third" ]
                [ h3 [] [ text "Filter" ]
                , filterView model
                ]
            , div [ class "column is-two-third" ] [ calendarView model ]
            ]
        ]


headerView : Html Msg
headerView =
    header [ class "nav" ]
        [ div [ class "container" ]
            [ div [ class "nav-left" ]
                [ a [ href "#", class "nav-item is-active" ] [ text "SCU" ] ]
            , div [ class "nav-right nav-menu" ]
                [ span [ class "nav-item" ]
                    [ a [ href "#", class "button is-info is-inverted" ]
                        [ span [ class "icon" ]
                            [ i [ class "fa fa-user" ] [] ]
                        , span [] [ text "Login" ]
                        ]
                    ]
                ]
            ]
        ]


filterView : Model -> Html Msg
filterView model =
    let
        majors =
            model.majors
                |> List.map
                    (\major ->
                        span [ class "tag is-primary is-medium" ]
                            [ text major
                            , button [ class "button delete", onClick (RemoveMajor major) ]
                                []
                            ]
                    )

        simpleMatchMajor major =
            match [] [] model.currentMajor major |> .score

        majorPanels =
            (model.majorOptions
                |> List.sortBy simpleMatchMajor
                |> List.take 4
                |> List.map
                    (\major ->
                        div [ class "panel-block" ]
                            [ span
                                [ class "major is-info"
                                , onClick
                                    (AddMajor major)
                                ]
                                [ a [] [ text major ] ]
                            ]
                    )
            )

        organizations =
            model.organizations
                |> List.map
                    (\organization ->
                        span [ class "tag is-primary is-medium" ]
                            [ text organization
                            , button [ class "button delete", onClick (RemoveOrganization organization) ]
                                []
                            ]
                    )

        simpleMatchOrg org =
            match [] [] model.currentOrganization org |> .score

        organizationPanels =
            (model.organizationOptions
                |> List.sortBy simpleMatchOrg
                |> List.take 4
                |> List.map
                    (\organization ->
                        div [ class "panel-block" ]
                            [ span
                                [ class "major is-info"
                                , onClick
                                    (AddOrganization organization)
                                ]
                                [ a [] [ text organization ] ]
                            ]
                    )
            )
    in
        div [ class "control" ]
            [ label [ class "checkbox" ]
                [ input [ type_ "checkbox", onClick ToggleFreeFood ] []
                , text "Free Food?"
                ]
            , label [ class "checkbox" ]
                [ input [ type_ "checkbox", onClick ToggleVolunteer ]
                    []
                , text "Volunteer"
                ]
            , ul [] majors
            , div [ class "panel" ]
                ([ div [ class "panel block" ]
                    [ input
                        [ class "input"
                        , value model.currentMajor
                        , placeholder "Enter to add another major"
                        , onInput UpdateCurrentMajor
                        ]
                        []
                    ]
                 ]
                    ++ majorPanels
                )
            , ul [] organizations
            , div [ class "panel" ]
                ([ div [ class "panel block" ]
                    [ input
                        [ class "input"
                        , value model.currentOrganization
                        , placeholder "Enter to add another organization"
                        , onInput UpdateCurrentOrganization
                        ]
                        []
                    ]
                 ]
                    ++ organizationPanels
                )
            ]


toDayString : Maybe Date -> String
toDayString =
    Maybe.map day >> Maybe.withDefault 0 >> toString


toCalendarStyle : Date -> String
toCalendarStyle date =
    [ month >> toString, year >> toString ]
        |> List.map ((|>) date)
        |> String.join " "


toMonthString : Maybe Date -> String
toMonthString =
    Maybe.map toCalendarStyle >> Maybe.withDefault "Failed"


calendarView : Model -> Html Msg
calendarView model =
    let
        calendarHeader =
            [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]
                |> List.map (\day -> div [ class "calendar-date" ] [ text day ])

        calendarBody =
            model.dates
                |> List.map
                    (\x ->
                        let
                            events =
                                model.events
                                    |> List.filter
                                        (\y ->
                                            (y.startDateTime |> toMonthString)
                                                == (x |> toMonthString)
                                                && (y.startDateTime |> toDayString)
                                                == (x |> toDayString)
                                        )
                                    |> List.map
                                        (\z ->
                                            a [ class "calendar-event is-primary" ] [ text z.title ]
                                        )
                        in
                            div [ class "calendar-date" ]
                                [ button [ class "date-item" ]
                                    [ text
                                        (toDayString x)
                                    ]
                                , div
                                    [ class "calendar-events" ]
                                    events
                                ]
                    )
    in
        div [ class "calendar is-large" ]
            [ div [ class "calendar-nav" ]
                [ div [ class "calendar-nav-previous-month" ]
                    [ button [ class "button is-text", onClick (ChangeCalendar -1) ]
                        [ i [ class "fa fa-chevron-left" ] [] ]
                    ]
                , div []
                    [ text
                        (model.dates
                            |> List.drop 7
                            |> List.head
                            |> Maybe.withDefault Nothing
                            |> toMonthString
                        )
                    ]
                , div [ class "calendar-nav-next-month" ]
                    [ button [ class "button is-text", onClick (ChangeCalendar 1) ]
                        [ i [ class "fa fa-chevron-right" ] [] ]
                    ]
                ]
            , div [ class "calendar-container" ]
                [ div [ class "calendar-header" ] calendarHeader
                , div [ class "calendar-body" ] calendarBody
                ]
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.onPopulateCalendar PopulateCalendar



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
