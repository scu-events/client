module Views.Calendar exposing (toMonthString, calendarView)

import Date exposing (Date, day, month, year, fromString)
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
        , p
        , section
        , footer
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
        , disabled
        )
import Html.Events
    exposing
        ( onInput
        , onWithOptions
        , keyCode
        , defaultOptions
        , onClick
        )
import Data.Event exposing (Event)
import Msg exposing (..)


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


calendarView : List (Maybe Date) -> List Event -> Maybe Event -> Html Msg
calendarView dates events modalEvent =
    let
        calendarHeader =
            [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]
                |> List.map (\day -> div [ class "calendar-date" ] [ text day ])

        calendarBody =
            dates
                |> List.map
                    (\x ->
                        let
                            calendar_events =
                                events
                                    |> List.filter
                                        (\y ->
                                            (y.start_date_time |> toMonthString)
                                                == (x |> toMonthString)
                                                && (y.start_date_time |> toDayString)
                                                == (x |> toDayString)
                                        )
                                    |> List.map
                                        (\z ->
                                            a [ class "calendar-event\n\n                                            is-primary", onClick (ShowEvent z) ] [ text z.title ]
                                        )
                        in
                            div [ class "calendar-date" ]
                                [ button [ class "date-item" ]
                                    [ text
                                        (toDayString x)
                                    ]
                                , div
                                    [ class "calendar-events" ]
                                    calendar_events
                                ]
                    )

        modal =
            case modalEvent of
                Just event ->
                    div [ class "modal is-active" ]
                        [ div [ class "modal-background" ]
                            []
                        , div [ class "modal-card" ]
                            [ header [ class "modal-card-head" ]
                                [ p [ class "modal-card-title" ]
                                    [ text event.title ]
                                , button
                                    [ class "delete", onClick HideEvent ]
                                    []
                                ]
                            , section [ class "modal-card-body" ]
                                [ text event.description ]
                            , footer [ class "modal-card-foot" ]
                                [ button [ class "button is-success" ]
                                    [ text "Add to your calendar" ]
                                ]
                            ]
                        ]

                Nothing ->
                    div [] []
    in
        div [ class "calendar" ]
            [ div [ class "calendar-nav" ]
                [ div [ class "calendar-nav-previous-month" ]
                    [ button [ class "button is-primary", onClick (ChangeCalendar -1) ]
                        [ i [ class "fa fa-chevron-left" ] [] ]
                    ]
                , div []
                    [ text
                        (dates
                            |> List.drop 7
                            |> List.head
                            |> Maybe.withDefault Nothing
                            |> toMonthString
                        )
                    ]
                , div [ class "calendar-nav-next-month" ]
                    [ button [ class "button is-primary", onClick (ChangeCalendar 1) ]
                        [ i [ class "fa fa-chevron-right" ] [] ]
                    ]
                ]
            , div [ class "calendar-container" ]
                [ div [ class "calendar-header" ] calendarHeader
                , div [ class "calendar-body" ] calendarBody
                ]
            , modal
            ]
