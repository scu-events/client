module Views.Calendar exposing (calendarView)

import Time exposing (Posix)
import Utils.Time exposing (toMonthString, toDayString)
import Html
    exposing
        ( Html
        , text
        , div
        , header
        , a
        , i
        , button
        , p
        , section
        , footer
        )
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Data.Event exposing (Event)
import Msg exposing (..)


calendarView : List Posix -> List Event -> Maybe Event -> Html Msg
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
                            current_events =
                                events
                                    |> List.filter
                                        (\y ->
                                            (y.start_date_time |> toMonthString)
                                                == (x |> toMonthString)
                                                && (y.start_date_time |> toDayString)
                                                == (x |> toDayString)
                                        )

                            state =
                                current_events |> List.isEmpty
                        in
                            div
                                [ class
                                    ([ "calendar-date"
                                     , if state then
                                        ""
                                       else
                                        "tooltip"
                                     ]
                                        |> String.join " "
                                    )
                                , attribute "data-tooltip"
                                    (current_events |> List.length |> String.fromInt)
                                ]
                                [ button
                                    [ class
                                        ([ "date-item"
                                         , if state then
                                            ""
                                           else
                                            "is-today"
                                         ]
                                            |> String.join " "
                                        )
                                    ]
                                    [ text (toDayString x) ]
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
                                    [ text event.summary ]
                                , button
                                    [ class "delete", onClick HideEvent ]
                                    []
                                ]
                            , section [ class "modal-card-body" ]
                                [ text event.summary ]
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
