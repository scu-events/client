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


calendarView : List (Maybe Posix) -> List Event -> Maybe Event -> Html Msg
calendarView dates events modalEvent =
    let
        calendarHeader : List (Html Msg)
        calendarHeader =
            [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]
                |> List.map (\day -> div [ class "calendar-date" ] [ text day ])

        calendarBody : List (Html Msg)
        calendarBody =
            dates
                |> List.map
                    (\x ->
                        let
                            current_events : List Event
                            current_events =
                                events
                                    |> List.filter
                                        (\y ->
                                            (y.start_date_time |> toMonthString)
                                                == (x |> toMonthString)
                                                && (y.start_date_time |> toDayString)
                                                == (x |> toDayString)
                                        )

                            state : Bool
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
            ]
