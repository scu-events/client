module Views.Events exposing (eventsView)

import Date exposing (Date, day, year, month, hour, minute)
import Data.Event exposing (Event)
import Msg exposing (..)
import Html exposing (Html, div, text, h3, header, p)
import Html.Attributes exposing (class)


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


display : Maybe Date -> String
display date =
    (date |> toMonthString)
        ++ " "
        ++ (date |> toDayString)


displayTime : Maybe Date -> String
displayTime date =
    (date |> (Maybe.map hour >> Maybe.withDefault 0 >> toString))
        ++ ":"
        ++ (date |> (Maybe.map minute >> Maybe.withDefault 0 >> toString))


eventsView : List Event -> Html Msg
eventsView events =
    let
        dates =
            events
                |> List.map
                    (\event -> event.start_date_time |> display)
    in
        div []
            (dates
                |> List.map
                    (\date ->
                        (div []
                            [ h3 [] [ text date ]
                            , div []
                                (events
                                    |> List.filter
                                        (\event ->
                                            (event.start_date_time |> display) == date
                                        )
                                    |> List.map
                                        (\event ->
                                            div [ class "card" ]
                                                [ header [ class "card-header" ]
                                                    [ p [ class "card-header-title" ]
                                                        [ text
                                                            ((event.start_date_time |> displayTime)
                                                                ++ " "
                                                                ++ event.title
                                                            )
                                                        ]
                                                    ]
                                                , div [ class "card-content" ] [ div [ class "content" ] [ text event.summary ] ]
                                                ]
                                        )
                                )
                            ]
                        )
                    )
            )
