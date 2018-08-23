module Views.Events exposing (eventsView)

import Utils.Time
    exposing
        ( timeToString
        , toCalendarStyle
        , toMonthString
        , displayDate
        , displayTime
        , formatTime
        )
import Data.Event exposing (Event)
import Msg exposing (..)
import Html exposing (Html, h2, div, text, header, p, footer, a)
import Html.Attributes exposing (class, href)
import Set as Set


eventsView : List Event -> Html Msg
eventsView events =
    div []
        (if List.isEmpty events then
            [ h2 [] [ text "No Events this month" ] ]
         else
            events
                |> List.map (.start_date_time >> displayDate)
                |> Set.fromList
                |> Set.toList
                |> List.map
                    (\date ->
                        (div []
                            [ p [ class "has-text-weight-bold" ] [ text date ]
                            , div []
                                (events
                                    |> List.filter
                                        (\e -> displayDate e.start_date_time == date)
                                    |> List.map eventView
                                )
                            ]
                        )
                    )
        )


eventView : Event -> Html Msg
eventView event =
    div [ class "card" ]
        [ header [ class "card-header" ]
            [ p [ class "card-header-title" ]
                [ text
                    ([ displayTime event.start_date_time
                     , event.summary
                     , " @ "
                     , Maybe.withDefault "No location provided" event.location
                     ]
                        |> String.join " "
                    )
                ]
            ]
        , div [ class "card-content" ]
            [ div [ class "content" ]
                [ text (Maybe.withDefault event.summary event.description) ]
            ]
        , footer [ class "card-footer" ]
            [ a [ href event.html_link, class "card-footer-item" ]
                [ text "Add to Calendar" ]
            , a [ href "#", class "card-footer-item" ]
                [ text "Invite friends" ]
            ]
        ]
