module Data.Event exposing (Event, eventsDecoder)

import Date exposing (Date, fromString)
import Json.Decode as Json


type alias Event =
    { start_date_time : Maybe Date
    , end_date_time : Maybe Date
    , summary : String
    , html_link : String
    }


dateDecoder : Json.Decoder (Maybe Date)
dateDecoder =
    let
        convert : String -> Json.Decoder Date
        convert raw =
            case fromString raw of
                Ok date ->
                    Json.succeed date

                Err error ->
                    Json.fail error
    in
        Json.field "dateTime" Json.string |> Json.andThen convert |> Json.maybe


eventsDecoder : Json.Decoder (List Event)
eventsDecoder =
    Json.at [ "data" ]
        (Json.map4 Event
            (Json.at [ "start" ] dateDecoder)
            (Json.at [ "end" ] dateDecoder)
            (Json.at [ "summary" ] Json.string)
            (Json.at [ "htmlLink" ] Json.string)
            |> Json.list
        )
