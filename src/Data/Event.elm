module Data.Event exposing (Event, eventsDecoder)

import Date exposing (Date, fromString)
import Json.Decode as Json


type alias Event =
    { start_date_time : Maybe Date
    , end_date_time : Maybe Date
    , description : String
    , title : String
    , summary : String
    }


date : Json.Decoder (Maybe Date)
date =
    let
        convert : String -> Json.Decoder Date
        convert raw =
            case fromString raw of
                Ok date ->
                    Json.succeed date

                Err error ->
                    Json.fail error
    in
        Json.string |> Json.andThen convert |> Json.maybe


eventsDecoder : Json.Decoder (List Event)
eventsDecoder =
    Json.at [ "data" ]
        (Json.map5 Event
            (Json.at [ "start_date_time" ] date)
            (Json.at [ "end_date_time" ] date)
            (Json.at [ "description" ] Json.string)
            (Json.at [ "title" ] Json.string)
            (Json.at [ "summary" ] Json.string)
            |> Json.list
        )
