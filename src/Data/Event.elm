module Data.Event exposing (Event, eventsDecoder)

import Date exposing (Date, fromString)
import Json.Decode as Decode


type alias Event =
    { start_date_time : Maybe Date
    , end_date_time : Maybe Date
    , summary : String
    , html_link : String
    , location : Maybe String
    , description : Maybe String
    }


dateDecoder : Decode.Decoder (Maybe Date)
dateDecoder =
    let
        convert : String -> Decode.Decoder Date
        convert raw =
            case fromString raw of
                Ok date ->
                    Decode.succeed date

                Err error ->
                    Decode.fail error
    in
        Decode.field "dateTime" Decode.string |> Decode.andThen convert |> Decode.maybe


eventsDecoder : Decode.Decoder (List Event)
eventsDecoder =
    Decode.at [ "data" ]
        (Decode.map6 Event
            (Decode.at [ "start" ] dateDecoder)
            (Decode.at [ "end" ] dateDecoder)
            (Decode.at [ "summary" ] Decode.string)
            (Decode.at [ "htmlLink" ] Decode.string)
            (Decode.at [ "location" ] Decode.string |> Decode.maybe)
            (Decode.at [ "description" ] Decode.string |> Decode.maybe)
            |> Decode.list
        )
