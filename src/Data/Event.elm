module Data.Event exposing (Event, eventsDecoder)

import Time exposing (Posix)
import Json.Decode as Decode
import Iso8601 exposing (toTime)


type alias Event =
    { start_date_time : Maybe Posix
    , end_date_time : Maybe Posix
    , summary : String
    , html_link : String
    , location : Maybe String
    , description : Maybe String
    }


dateDecoder : Decode.Decoder (Maybe Posix)
dateDecoder =
    let
        convert : String -> Decode.Decoder Posix
        convert raw =
            case toTime raw of
                Ok date ->
                    Decode.succeed date

                Err _ ->
                    Decode.fail "failed"
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
