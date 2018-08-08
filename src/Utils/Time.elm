module Utils.Time
    exposing
        ( timeToString
        , toCalendarStyle
        , toMonthString
        , displayDate
        , displayTime
        , formatTime
        , toDayString
        )

import Date exposing (Date, day, year, month, hour, minute)


timeToString : Maybe Date -> (Date -> Int) -> String
timeToString time func =
    time |> (Maybe.map func >> Maybe.withDefault 0 >> toString)


toCalendarStyle : Date -> String
toCalendarStyle date =
    [ month >> toString, year >> toString ]
        |> List.map ((|>) date)
        |> String.join " "


toMonthString : Maybe Date -> String
toMonthString =
    Maybe.map toCalendarStyle >> Maybe.withDefault "Failed"


displayDate : Maybe Date -> String
displayDate time =
    [ toMonthString time, timeToString time day ] |> String.join "  "


formatTime : Maybe Date -> (Date -> Int) -> String
formatTime time func =
    timeToString time func |> String.padLeft 2 '0'


displayTime : Maybe Date -> String
displayTime date =
    [ (formatTime date hour), (formatTime date minute) ] |> String.join ":"


toDayString : Maybe Date -> String
toDayString time =
    timeToString time day
