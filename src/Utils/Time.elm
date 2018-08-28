module Utils.Time
    exposing
        ( timeToString
        , toCalendarStyle
        , toMonthString
        , displayDate
        , displayTime
        , formatTime
        , toDayString
        , toFirstThreeCharMonth
        )

import Time exposing (Posix, Month(..), toDay, toYear, toMonth, toHour, toMinute, utc)


timeToString : Maybe Posix -> (Posix -> Int) -> String
timeToString time func =
    time |> (Maybe.map func >> Maybe.withDefault 0 >> String.fromInt)


toCalendarStyle : Posix -> String
toCalendarStyle date =
    [ (toMonth utc) >> toFirstThreeCharMonth, (toYear utc) >> String.fromInt ]
        |> List.map ((|>) date)
        |> String.join " "


toMonthString : Maybe Posix -> String
toMonthString =
    Maybe.map toCalendarStyle >> Maybe.withDefault "Failed"


toFirstThreeCharMonth : Month -> String
toFirstThreeCharMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


displayDate : Maybe Posix -> String
displayDate time =
    [ toMonthString time, timeToString time (toDay utc) ] |> String.join "  "


formatTime : Maybe Posix -> (Posix -> Int) -> String
formatTime time func =
    timeToString time func |> String.padLeft 2 '0'


displayTime : Maybe Posix -> String
displayTime date =
    [ (formatTime date (toHour utc)), (formatTime date (toMinute utc)) ] |> String.join ":"


toDayString : Maybe Posix -> String
toDayString time =
    timeToString time (toDay utc)
