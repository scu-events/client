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
        , generateArrayOfTheMonth
        )

import Time
    exposing
        ( Posix
        , Month(..)
        , Weekday(..)
        , toDay
        , toWeekday
        , toYear
        , toMonth
        , toHour
        , toMinute
        , utc
        , posixToMillis
        , millisToPosix
        )


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


numberOfDaysInTheMonth : Int -> Int -> Int
numberOfDaysInTheMonth month year =
    case monthIntToLength month of
        28 ->
            if leapYear year then
                29
            else
                28

        x ->
            x


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


weekdayToInt : Weekday -> Int
weekdayToInt weekday =
    case weekday of
        Sun ->
            0

        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


monthIntToLength : Int -> Int
monthIntToLength month =
    case month of
        0 ->
            31

        1 ->
            28

        2 ->
            31

        3 ->
            30

        4 ->
            31

        5 ->
            30

        6 ->
            31

        7 ->
            31

        8 ->
            30

        9 ->
            31

        10 ->
            30

        11 ->
            31

        _ ->
            0


leapYear : Int -> Bool
leapYear year =
    if ((modBy 4 year == 0) && (modBy 100 year /= 0)) || (modBy 400 year == 0) then
        True
    else
        False


posixToLengthOfTheMonth : Posix -> Int
posixToLengthOfTheMonth posix =
    case monthIntToLength (monthToInt (toMonth utc posix)) of
        28 ->
            if leapYear (toYear utc posix) then
                29
            else
                28

        x ->
            x


displayDate : Maybe Posix -> String
displayDate time =
    [ timeToString time (toDay utc) |> String.padLeft 2 '0', toMonthString time ] |> String.join "  "


formatTime : Maybe Posix -> (Posix -> Int) -> String
formatTime time func =
    timeToString time func |> String.padLeft 2 '0'


displayTime : Maybe Posix -> String
displayTime date =
    [ (formatTime date (toHour utc)), (formatTime date (toMinute utc)) ] |> String.join ":"


toDayString : Maybe Posix -> String
toDayString time =
    case timeToString time (toDay utc) of
        "0" ->
            " "

        x ->
            x


generateArrayOfTheMonth : Maybe Posix -> Int -> List (Maybe Posix)
generateArrayOfTheMonth mposix diff =
    case mposix of
        Just posix ->
            let
                thisMonthIntBeforeMod : Int
                thisMonthIntBeforeMod =
                    monthToInt (toMonth utc posix)

                yearWithOffset : Int -> Int
                yearWithOffset d =
                    let
                        month : Int
                        month =
                            thisMonthIntBeforeMod + d
                    in
                        toYear utc posix
                            + month
                            // 12
                            + if month < 0 then
                                -1
                              else
                                0

                numOfDaysInTheMonthWithOffset : Int -> Int
                numOfDaysInTheMonthWithOffset d =
                    numberOfDaysInTheMonth (modBy 12 (thisMonthIntBeforeMod + d))
                        (yearWithOffset d)

                days : Int -> Int -> List Int
                days start end =
                    List.range start end
                        |> List.map numOfDaysInTheMonthWithOffset

                diffDays : List Int
                diffDays =
                    if diff /= 0 then
                        if diff < 0 then
                            days diff -1 |> List.map ((*) -1)
                        else
                            days 0 (diff - 1)
                    else
                        []

                firstDayOfTheMonth : Int
                firstDayOfTheMonth =
                    posixToMillis posix + daysInMillis (List.sum diffDays - (toDay utc posix - 1))

                weekdayOfTheFirstDay : Int
                weekdayOfTheFirstDay =
                    weekdayToInt
                        (toWeekday utc (millisToPosix firstDayOfTheMonth))

                lengthOfThisMonth : Int
                lengthOfThisMonth =
                    numOfDaysInTheMonthWithOffset diff

                totalLength : Int
                totalLength =
                    case weekdayOfTheFirstDay of
                        6 ->
                            if lengthOfThisMonth >= 30 then
                                42
                            else
                                35

                        5 ->
                            if lengthOfThisMonth >= 31 then
                                42
                            else
                                35

                        _ ->
                            35

                daysFromThisMonth : List (Maybe Posix)
                daysFromThisMonth =
                    List.range 0 (lengthOfThisMonth - 1)
                        |> List.map daysInMillis
                        |> List.map (\d -> Just (millisToPosix (firstDayOfTheMonth + d)))

                daysFromLastMonth : List (Maybe Posix)
                daysFromLastMonth =
                    List.repeat weekdayOfTheFirstDay Nothing

                daysFromNextMonth : List (Maybe Posix)
                daysFromNextMonth =
                    List.repeat
                        (totalLength
                            - List.length daysFromLastMonth
                            - List.length daysFromThisMonth
                        )
                        Nothing
            in
                List.concat [ daysFromLastMonth, daysFromThisMonth, daysFromNextMonth ]

        Nothing ->
            []


daysInMillis : Int -> Int
daysInMillis days =
    days * 86400000
