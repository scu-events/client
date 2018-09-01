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
    case monthIntToLength (month) of
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



-- use posix milliseconds : a day == 86400000 milliseconds


generateArrayOfTheMonth : Maybe Posix -> Int -> List (Maybe Posix)
generateArrayOfTheMonth mposix diff =
    case mposix of
        Just posix ->
            let
                thisMonthIntBeforeMod =
                    monthToInt (toMonth utc posix) + diff

                thisMonthInt =
                    modBy 12 thisMonthIntBeforeMod

                thisYear =
                    toYear utc posix
                        + thisMonthIntBeforeMod
                        // 12
                        + if thisMonthIntBeforeMod < 0 then
                            -1
                          else
                            0

                baseDate =
                    modBy 7 (weekdayToInt (toWeekday utc posix) - (modBy 7 (toDay utc posix)) + 1)

                diffDays =
                    if diff < 0 then
                        List.range diff -1
                            |> List.map
                                (\m ->
                                    numberOfDaysInTheMonth (modBy 12 (thisMonthInt + m))
                                        (toYear utc posix
                                            + thisMonthIntBeforeMod
                                            // 12
                                            + if thisMonthIntBeforeMod < 0 then
                                                -1
                                              else
                                                0
                                        )
                                )
                    else
                        List.range 0 diff
                            |> List.map
                                (\m ->
                                    numberOfDaysInTheMonth (modBy 12 (thisMonthInt + m))
                                        (toYear utc posix
                                            + thisMonthIntBeforeMod
                                            // 12
                                            + if thisMonthIntBeforeMod < 0 then
                                                -1
                                              else
                                                0
                                        )
                                )

                weekdayOfTheFirstDayOfTheMonth =
                    modBy 7 (baseDate + List.sum diffDays)

                lengthOfThisMonth =
                    numberOfDaysInTheMonth thisMonthInt thisYear

                daysFromThisMonth =
                    List.range 1 lengthOfThisMonth

                lastMonthLength =
                    numberOfDaysInTheMonth (modBy 12 (thisMonthInt - 1))
                        (if thisMonthInt == 0 then
                            thisYear - 1
                         else
                            thisYear
                        )

                totalLength =
                    if
                        (weekdayOfTheFirstDayOfTheMonth == 6 && lengthOfThisMonth > 29)
                            || (weekdayOfTheFirstDayOfTheMonth
                                    == 5
                                    && lengthOfThisMonth
                                    > 30
                               )
                    then
                        42
                    else
                        35

                daysFromLastMonth =
                    List.repeat weekdayOfTheFirstDayOfTheMonth 0

                daysFromNextMonth =
                    List.repeat (totalLength - List.length daysFromLastMonth - lengthOfThisMonth) 0

                -- List.range 1 (length - lengthOfDaysFromLastMonth - lengthOfThisMonth)
            in
                List.concat [ daysFromLastMonth, daysFromThisMonth, daysFromNextMonth ]

        Nothing ->
            []
