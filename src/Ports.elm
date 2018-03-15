port module Ports
    exposing
        ( populateCalendar
        , onPopulateCalendar
        , repopulateCalendar
        )


port populateCalendar : String -> Cmd msg


port repopulateCalendar : ( String, Int ) -> Cmd msg


port onPopulateCalendar : (List String -> msg) -> Sub msg
