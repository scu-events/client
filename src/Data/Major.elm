module Data.Major exposing (Model, Msg(..), update)


type alias Major =
    String


type alias Model =
    { list : List Major
    , searching : String
    , selected : List Major
    }


type Msg
    = Select Major
    | Deselect Major
    | UpdateSearch Major


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select major ->
            ( { model
                | selected = model.selected ++ [ major ]
                , searching = ""
                , list = model.list |> List.filter ((/=) major)
              }
            , Cmd.none
            )

        Deselect major ->
            ( { model
                | selected = model.selected |> List.filter ((/=) major)
                , list = model.list ++ [ major ]
              }
            , Cmd.none
            )

        UpdateSearch major ->
            ( { model | searching = major }, Cmd.none )
