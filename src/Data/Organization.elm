module Data.Organization exposing (Model, Msg(..), update)


type alias Organization =
    String


type alias Model =
    { list : List Organization
    , searching : String
    , selected : List Organization
    }


type Msg
    = Select Organization
    | Deselect Organization
    | UpdateSearch Organization


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select organization ->
            ( { model
                | selected = model.selected ++ [ organization ]
                , searching = ""
                , list = model.list |> List.filter ((/=) organization)
              }
            , Cmd.none
            )

        Deselect organization ->
            ( { model
                | selected =
                    model.selected
                        |> List.filter
                            ((/=)
                                organization
                            )
                , list =
                    model.list
                        ++ [ organization ]
              }
            , Cmd.none
            )

        UpdateSearch organization ->
            ( { model | searching = organization }, Cmd.none )
