module Views.Filter exposing (filterView)

import Html
    exposing
        ( Html
        , text
        , div
        , a
        , span
        , button
        , input
        , ul
        , label
        )
import Html.Attributes
    exposing
        ( class
        , placeholder
        , value
        , type_
        )
import Html.Events exposing (onInput, onClick)
import Fuzzy exposing (match)
import Data.Major exposing (Major)
import Data.Organization exposing (Organization)
import Msg exposing (..)


filterView : List Major -> Major -> List Major -> List Organization -> Organization -> List Organization -> Html Msg
filterView majors currentMajor majorOptions organizations currentOrganization organizationOptions =
    let
        mjrs =
            majors
                |> List.map
                    (\major ->
                        span [ class "tag is-primary is-medium" ]
                            [ text major
                            , button [ class "delete", onClick (RemoveMajor major) ]
                                []
                            ]
                    )

        simpleMatchMajor major =
            match [] [] currentMajor major |> .score

        majorPanels =
            (majorOptions
                |> List.sortBy simpleMatchMajor
                |> List.take 4
                |> List.map
                    (\major ->
                        div [ class "panel-block" ]
                            [ span
                                [ class "major is-info"
                                , onClick
                                    (AddMajor major)
                                ]
                                [ a [] [ text major ] ]
                            ]
                    )
            )

        orgs =
            organizations
                |> List.map
                    (\organization ->
                        span [ class "tag is-primary is-medium" ]
                            [ text organization
                            , button [ class "delete", onClick (RemoveOrganization organization) ]
                                []
                            ]
                    )

        simpleMatchOrg org =
            match [] [] currentOrganization org |> .score

        organizationPanels =
            (organizationOptions
                |> List.sortBy simpleMatchOrg
                |> List.take 4
                |> List.map
                    (\organization ->
                        div [ class "panel-block" ]
                            [ span
                                [ class "major is-info"
                                , onClick
                                    (AddOrganization organization)
                                ]
                                [ a [] [ text organization ] ]
                            ]
                    )
            )
    in
        div [ class "control" ]
            [ label [ class "checkbox" ]
                [ input [ type_ "checkbox", onClick ToggleFreeFood ] []
                , text "Free Food?"
                ]
            , label [ class "checkbox" ]
                [ input [ type_ "checkbox", onClick ToggleVolunteer ]
                    []
                , text "Volunteer"
                ]
            , ul [] mjrs
            , div [ class "panel" ]
                ([ div [ class "panel block" ]
                    [ input
                        [ class "input"
                        , value currentMajor
                        , placeholder "Enter to add another major"
                        , onInput UpdateCurrentMajor
                        ]
                        []
                    ]
                 ]
                    ++ majorPanels
                )
            , ul [] orgs
            , div [ class "panel" ]
                ([ div [ class "panel block" ]
                    [ input
                        [ class "input"
                        , value currentOrganization
                        , placeholder "Enter to add another organization"
                        , onInput UpdateCurrentOrganization
                        ]
                        []
                    ]
                 ]
                    ++ organizationPanels
                )
            ]
