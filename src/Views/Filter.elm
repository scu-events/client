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
        , label
        , i
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
import Data.Feature exposing (Feature)
import Msg exposing (..)


filterView : List Major -> Major -> List Major -> List Organization -> Organization -> List Organization -> SearchFilter -> List Feature -> Html Msg
filterView majors currentMajor majorOptions organizations currentOrganization organizationOptions searchFilter features =
    let
        simpleMatchMajor major =
            match [] [] currentMajor major |> .score

        majorPanels =
            (majorOptions
                |> List.sortBy simpleMatchMajor
                |> List.take 3
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

        simpleMatchOrg org =
            match [] [] currentOrganization org |> .score

        organizationPanels =
            (organizationOptions
                |> List.sortBy simpleMatchOrg
                |> List.take 3
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
        div [ class "columns" ]
            [ div [ class "column" ]
                [ div
                    [ class
                        ("dropdown "
                            ++ if searchFilter == FeatureFilter then
                                "is-active"
                               else
                                ""
                        )
                    ]
                    [ div [ class "dropdown-trigger" ]
                        [ button
                            [ class "button is-primary"
                            , onClick (ShowSearchFilter FeatureFilter)
                            ]
                            [ text "Feature", i [ class "fa fa-angle-down" ] [] ]
                        ]
                    , div [ class "dropdown-menu" ]
                        [ div [ class "dropdown-content" ]
                            (features
                                |> List.map
                                    (\feature ->
                                        div [ class "dropdown-item" ]
                                            [ label [ class "checkbox" ]
                                                [ input
                                                    [ type_ "checkbox"
                                                    , onClick
                                                        (ToggleFeature feature)
                                                    ]
                                                    []
                                                , text feature
                                                ]
                                            ]
                                    )
                            )
                        ]
                    ]
                ]
            , div [ class "column" ]
                [ div
                    [ class
                        ("dropdown "
                            ++ if searchFilter == MajorFilter then
                                "is-active"
                               else
                                ""
                        )
                    ]
                    [ div [ class "dropdown-trigger" ]
                        [ button [ class "button is-primary", onClick (ShowSearchFilter MajorFilter) ]
                            [ text "Majors", i [ class "fa fa-angle-down" ] [] ]
                        ]
                    , div [ class "dropdown-menu" ]
                        [ div [ class "dropdown-content" ]
                            [ div [ class "dropdown-item" ]
                                [ div [ class "panel" ]
                                    ([ div [ class "panel block" ]
                                        [ input
                                            [ class "input"
                                            , value currentMajor
                                            , placeholder "Major"
                                            , onInput UpdateCurrentMajor
                                            ]
                                            []
                                        ]
                                     ]
                                        ++ majorPanels
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "column" ]
                [ div
                    [ class
                        ("dropdown "
                            ++ if searchFilter == OrganizationFilter then
                                "is-active"
                               else
                                ""
                        )
                    ]
                    [ div [ class "dropdown-trigger" ]
                        [ button
                            [ class "button is-primary"
                            , onClick (ShowSearchFilter OrganizationFilter)
                            ]
                            [ text "Organizations", i [ class "fa fa-angle-down" ] [] ]
                        ]
                    , div [ class "dropdown-menu" ]
                        [ div [ class "dropdown-content" ]
                            [ div [ class "dropdown-item" ]
                                [ div [ class "panel" ]
                                    ([ div [ class "panel block" ]
                                        [ input
                                            [ class "input"
                                            , value currentOrganization
                                            , placeholder "Organization"
                                            , onInput UpdateCurrentOrganization
                                            ]
                                            []
                                        ]
                                     ]
                                        ++ organizationPanels
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
            ]
