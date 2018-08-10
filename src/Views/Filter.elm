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


filterView :
    List Major
    -> Major
    -> List Major
    -> List Organization
    -> Organization
    -> List Organization
    -> SearchFilter
    -> List Feature
    -> List Feature
    -> Html Msg
filterView majors currentMajor majorOptions organizations currentOrganization organizationOptions searchFilter features selectedFeatures =
    let
        featuresTags =
            features
                |> List.map
                    (\feature ->
                        case List.member feature selectedFeatures of
                            True ->
                                a [ class "tag is-primary is-medium is-rounded", onClick (ToggleFeature feature) ]
                                    [ text feature ]

                            False ->
                                a [ class "tag is-light is-medium is-rounded", onClick (ToggleFeature feature) ]
                                    [ text feature ]
                    )

        simpleMatchMajor major =
            match [] [] currentMajor major |> .score

        mjrs =
            majors
                |> List.map
                    (\major ->
                        div [ class "tag is-primary is-medium is-rounded" ]
                            [ text major
                            , button [ class "delete", onClick (RemoveMajor major) ]
                                []
                            ]
                    )

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

        orgs =
            organizations
                |> List.map
                    (\organization ->
                        div [ class "tag is-primary is-medium is-rounded" ]
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
        div []
            [ div [ class "columns" ]
                [ div [ class "column is-clearfix" ]
                    [ div
                        [ class
                            "is-pulled-left"
                        ]
                        (featuresTags ++ mjrs ++ orgs)
                    ]
                ]
            , div [ class "columns" ]
                [ div [ class "column is-clearfix" ]
                    [ div [ class "is-pulled-left" ]
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
                        , div
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
                ]
            ]
