module Views.Filter exposing (filterView)

import Html
    exposing
        ( Html
        , text
        , div
        , a
        , button
        , input
        , label
        , i
        )
import Html.Attributes
    exposing
        ( class
        , id
        , placeholder
        , value
        , type_
        )
import Html.Events exposing (onInput, onClick)
import Fuzzy exposing (match)
import Data.Major as Major
import Data.Organization as Organization
import Data.Feature exposing (Feature)
import Msg exposing (..)


filterView :
    Major.Model
    -> Organization.Model
    -> SearchFilter
    -> List Feature
    -> List Feature
    -> Html Msg
filterView majorModel organizationModel searchFilter features selectedFeatures =
    let
        featuresTags =
            features
                |> List.map
                    (\feature ->
                        case List.member feature selectedFeatures of
                            True ->
                                div [ id "feature-tag", class "tag is-info is-medium is-rounded", onClick (ToggleFeature feature) ]
                                    [ text feature ]

                            False ->
                                div [ id "feature-tag", class "tag is-light is-medium is-rounded", onClick (ToggleFeature feature) ]
                                    [ text feature ]
                    )

        currentMajor =
            majorModel.searching

        simpleMatchMajor major =
            match [] [] currentMajor major |> .score

        mjrs =
            majorModel.selected
                |> List.map
                    (\major ->
                        div [ class "tag is-info is-medium is-rounded" ]
                            [ text major
                            , button [ class "delete", onClick (MajorMsg (Major.Deselect major)) ]
                                []
                            ]
                    )

        majorPanels =
            (majorModel.list
                |> List.sortBy simpleMatchMajor
                |> List.take 3
                |> List.map
                    (\major ->
                        div [ class "panel-block" ]
                            [ div
                                [ class "major is-info"
                                , onClick
                                    (MajorMsg (Major.Select major))
                                ]
                                [ a [] [ text major ] ]
                            ]
                    )
            )

        orgs =
            organizationModel.selected
                |> List.map
                    (\organization ->
                        div [ class "tag is-info is-medium is-rounded" ]
                            [ text organization
                            , button
                                [ class "delete"
                                , onClick
                                    (OrganizationMsg
                                        (Organization.Deselect organization)
                                    )
                                ]
                                []
                            ]
                    )

        currentOrganization =
            organizationModel.searching

        simpleMatchOrg org =
            match [] [] currentOrganization org |> .score

        organizationPanels =
            (organizationModel.list
                |> List.sortBy simpleMatchOrg
                |> List.take 3
                |> List.map
                    (\organization ->
                        div [ class "panel-block" ]
                            [ div
                                [ class "major is-info"
                                , onClick
                                    (OrganizationMsg (Organization.Select organization))
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
                                [ button [ class "is-hidden-touch button is-info is-inverted is-outlined", onClick (ShowSearchFilter MajorFilter) ]
                                    [ text "Majors", i [ class "fa fa-angle-down" ] [] ]
                                , button [ class "is-hidden-desktop button is-info is-outlined", onClick (ShowSearchFilter MajorFilter) ]
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
                                                    , onInput
                                                        (MajorMsg
                                                            << Major.UpdateSearch
                                                        )
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
                                    [ class "is-hidden-touch button is-info is-inverted is-outlined"
                                    , onClick (ShowSearchFilter OrganizationFilter)
                                    ]
                                    [ text "Organizations", i [ class "fa fa-angle-down" ] [] ]
                                , button
                                    [ class "is-hidden-desktop button is-info is-outlined"
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
                                                    , onInput
                                                        (OrganizationMsg
                                                            << Organization.UpdateSearch
                                                        )
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
                        , div [ class "is-pulled-right" ]
                            [ button
                                [ class "is-hidden-touch button is-rounded is-danger" ]
                                [ text "Apply" ]
                            , button
                                [ class
                                    "is-hidden-desktop button is-rounded is-danger"
                                ]
                                [ text "Apply" ]
                            ]
                        ]
                    ]
                ]
            ]
