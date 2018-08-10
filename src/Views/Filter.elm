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
import Data.Major exposing (Major)
import Data.Organization as Organization
import Data.Feature exposing (Feature)
import Msg exposing (..)


filterView :
    List Major
    -> Major
    -> List Major
    -> Organization.Model
    -> SearchFilter
    -> List Feature
    -> List Feature
    -> Html Msg
filterView majors currentMajor majorOptions organizationModel searchFilter features selectedFeatures =
    let
        featuresTags =
            features
                |> List.map
                    (\feature ->
                        case List.member feature selectedFeatures of
                            True ->
                                div [ id "feature-tag", class "tag is-primary is-medium is-rounded", onClick (ToggleFeature feature) ]
                                    [ text feature ]

                            False ->
                                div [ id "feature-tag", class "tag is-light is-medium is-rounded", onClick (ToggleFeature feature) ]
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
                            [ div
                                [ class "major is-info"
                                , onClick
                                    (AddMajor major)
                                ]
                                [ a [] [ text major ] ]
                            ]
                    )
            )

        orgs =
            organizationModel.selected
                |> List.map
                    (\organization ->
                        div [ class "tag is-primary is-medium is-rounded" ]
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
                        ]
                    ]
                ]
            ]
