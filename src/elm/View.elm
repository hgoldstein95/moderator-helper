module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Material.Scheme
import Material.Chip as Chip
import Material.Button as Button
import Material.Options as Options
import Material.Color as Color
import Material.List as Lists
import Material.Icon as Icon
import Model exposing (..)
import Update exposing (..)
import Mafia exposing (..)


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "max-width", "600px" )
            , ( "margin", "100px auto" )
            , ( "font-family", "sans-serif" )
            ]
        ]
        [ h1 [] [ text "Mafia App" ]
        , case model.state of
            Create ->
                viewCreate model

            Night ->
                viewNight model

            Day ->
                viewDay model

            End ->
                viewEnd model
        ]
        |> Material.Scheme.topWithScheme Color.Teal Color.Red


roleInput : Model -> Role -> Html Msg
roleInput model role =
    Lists.ul
        []
        [ Lists.li [ Lists.withSubtitle ]
            [ Lists.content []
                [ text role.name
                , Lists.subtitle []
                    [ Maybe.withDefault
                        0
                        (Dict.get role.name model.setup)
                        |> toString
                        |> text
                    ]
                ]
            , Lists.content2 []
                [ Button.render Mdl
                    []
                    model.mdl
                    [ Options.onClick (IncRole role) ]
                    [ Icon.i "keyboard_arrow_up" ]
                , Button.render Mdl
                    []
                    model.mdl
                    [ Options.onClick (DecRole role) ]
                    [ Icon.i "keyboard_arrow_down" ]
                ]
            ]
        ]


viewCreate : Model -> Html Msg
viewCreate model =
    div []
        [ h2 [] [ text "Create" ]
        , div [] (List.map (roleInput model) (Dict.values roles))
        , Button.render Mdl
            []
            model.mdl
            [ Button.colored, Button.raised, Options.onClick Start ]
            [ text "Start" ]
        ]


playerItem : Model -> Player -> Html Msg
playerItem model p =
    let
        actBtns =
            List.map
                (\a ->
                    button
                        [ class "role-modifier"
                        , onClick (Act p a)
                        ]
                        [ text (toString a) ]
                )
                p.role.actions

        isVisiting =
            case model.visiting of
                Nothing ->
                    False

                Just ( play, _ ) ->
                    p.id == play.id

        attrs =
            onClick (Visit p)
                :: (if isVisiting then
                        [ class "visiting" ]
                    else
                        []
                   )
    in
        li
            [ class "role-item" ]
            [ div []
                ([ span
                    attrs
                    [ text <| (toString p.id) ++ " " ++ p.role.name ]
                 ]
                    ++ actBtns
                )
            ]


showVisited : Model -> Html Msg
showVisited model =
    ul
        [ class "role-list" ]
        (List.concatMap
            (\( id, l ) ->
                List.map
                    (\( s, a ) ->
                        li []
                            [ text <|
                                (toString s.id)
                                    ++ " -"
                                    ++ (toString a)
                                    ++ "-> "
                                    ++ (toString id)
                            ]
                    )
                    l
            )
            (Dict.toList model.visited)
        )


viewNight : Model -> Html Msg
viewNight model =
    div []
        [ h2 [] [ text "Night" ]
        , button [ onClick ToDay ] [ text "In the morning..." ]
        , button [ onClick EndGame ] [ text "Game Over" ]
        , ul [] (List.map (playerItem model) (Dict.values model.players))
        , showVisited model
        ]


viewDay : Model -> Html Msg
viewDay model =
    div []
        [ h2 [] [ text "Day" ]
        , button [ onClick ToNight ] [ text "Go to sleep." ]
        , button [ onClick EndGame ] [ text "Game Over" ]
        , h3 [] [ text "Announcements" ]
        , ul []
            (model.announcements
                |> List.map (\x -> li [] [ text (toString x) ])
            )
        ]


viewEnd : Model -> Html Msg
viewEnd model =
    div []
        [ h2 [] [ text "End" ]
        , button [ onClick Reset ] [ text "New game." ]
        ]
