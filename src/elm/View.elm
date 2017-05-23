module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Dict exposing (Dict)
import Material.Scheme
import Material.Button as Button
import Material.Options as Options
import Material.Color as Color
import Material.List as Lists
import Material.Icon as Icon
import Material.Typography as Typo
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
    Options.div []
        [ Options.styled p [ Typo.display2 ] [ text "Create a Setup" ]
        , Options.div [] (List.map (roleInput model) (Dict.values roles))
        , Button.render Mdl
            []
            model.mdl
            [ Button.colored, Button.raised, Options.onClick Start ]
            [ text "Start" ]
        ]


makeActBtn : Model -> Player -> Action -> Html Msg
makeActBtn model p a =
    Button.render Mdl
        []
        model.mdl
        [ Options.onWithOptions
            "click"
            { stopPropagation = True
            , preventDefault = False
            }
            (Json.succeed (Act p a))
        , case model.visiting of
            Nothing ->
                Options.nop

            Just ( play, act ) ->
                if p.id == play.id && act == a then
                    Button.colored
                else
                    Options.nop
        ]
        [ text (toString a) ]


playerItem : Model -> Player -> Html Msg
playerItem model p =
    Lists.li
        [ Options.onClick (Visit p) ]
        [ Lists.content []
            [ span [] [ text <| (toString p.id) ++ " " ++ p.role.name ] ]
        , Lists.content2 [] (List.map (makeActBtn model p) p.role.actions)
        ]


viewNight : Model -> Html Msg
viewNight model =
    div []
        [ Options.styled p [ Typo.display2 ] [ text "Night Round" ]
        , Lists.ul [] (List.map (playerItem model) (Dict.values model.players))
        , Button.render Mdl
            []
            model.mdl
            [ Options.onClick ToDay
            , Button.primary
            , Button.raised
            , Options.css "margin-right" "20px"
            ]
            [ text "In the morning..." ]
        , Button.render Mdl
            []
            model.mdl
            [ Options.onClick EndGame, Button.accent, Button.raised ]
            [ text "Game Over" ]
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
