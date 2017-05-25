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
import Material.Typography as Typo
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
        |> Material.Scheme.topWithScheme Color.DeepOrange Color.Blue


addRoleButton : Model -> Role -> Html Msg
addRoleButton model role =
    Button.render Mdl
        []
        model.mdl
        [ Options.css "margin" "0 20px 10px 0"
        , Button.accent
        , Button.raised
        , Options.onClick (AddPlayer role)
        ]
        [ text (toString role) ]


roleItem : Player -> Html Msg
roleItem p =
    Lists.li
        []
        [ Lists.content [] [ text (toString p.role) ]
        , Lists.content2
            [ Options.onClick (RemovePlayer p) ]
            [ Icon.i "close" ]
        ]


viewCreate : Model -> Html Msg
viewCreate model =
    Options.div []
        [ Options.styled p [ Typo.display2 ] [ text "Create a Setup" ]
        , Options.div [] (List.map (addRoleButton model) model.roles)
        , Lists.ul [] (List.map roleItem model.setup)
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
    let
        info =
            roleInfo p.role
    in
        Lists.li
            [ Options.onClick (Visit p) ]
            [ Lists.content []
                [ span [] [ text <| (toString p.id) ++ " " ++ info.name ] ]
            , Lists.content2 [] (List.map (makeActBtn model p) info.actions)
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
        , Options.div [] [ text (toString model.visiting) ]
        , Options.div [] [ text (toString model.visited) ]
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
