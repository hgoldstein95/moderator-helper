module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Update exposing (..)


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "width", "600px" )
            , ( "margin", "100px auto" )
            , ( "font-family", "sans-serif" )
            ]
        ]
        [ h1 [] [ text "Mafia App" ]
        , case model.viewState of
            Create ->
                viewCreate model

            Night ->
                viewNight model

            Day ->
                viewDay model

            End ->
                viewEnd model
        ]


roleButton : Model -> Role -> Html Msg
roleButton model x =
    let
        count =
            List.filter (\p -> p.role == x) model.setup |> List.length
    in
        button
            [ onClick (AddPlayer x) ]
            [ text <| roleToString x ++ " (" ++ toString count ++ ")" ]


viewCreate : Model -> Html Msg
viewCreate model =
    div []
        [ h2 [] [ text "Create" ]
        , div []
            (List.map (roleButton model) allRoles)
        , button [ onClick ToNight ] [ text "Start game!" ]
        ]


viewNight : Model -> Html Msg
viewNight model =
    div []
        [ h2 [] [ text "Night" ]
        , button [ onClick ToDay ] [ text "In the morning..." ]
        , button [ onClick Reset ] [ text "Game Over" ]
        , ul []
            (List.map (\x -> li [] [ text (roleToString x.role) ]) model.setup)
        ]


viewDay : Model -> Html Msg
viewDay model =
    div []
        [ h2 [] [ text "Day" ]
        , button [ onClick ToNight ] [ text "Go to sleep." ]
        , button [ onClick Reset ] [ text "Game Over" ]
        , ul []
            (List.map (\x -> li [] [ text (roleToString x.role) ]) model.setup)
        ]


viewEnd : Model -> Html Msg
viewEnd model =
    div []
        [ h2 [] [ text "End" ]
        , button [ onClick ToCreate ] [ text "New game." ]
        ]
