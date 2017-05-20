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


viewCreate : Model -> Html Msg
viewCreate model =
    div []
        [ text "Create"
        , button [ onClick ToNight ] [ text "To Night" ]
        ]


viewNight : Model -> Html Msg
viewNight model =
    div []
        [ text "Night"
        , button [ onClick ToDay ] [ text "To Day" ]
        ]


viewDay : Model -> Html Msg
viewDay model =
    div []
        [ text "Day"
        , button [ onClick ToEnd ] [ text "To End" ]
        ]


viewEnd : Model -> Html Msg
viewEnd model =
    div []
        [ text "End"
        , button [ onClick ToCreate ] [ text "To Start" ]
        ]
