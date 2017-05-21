module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
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


roleInput : Role -> Html Msg
roleInput role =
    div []
        [ span [] [ text role.name ]
        , button [ onClick (IncRole role) ] [ text "+" ]
        , button [ onClick (DecRole role) ] [ text "-" ]
        ]


viewCreate : Model -> Html Msg
viewCreate model =
    div []
        [ h2 [] [ text "Create" ]
        , div [] (List.map roleInput <| Dict.values roles)
        , button [ onClick Start ] [ text "Start game!" ]
        , text (toString model.setup)
        ]


playerItem : Player -> Html Msg
playerItem p =
    let
        actBtns acts =
            List.map
                (\a ->
                    button
                        [ onClick (Act p a) ]
                        [ text (toString a) ]
                )
                acts
    in
        li []
            [ div []
                ([ span
                    [ onClick (Visit p) ]
                    [ text p.role.name ]
                 ]
                    ++ actBtns p.role.actions
                )
            ]


showVisited : Model -> Html Msg
showVisited model =
    ul [] <|
        List.map
            (\( t, s, a ) ->
                li [] [ text <| (toString t) ++ " -> " ++ (toString t) ]
            )
            model.visited


viewNight : Model -> Html Msg
viewNight model =
    div []
        [ h2 [] [ text "Night" ]
        , button [ onClick ToDay ] [ text "In the morning..." ]
        , button [ onClick EndGame ] [ text "Game Over" ]
        , ul [] (List.map playerItem model.players)
        , span [] [ text (toString model.visiting) ]
        , showVisited model
        ]


visitList : List ( Player, Player, Action ) -> List ( Player, List Action )
visitList l =
    case l of
        [] ->
            []

        ( r, _, act ) :: ps ->
            let
                ( match, rest ) =
                    List.partition (\( x, _, _ ) -> r.id == x.id) ps
            in
                ( r, act :: (List.map (\( _, _, x ) -> x) match) )
                    :: visitList rest


makeAnnouncements : List ( Player, Player, Action ) -> List String
makeAnnouncements visits =
    visitList visits
        |> List.filterMap
            (\( p, acts ) ->
                if List.member Kill acts && not (List.member Save acts) then
                    Just <| (toString p) ++ " is Dead"
                else
                    Nothing
            )


viewDay : Model -> Html Msg
viewDay model =
    div []
        [ h2 [] [ text "Day" ]
        , button [ onClick ToNight ] [ text "Go to sleep." ]
        , button [ onClick EndGame ] [ text "Game Over" ]
        , h3 [] [ text "Announcements" ]
        , ul []
            (makeAnnouncements model.visited
                |> List.map (\x -> li [] [ text x ])
            )
        ]


viewEnd : Model -> Html Msg
viewEnd model =
    div []
        [ h2 [] [ text "End" ]
        , button [ onClick Reset ] [ text "New game." ]
        ]
