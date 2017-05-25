module Update exposing (..)

import Dict exposing (Dict)
import Material
import Model exposing (..)
import Mafia exposing (..)


type Msg
    = Reset
    | Mdl (Material.Msg Msg)
    | AddPlayer Role
    | UpdatePlayerName Player String
    | RemovePlayer Player
    | Start
    | ToNight
    | ToDay
    | EndGame
    | Act Player Action
    | Visit Player


addVisit :
    Player
    -> Player
    -> Action
    -> Dict Int (List ( Player, Action ))
    -> Dict Int (List ( Player, Action ))
addVisit t s act v =
    Dict.update t.id
        (\curr -> ( s, act ) :: (Maybe.withDefault [] curr) |> Just)
        v


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        Mdl msg ->
            Material.update Mdl msg model

        AddPlayer role ->
            ( { model
                | uid = model.uid + 1
                , players = model.players ++ [ Player model.uid role "" True ]
              }
            , Cmd.none
            )

        UpdatePlayerName player name ->
            ( { model
                | players =
                    List.map
                        (\p ->
                            if p.id == player.id then
                                { p | name = name }
                            else
                                p
                        )
                        model.players
              }
            , Cmd.none
            )

        RemovePlayer player ->
            ( { model
                | players = List.filter (\p -> p.id /= player.id) model.players
              }
            , Cmd.none
            )

        Start ->
            ( { model | state = Night }, Cmd.none )

        ToNight ->
            ( { model
                | state = Night
                , visited = Dict.empty
                , announcements = []
              }
            , Cmd.none
            )

        ToDay ->
            ( { model
                | state = Day
                , announcements = getOutcomes model.players model.visited
              }
            , Cmd.none
            )

        EndGame ->
            ( { model | state = End }, Cmd.none )

        Act player action ->
            ( case model.visiting of
                Nothing ->
                    { model | visiting = Just ( player, action ) }

                Just ( p, act ) ->
                    if p.id == player.id then
                        { model | visiting = Nothing }
                    else
                        { model | visiting = Just ( player, action ) }
            , Cmd.none
            )

        Visit player ->
            ( case model.visiting of
                Nothing ->
                    model

                Just ( p, act ) ->
                    { model
                        | visiting = Nothing
                        , visited = addVisit player p act model.visited
                    }
            , Cmd.none
            )
