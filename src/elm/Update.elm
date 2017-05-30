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
    | RemoveVisit Player Player Action


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
                , players =
                    model.players
                        ++ [ { id = model.uid
                             , role = role
                             , name = ""
                             , alive = True
                             }
                           ]
              }
            , Cmd.none
            )

        UpdatePlayerName player name ->
            ( { model
                | players =
                    List.map
                        (\other ->
                            if other.id == player.id then
                                { other | name = name }
                            else
                                other
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

        Visit target ->
            ( case model.visiting of
                Nothing ->
                    model

                Just ( source, act ) ->
                    { model
                        | visiting = Nothing
                        , visited =
                            Dict.update target.id
                                (\curr ->
                                    ( source, act )
                                        :: Maybe.withDefault [] curr
                                        |> Just
                                )
                                model.visited
                    }
            , Cmd.none
            )

        RemoveVisit t s a ->
            ( { model
                | visited =
                    model.visited
                        |> Dict.update t.id
                            ((\( p, act ) -> p.id /= s.id || a /= act)
                                |> List.filter
                                |> Maybe.map
                            )
              }
            , Cmd.none
            )
