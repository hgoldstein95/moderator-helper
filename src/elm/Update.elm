module Update exposing (..)

import Dict exposing (Dict)
import Material
import Model exposing (..)
import Mafia exposing (..)


type Msg
    = Reset
    | Mdl (Material.Msg Msg)
    | Start
    | ToNight
    | ToDay
    | EndGame
    | IncRole Role
    | DecRole Role
    | Act Player Action
    | Visit Player


incrRole : Role -> Dict String Int -> Dict String Int
incrRole role d =
    if role.unique then
        Dict.insert role.name 1 d
    else
        let
            count =
                Maybe.withDefault 0 (Dict.get role.name d)
        in
            Dict.insert role.name (count + 1) d


decrRole : Role -> Dict String Int -> Dict String Int
decrRole role d =
    let
        count =
            Maybe.withDefault 0 (Dict.get role.name d)

        newVal =
            if count <= 0 then
                0
            else
                count - 1
    in
        Dict.insert role.name newVal d


makePlayers : Dict String Int -> Dict Int Player
makePlayers d =
    Dict.toList d
        |> List.concatMap (\( r, n ) -> List.repeat n r)
        |> List.filterMap (\r -> Dict.get r roles)
        |> List.indexedMap (\i x -> ( i, Player i x ))
        |> Dict.fromList


addVisit :
    Player
    -> Player
    -> Action
    -> Dict Int (List ( Player, Action ))
    -> Dict Int (List ( Player, Action ))
addVisit t s act v =
    let
        curr =
            Maybe.withDefault [] (Dict.get t.id v)
    in
        Dict.insert t.id (( s, act ) :: curr) v


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        Mdl msg ->
            Material.update Mdl msg model

        Start ->
            ( { model
                | state = Night
                , setup = Dict.empty
                , players = makePlayers model.setup
              }
            , Cmd.none
            )

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

        IncRole role ->
            ( { model | setup = incrRole role model.setup }
            , Cmd.none
            )

        DecRole role ->
            ( { model | setup = decrRole role model.setup }
            , Cmd.none
            )

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
