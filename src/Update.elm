module Update exposing (..)

import Dict exposing (Dict)
import Model exposing (..)


type Msg
    = Reset
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


makePlayers : Dict String Int -> List Player
makePlayers d =
    Dict.toList d
        |> List.concatMap (\( r, n ) -> List.repeat n r)
        |> List.filterMap (\r -> Dict.get r roles)
        |> List.indexedMap (\i x -> Player i x)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

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
                , visited = []
              }
            , Cmd.none
            )

        ToDay ->
            ( { model | state = Day }, Cmd.none )

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
                    { model
                        | visiting = Just ( player, action )
                    }

                Just _ ->
                    model
            , Cmd.none
            )

        Visit player ->
            ( case model.visiting of
                Nothing ->
                    model

                Just ( p, act ) ->
                    { model
                        | visiting = Nothing
                        , visited = ( player, p, act ) :: model.visited
                    }
            , Cmd.none
            )
