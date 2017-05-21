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
    | RemoveVisit ( Player, Player, Action )


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


makeAnnouncements : List ( Player, Player, Action ) -> List Outcome
makeAnnouncements visits =
    visitList visits
        |> List.filterMap
            (\( p, acts ) ->
                if List.member Kill acts && not (List.member Save acts) then
                    Just <| Dead p
                else
                    Nothing
            )


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
                , announcements = []
              }
            , Cmd.none
            )

        ToDay ->
            ( { model
                | state = Day
                , announcements = makeAnnouncements model.visited
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
                        , visited = ( player, p, act ) :: model.visited
                    }
            , Cmd.none
            )

        RemoveVisit badAct ->
            ( { model
                | visited = List.filter (\act -> act /= badAct) model.visited
              }
            , Cmd.none
            )
