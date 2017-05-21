module Mafia exposing (..)

import Dict exposing (Dict)
import Model exposing (..)


roles : Dict String Role
roles =
    Dict.empty
        |> Dict.insert "Townie" (Role "Townie" False T [])
        |> Dict.insert "Sheriff" (Role "Sheriff" True T [ Check ])
        |> Dict.insert "Doctor" (Role "Doctor" True T [ Save ])
        |> Dict.insert "Mafia" (Role "Mafia" False M [ Kill ])
        |> Dict.insert "Godfather" (Role "Godfather" True M [ Kill ])


visitList : List ( Player, Player, Action ) -> List ( Player, List Action )
visitList l =
    case l of
        [] ->
            []

        ( r, _, act ) :: ps ->
            let
                ( match, rest ) =
                    List.partition (\( x, _, _ ) -> r.id == x.id) ps

                actList =
                    act :: (List.map (\( _, _, x ) -> x) match)
            in
                ( r, actList ) :: visitList rest


getOutcomes : List ( Player, Player, Action ) -> List Outcome
getOutcomes visits =
    visitList visits
        |> List.filterMap
            (\( p, acts ) ->
                if List.member Kill acts && not (List.member Save acts) then
                    Just (Dead p)
                else
                    Nothing
            )
