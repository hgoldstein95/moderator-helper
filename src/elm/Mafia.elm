module Mafia exposing (..)

import Dict exposing (Dict)
import Model exposing (..)


roleInfo : Role -> RoleInfo
roleInfo role =
    case role of
        Townie ->
            RoleInfo "Townie" False T []

        Sheriff ->
            RoleInfo "Sheriff" True T [ Check ]

        Doctor ->
            RoleInfo "Doctor" True T [ Save ]

        Mafia ->
            RoleInfo "Mafia" False M [ Kill ]

        Godfather ->
            RoleInfo "Godfather" True M [ Kill ]


getOutcomes :
    List Player
    -> Dict Int (List ( Player, Action ))
    -> List Outcome
getOutcomes players visits =
    Dict.toList visits
        |> List.filterMap
            (\( id, vs ) ->
                let
                    acts =
                        List.map Tuple.second vs
                in
                    if List.member Kill acts && not (List.member Save acts) then
                        players
                            |> List.filter (\p -> p.id == id)
                            |> List.head
                            |> Maybe.map Dead
                    else
                        Nothing
            )
