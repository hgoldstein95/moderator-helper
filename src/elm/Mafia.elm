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


getOutcomes :
    Dict Int Player
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
                    if
                        List.member Kill acts
                            && not (List.member Save acts)
                    then
                        Maybe.map Dead (Dict.get id players)
                    else
                        Nothing
            )
