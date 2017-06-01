module Mafia exposing (..)

import Dict exposing (Dict)
import Model exposing (..)


roleInfo : Role -> RoleInfo
roleInfo role =
    case role of
        Townie ->
            { unique = False
            , alignment = T
            , actions = []
            }

        Sheriff ->
            { unique = True
            , alignment = T
            , actions = [ Check ]
            }

        Doctor ->
            { unique = True
            , alignment = T
            , actions = [ Save ]
            }

        Mafia ->
            { unique = False
            , alignment = M
            , actions = [ Kill ]
            }

        Godfather ->
            { unique = True
            , alignment = M
            , actions = [ Kill ]
            }


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
