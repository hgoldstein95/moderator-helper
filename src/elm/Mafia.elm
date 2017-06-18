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
            , priority = None
            }

        Sheriff ->
            { unique = True
            , alignment = T
            , actions = [ Check ]
            , priority = Low
            }

        Doctor ->
            { unique = True
            , alignment = T
            , actions = [ Save ]
            , priority = Low
            }

        Milkman ->
            { unique = True
            , alignment = T
            , actions = [ GiveMilk ]
            , priority = Low
            }

        Tracker ->
            { unique = True
            , alignment = T
            , actions = [ Check ]
            , priority = High
            }

        Mafia ->
            { unique = False
            , alignment = M
            , actions = [ Kill ]
            , priority = Med
            }

        Godfather ->
            { unique = True
            , alignment = M
            , actions = [ Kill ]
            , priority = Med
            }


playerOutcomes : List Action -> Player -> List Outcome
playerOutcomes acts player =
    List.concat
        [ if List.member Kill acts && not (List.member Save acts) then
            [ Dead player ]
          else
            []
        , if List.member GiveMilk acts then
            [ GotMilk player ]
          else
            []
        ]


getOutcomes :
    List Player
    -> Dict Int (List ( Player, Action ))
    -> List Outcome
getOutcomes players visits =
    Dict.toList visits
        |> List.filterMap
            (\( id, vs ) ->
                players
                    |> List.filter (\p -> p.id == id)
                    |> List.head
                    |> Maybe.map (playerOutcomes (List.map Tuple.second vs))
            )
        |> List.concat
