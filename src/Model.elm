module Model exposing (..)

import Dict exposing (Dict)


type ViewState
    = Create
    | Night
    | Day
    | End


type alias Player =
    { id : Int
    , role : Role
    }


type Alignment
    = T
    | M
    | X


type Action
    = Kill
    | Save
    | Check


type alias Role =
    { name : String
    , unique : Bool
    , alignment : Alignment
    , actions : List Action
    }


roles : Dict String Role
roles =
    Dict.empty
        |> Dict.insert "Townie" (Role "Townie" False T [])
        |> Dict.insert "Sheriff" (Role "Sheriff" True T [ Check ])
        |> Dict.insert "Doctor" (Role "Doctor" True T [ Save ])
        |> Dict.insert "Mafia" (Role "Mafia" False M [ Kill ])
        |> Dict.insert "Godfather" (Role "Godfather" True M [ Kill ])


type alias Model =
    { uid : Int
    , state : ViewState
    , setup : Dict String Int
    , players : List Player
    , visiting : Maybe ( Player, Action )
    , visited : List ( Player, Player, Action )
    }


init : ( Model, Cmd msg )
init =
    ( Model 0 Create Dict.empty [] Nothing [], Cmd.none )
