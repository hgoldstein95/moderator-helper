module Model exposing (..)

import Dict exposing (Dict)


type ViewState
    = Create
    | Night
    | Day
    | End


type alias Player =
    { id : Int
    , role : String
    }


roles : List String
roles =
    [ "Townie"
    , "Sheriff"
    , "Doctor"
    , "Mafia"
    , "Godfather"
    ]


type alias Model =
    { uid : Int
    , state : ViewState
    , setup : Dict String Int
    , players : List Player
    }


init : ( Model, Cmd msg )
init =
    ( Model 0 Create Dict.empty [], Cmd.none )
