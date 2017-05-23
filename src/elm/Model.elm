module Model exposing (..)

import Dict exposing (Dict)
import Material


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


type Outcome
    = Dead Player


type alias Model =
    { uid : Int
    , mdl : Material.Model
    , state : ViewState
    , setup : Dict String Int
    , players : Dict Int Player
    , visiting : Maybe ( Player, Action )
    , visited : Dict Int (List ( Player, Action ))
    , announcements : List Outcome
    }


init : ( Model, Cmd msg )
init =
    ( Model
        0
        Material.model
        Create
        Dict.empty
        Dict.empty
        Nothing
        Dict.empty
        []
    , Cmd.none
    )
