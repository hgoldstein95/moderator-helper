module Model exposing (..)

import Dict exposing (Dict)
import Material


type Role
    = Townie
    | Sheriff
    | Doctor
    | Mafia
    | Godfather


type alias RoleInfo =
    { name : String
    , unique : Bool
    , alignment : Alignment
    , actions : List Action
    }


type ViewState
    = Create
    | Night
    | Day
    | End


type alias Player =
    { id : Int
    , role : Role
    , name : String
    , alive : Bool
    }


type Alignment
    = T
    | M


type Action
    = Kill
    | Save
    | Check


type Outcome
    = Dead Player


type alias Model =
    { uid : Int
    , roles : List Role
    , mdl : Material.Model
    , state : ViewState
    , players : List Player
    , visiting : Maybe ( Player, Action )
    , visited : Dict Int (List ( Player, Action ))
    , announcements : List Outcome
    }


tier1 : List Role
tier1 =
    [ Townie, Sheriff, Doctor, Mafia, Godfather ]


init : ( Model, Cmd msg )
init =
    ( Model
        0
        tier1
        Material.model
        Create
        []
        Nothing
        Dict.empty
        []
    , Cmd.none
    )
