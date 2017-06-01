module Model exposing (..)

import Dict exposing (Dict)
import Material


type Role
    = Townie
    | Sheriff
    | Doctor
    | Milkman
    | Mafia
    | Godfather


type alias RoleInfo =
    { unique : Bool
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
    | GiveMilk


type Outcome
    = Dead Player
    | GotMilk Player


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
    [ Townie, Sheriff, Doctor, Milkman, Mafia, Godfather ]


init : ( Model, Cmd msg )
init =
    ( { uid = 0
      , roles = tier1
      , mdl = Material.model
      , state = Create
      , players = []
      , visiting = Nothing
      , visited = Dict.empty
      , announcements = []
      }
    , Cmd.none
    )
