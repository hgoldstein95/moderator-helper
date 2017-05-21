module Model exposing (..)

import Dict exposing (Dict)


type ViewState
    = Create
    | Night
    | Day
    | End


type Role
    = Townie
    | Sheriff
    | Doctor
    | Mafia
    | Godfather


allRoles : List Role
allRoles =
    [ Townie, Sheriff, Doctor, Mafia, Godfather ]


type alias Model =
    { uid : Int
    , viewState : ViewState
    , setup : Dict String Int
    }


init : ( Model, Cmd msg )
init =
    ( Model 0 Create Dict.empty, Cmd.none )
