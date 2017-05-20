module Model exposing (..)


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


roleToString : Role -> String
roleToString role =
    case role of
        Townie ->
            "Townie"

        Sheriff ->
            "Sheriff"

        Doctor ->
            "Doctor"

        Mafia ->
            "Mafia"

        Godfather ->
            "Godfather"


type alias Player =
    { id : Int, role : Role }


type alias Model =
    { uid : Int
    , viewState : ViewState
    , setup : List Player
    }


init : ( Model, Cmd msg )
init =
    ( Model 0 Create [], Cmd.none )


allRoles : List Role
allRoles =
    [ Townie, Sheriff, Doctor, Mafia, Godfather ]
