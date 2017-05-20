module Model exposing (..)


type ViewState
    = Create
    | Night
    | Day
    | End


type alias Model =
    { uid : Int
    , viewState : ViewState
    }


init : ( Model, Cmd msg )
init =
    ( Model 0 Create, Cmd.none )
