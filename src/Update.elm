module Update exposing (..)

import Dict exposing (Dict)
import Model exposing (..)


type Msg
    = Reset
    | ToNight
    | ToDay
    | EndGame
    | IncRole Role
    | DecRole Role


modifyRole : Int -> Role -> Model -> Model
modifyRole n role model =
    let
        r =
            toString role

        count =
            Maybe.withDefault 0 (Dict.get r model.setup)

        newVal =
            if count + n >= 0 then
                count + n
            else
                count
    in
        { model | setup = Dict.insert r newVal model.setup }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | viewState = Create, setup = Dict.empty }, Cmd.none )

        ToNight ->
            ( { model | viewState = Night }, Cmd.none )

        ToDay ->
            ( { model | viewState = Day }, Cmd.none )

        EndGame ->
            ( { model | viewState = End }, Cmd.none )

        IncRole role ->
            ( modifyRole 1 role model, Cmd.none )

        DecRole role ->
            ( modifyRole (-1) role model, Cmd.none )
