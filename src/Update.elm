module Update exposing (..)

import Dict exposing (Dict)
import Model exposing (..)


type Msg
    = Reset
    | ToNight
    | ToDay
    | EndGame
    | AddRole Role


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

        AddRole role ->
            let
                r =
                    toString role

                count =
                    Maybe.withDefault 0 (Dict.get r model.setup)
            in
                ( { model
                    | setup = Dict.insert r (count + 1) model.setup
                  }
                , Cmd.none
                )
