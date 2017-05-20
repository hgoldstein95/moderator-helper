module Update exposing (..)

import Model exposing (..)


type Msg
    = ToCreate
    | ToNight
    | ToDay
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToCreate ->
            ( { model | viewState = Create }, Cmd.none )

        ToNight ->
            ( { model | viewState = Night }, Cmd.none )

        ToDay ->
            ( { model | viewState = Day }, Cmd.none )

        Reset ->
            ( { model | viewState = End }, Cmd.none )
