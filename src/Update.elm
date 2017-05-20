module Update exposing (..)

import Model exposing (..)


type Msg
    = ToCreate
    | AddPlayer Role
    | ToNight
    | ToDay
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToCreate ->
            ( { model | viewState = Create }, Cmd.none )

        AddPlayer role ->
            ( { model
                | setup = model.setup ++ [ Player model.uid role ]
                , uid = model.uid + 1
              }
            , Cmd.none
            )

        ToNight ->
            ( { model | viewState = Night }, Cmd.none )

        ToDay ->
            ( { model | viewState = Day }, Cmd.none )

        Reset ->
            ( { model | viewState = End, setup = [] }, Cmd.none )
