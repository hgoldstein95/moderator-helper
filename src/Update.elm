module Update exposing (..)

import Model exposing (..)


type Msg
    = Changed String
    | Reset
    | Add


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Changed word ->
            ( { model | curr = word }, Cmd.none )

        Reset ->
            ( { model | items = [], curr = "" }, Cmd.none )

        Add ->
            ( { model
                | items = model.items ++ [ item model.uid model.curr ]
                , curr = ""
                , uid = model.uid + 1
              }
            , Cmd.none
            )
