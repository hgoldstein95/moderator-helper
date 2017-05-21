module Update exposing (..)

import Dict exposing (Dict)
import Model exposing (..)


type Msg
    = Reset
    | Start
    | ToNight
    | ToDay
    | EndGame
    | IncRole String
    | DecRole String


modifyRole : Int -> String -> Model -> Model
modifyRole n role model =
    let
        count =
            Maybe.withDefault 0 (Dict.get role model.setup)

        newVal =
            if count + n >= 0 then
                count + n
            else
                count
    in
        { model | setup = Dict.insert role newVal model.setup }


makePlayers : Dict String Int -> List Player
makePlayers d =
    Dict.toList d
        |> List.concatMap (\( r, n ) -> List.repeat n r)
        |> List.indexedMap (\i x -> Player i x)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        Start ->
            ( { model
                | state = Night
                , players = makePlayers model.setup
              }
            , Cmd.none
            )

        ToNight ->
            ( { model | state = Night }, Cmd.none )

        ToDay ->
            ( { model | state = Day }, Cmd.none )

        EndGame ->
            ( { model | state = End }, Cmd.none )

        IncRole role ->
            ( modifyRole 1 role model, Cmd.none )

        DecRole role ->
            ( modifyRole (-1) role model, Cmd.none )
