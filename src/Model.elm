module Model exposing (..)


type alias Item =
    { id : Int
    , content : String
    }


item : Int -> String -> Item
item n s =
    Item n s


type alias Model =
    { uid : Int
    , curr : String
    , items : List Item
    }


init : ( Model, Cmd msg )
init =
    ( Model 0 "" [], Cmd.none )
