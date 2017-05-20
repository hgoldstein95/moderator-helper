module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import EnterEvent exposing (onEnter)
import Model exposing (..)
import Update exposing (..)


rootStyle : Attribute Msg
rootStyle =
    style
        [ ( "width", "600px" )
        , ( "margin", "100px auto" )
        , ( "font-family", "sans-serif" )
        ]


makeItem : Item -> Html Msg
makeItem x =
    li [] [ text x.content ]


view : Model -> Html Msg
view model =
    div [ rootStyle ]
        [ h1 [] [ text "Test Application" ]
        , input
            [ type_ "text"
            , placeholder "Write Here"
            , value model.curr
            , onInput Changed
            , onEnter Add
            ]
            []
        , button [ onClick Add ] [ text "Add" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , ul
            [ style
                [ ( "list-style", "none" )
                ]
            ]
            (List.map makeItem model.items)
        ]
