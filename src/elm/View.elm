module View exposing (..)

import Html exposing (Html, text)
import Dict exposing (Dict)
import Json.Decode as Json
import Material.Scheme
import Material.Button as Button
import Material.Options as Options
import Material.Color as Color
import Material.List as Lists
import Material.Icon as Icon
import Material.Textfield as Textfield
import Material.Layout as Layout
import Material.Grid as Grid
import Model exposing (..)
import Update exposing (..)
import Mafia exposing (..)


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        []
        { header =
            [ Layout.row [] [ Layout.title [] [ text "Mafia App" ] ] ]
        , drawer = []
        , tabs = ( [], [] )
        , main =
            [ Grid.grid []
                [ Grid.cell
                    [ Grid.offset Grid.Desktop 3
                    , Grid.size Grid.Desktop 6
                    , Grid.size Grid.Tablet 12
                    , Grid.size Grid.Phone 12
                    ]
                    [ case model.state of
                        Create ->
                            viewCreate model

                        Night ->
                            viewNight model

                        Day ->
                            viewDay model

                        End ->
                            viewEnd model
                    ]
                ]
            ]
        }
        |> Material.Scheme.topWithScheme Color.BlueGrey Color.Blue


displayPlayer : Player -> String
displayPlayer p =
    if p.name /= "" then
        String.join "" [ (toString p.role), " (", p.name, ")" ]
    else if not (roleInfo p.role).unique then
        String.join "" [ (toString p.role), " (", (toString p.id), ")" ]
    else
        toString p.role


displayAction : Action -> String
displayAction act =
    case act of
        Kill ->
            "Kill"

        Save ->
            "Save"

        Check ->
            "Check"

        GiveMilk ->
            "Give Milk"


priorityClass : Priority -> Options.Property c m
priorityClass priority =
    case priority of
        High ->
            Options.cs "high-priority"

        Med ->
            Options.cs "med-priority"

        Low ->
            Options.cs "low-priority"

        None ->
            Options.nop


addRoleButton : Model -> Role -> Html Msg
addRoleButton model role =
    Button.render Mdl
        []
        model.mdl
        [ Button.accent
        , Button.raised
        , Options.onClick (AddPlayer role)
        , Options.css "margin" "0 20px 20px 0"
        , Button.disabled
            |> Options.when
                ((roleInfo role).unique
                    && List.any (\p -> p.role == role) model.players
                )
        ]
        [ text (toString role) ]


roleItem : Model -> Player -> Html Msg
roleItem model p =
    Lists.li
        []
        [ Lists.content []
            [ Options.span
                [ Options.css "margin-right" "20px" ]
                [ text (toString p.role) ]
            , Textfield.render Mdl
                [ p.id ]
                model.mdl
                [ Options.onInput (UpdatePlayerName p)
                , Textfield.value p.name
                , Textfield.label "Name"
                ]
                []
            ]
        , Lists.content2
            [ Options.onClick (RemovePlayer p) ]
            [ Icon.i "close" ]
        ]


viewCreate : Model -> Html Msg
viewCreate model =
    Options.div []
        [ Html.h2 [] [ text "Create a Setup" ]
        , Options.div [] (List.map (addRoleButton model) model.roles)
        , Lists.ul [] (List.map (roleItem model) model.players)
        , Button.render Mdl
            []
            model.mdl
            [ Button.colored, Button.raised, Options.onClick Start ]
            [ text "Start" ]
        ]


makeActBtn : Model -> Player -> Action -> Html Msg
makeActBtn model p a =
    Button.render Mdl
        []
        model.mdl
        [ Options.onWithOptions
            "click"
            { stopPropagation = True
            , preventDefault = False
            }
            (Json.succeed (Act p a))
        , Button.colored
            |> Options.when
                (model.visiting
                    |> Maybe.map (\( play, act ) -> p.id == play.id && act == a)
                    |> Maybe.withDefault False
                )
        ]
        [ text (displayAction a) ]


playerItem : Model -> Player -> Html Msg
playerItem model p =
    Lists.li
        [ Options.onClick (Visit p) ]
        [ Lists.content
            []
            [ Options.div
                [ Options.cs "priority"
                , priorityClass (roleInfo p.role).priority
                ]
                []
            , Options.span [] [ text (displayPlayer p) ]
            ]
        , Lists.content2 []
            (List.map (makeActBtn model p) (roleInfo p.role).actions)
        ]


visitItem : Player -> Player -> Action -> Html Msg
visitItem currPlayer otherPlayer act =
    Lists.li [ Lists.withSubtitle ]
        [ Lists.content []
            [ Options.span
                [ Options.css "margin-right" "10px" ]
                [ displayPlayer otherPlayer
                    ++ " visits "
                    ++ (displayPlayer currPlayer)
                    |> text
                ]
            , Lists.subtitle [] [ text (displayAction act) ]
            ]
        , Lists.content2
            [ Options.onClick (RemoveVisit currPlayer otherPlayer act) ]
            [ Icon.i "close" ]
        ]


visitedItem : Model -> ( Int, List ( Player, Action ) ) -> List (Html Msg)
visitedItem model ( id, acts ) =
    model.players
        |> List.filter (\x -> x.id == id)
        |> List.head
        |> Maybe.map (\c -> List.map (\( o, a ) -> visitItem c o a) acts)
        |> Maybe.withDefault []


viewNight : Model -> Html Msg
viewNight model =
    Options.div []
        [ Html.h2 [] [ text "Night Round" ]
        , Lists.ul [] (List.map (playerItem model) model.players)
        , Html.hr [] []
        , Lists.ul []
            (model.visited
                |> Dict.toList
                |> List.concatMap (visitedItem model)
            )
        , Button.render Mdl
            []
            model.mdl
            [ Options.onClick ToDay
            , Button.primary
            , Button.raised
            , Options.css "margin-right" "20px"
            ]
            [ text "In the morning..." ]
        , Button.render Mdl
            []
            model.mdl
            [ Options.onClick EndGame, Button.accent, Button.raised ]
            [ text "Game Over" ]
        ]


announcementItem : Outcome -> Html Msg
announcementItem x =
    case x of
        Dead p ->
            Lists.li []
                [ Lists.content [] [ text (displayPlayer p ++ " is dead.") ] ]

        GotMilk p ->
            Lists.li []
                [ Lists.content [] [ text (displayPlayer p ++ " got milk.") ] ]


viewDay : Model -> Html Msg
viewDay model =
    Options.div []
        [ Html.h2 [] [ text "Day Round" ]
        , Button.render Mdl
            []
            model.mdl
            [ Options.onClick ToNight
            , Button.primary
            , Button.raised
            , Options.css "margin-right" "20px"
            ]
            [ text "Go to Sleep" ]
        , Button.render Mdl
            []
            model.mdl
            [ Options.onClick EndGame, Button.accent, Button.raised ]
            [ text "Game Over" ]
        , Html.h3 [] [ text "Announcements" ]
        , Lists.ul []
            (List.map
                announcementItem
                model.announcements
            )
        ]


viewEnd : Model -> Html Msg
viewEnd model =
    Options.div []
        [ Html.h2 [] [ text "Game Over" ]
        , Button.render Mdl
            []
            model.mdl
            [ Options.onClick Reset, Button.primary, Button.raised ]
            [ text "New Game" ]
        ]
