module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import List.Extra
import NES
import Round
import Time


impossibleProject : Project
impossibleProject =
    Project "end of the game or something?" 0 999999999999 0


projects : List Project
projects =
    List.map (\( name, cm3, reward ) -> Project name reward (round (cm3 * 1000.0 * 1000000.0)) 0)
        [ ( "Setup a planter box", 0.2, 350 )
        , ( "Fill a garden bed", 1, 350 )
        , ( "Build a small garden", 3, 350 )
        , ( "Level a back yard", 8, 1100 )
        , ( "Prepare a site for playgroud", 20, 3200 )
        , ( "Break ground on a Cul de sac", 50, 9300 )
        ]


nextProject : Int -> Project
nextProject numberOfProjectCompletions =
    Maybe.withDefault impossibleProject (List.Extra.getAt (floor (toFloat numberOfProjectCompletions / 10.0)) projects)


type alias Project =
    { name : String
    , payment : Int
    , mm3Required : Int
    , mm3Delivered : Int
    }


remainingDirtRequired : Project -> Int
remainingDirtRequired project =
    project.mm3Required - project.mm3Delivered


type alias Model =
    { lastTick : Maybe Time.Posix
    , amountOfDirt : Int
    , numberOfWorkers : Int
    , dirtPerAction : Int
    , dirtPerManualAction : Int
    , actionSpeed : Int
    , currency : Int
    , currentProject : Project
    , projectCompletions : Int
    }


workerCost : Model -> Int
workerCost model =
    100 + (2 ^ model.numberOfWorkers) - 1


dirtCost : Model -> Int
dirtCost model =
    100


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { amountOfDirt = 0
      , numberOfWorkers = 0
      , dirtPerAction = 100000
      , dirtPerManualAction = 1000000
      , actionSpeed = 5000
      , lastTick = Nothing
      , currency = 0
      , currentProject = nextProject 0
      , projectCompletions = 0
      }
    , Cmd.none
    )


type Msg
    = MoveDirt
    | GatherDirt
    | PurchaseWorker
    | BuyDirt
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveDirt ->
            let
                proposedMovingAmount =
                    min model.amountOfDirt model.dirtPerManualAction

                currentProject =
                    model.currentProject

                projectRequredDirt =
                    remainingDirtRequired currentProject

                proposedDirtDelivery =
                    min projectRequredDirt proposedMovingAmount

                updatedProject =
                    { currentProject | mm3Delivered = model.currentProject.mm3Delivered + proposedDirtDelivery }
            in
            ( { model | amountOfDirt = model.amountOfDirt - proposedDirtDelivery, currentProject = updatedProject }, Cmd.none )

        GatherDirt ->
            ( { model | amountOfDirt = model.amountOfDirt + model.dirtPerManualAction }, Cmd.none )

        PurchaseWorker ->
            if model.currency >= workerCost model then
                ( { model | numberOfWorkers = model.numberOfWorkers + 1, currency = model.currency - workerCost model }, Cmd.none )

            else
                ( model, Cmd.none )

        BuyDirt ->
            if model.currency >= dirtCost model then
                ( { model | amountOfDirt = model.amountOfDirt + 1, currency = model.currency - dirtCost model }, Cmd.none )

            else
                ( model, Cmd.none )

        Tick now ->
            case model.lastTick of
                Nothing ->
                    ( { model | lastTick = Just now }, Cmd.none )

                Just lastPosix ->
                    let
                        durationInMs =
                            toFloat (Time.posixToMillis now - Time.posixToMillis lastPosix)

                        amountOfDirtCanMoveThisTick =
                            round (toFloat (model.numberOfWorkers * model.dirtPerAction) * durationInMs / toFloat model.actionSpeed)

                        proposedMovingAmount =
                            min model.amountOfDirt amountOfDirtCanMoveThisTick

                        currentProject =
                            model.currentProject

                        projectRequiredDirt =
                            remainingDirtRequired currentProject

                        proposedDirtDelivery =
                            min projectRequiredDirt proposedMovingAmount
                    in
                    if proposedDirtDelivery <= 0 && model.amountOfDirt > 0 && projectRequiredDirt <= 0 then
                        let
                            updatedProject =
                                nextProject (model.projectCompletions + 1)
                        in
                        ( { model | lastTick = Just now, currentProject = updatedProject, currency = model.currency + currentProject.payment, projectCompletions = model.projectCompletions + 1 }, Cmd.none )

                    else
                        let
                            updatedProject =
                                { currentProject | mm3Delivered = model.currentProject.mm3Delivered + proposedDirtDelivery }
                        in
                        ( { model | lastTick = Just now, amountOfDirt = model.amountOfDirt - proposedDirtDelivery, currentProject = updatedProject }, Cmd.none )


viewActions : Model -> Html Msg
viewActions model =
    NES.container "Actions"
        [ class "is-height-2" ]
        [ button [ onClick GatherDirt ] [ text "Scrounge some dirt" ]
        , button [ onClick MoveDirt, disabled (model.amountOfDirt < model.dirtPerManualAction) ] [ text "Move some dirt" ]
        , button [ onClick PurchaseWorker, disabled (model.currency < workerCost model) ] [ text ("Hire a Will ($" ++ String.fromInt (workerCost model) ++ ")") ]
        , button [ onClick BuyDirt, disabled (model.currency < dirtCost model) ]
            [ text "Buy 1"
            , text "m"
            , Html.sup [] [ text "3" ]
            , text (" of dirt ($" ++ String.fromInt (dirtCost model) ++ ")")
            ]
        ]


viewStatus : Model -> Html Msg
viewStatus model =
    NES.container "You"
        [ class "is-span-2" ]
        [ div [] (viewDirtVolume "Amount of dirt: " model.amountOfDirt)
        , div [] [ text ("Number of Wills: " ++ String.fromInt model.numberOfWorkers) ]
        , div [] [ text ("Money: " ++ String.fromInt model.currency) ]
        ]


dirtVolume : Int -> String
dirtVolume volume =
    Round.round 5 <| toFloat volume / 1000.0 / 1000000.0


viewDirtVolume : String -> Int -> List (Html msg)
viewDirtVolume leading volume =
    [ text (leading ++ dirtVolume volume ++ "m"), Html.sup [] [ text "3" ] ]


viewProject : Project -> Html Msg
viewProject project =
    NES.container "Project"
        [ class "is-span-2" ]
        [ div [] [ text ("Current project: " ++ project.name ++ ": (+$" ++ String.fromInt project.payment ++ ")") ]
        , div [] (viewDirtVolume "Dirt required: " (remainingDirtRequired project))
        , Html.br [] []
        , NES.progress ((toFloat project.mm3Delivered / toFloat project.mm3Required) * 100)
        ]


viewStats : Model -> Html Msg
viewStats model =
    NES.container "Nerds"
        [ class "is-span-2 is-height-2" ]
        [ div [] (viewDirtVolume "Dirt per manual action: " model.dirtPerManualAction)
        , div [] (viewDirtVolume "Dirt per Will action: " model.dirtPerAction)
        , div [] [ text ("Will actions take: " ++ Round.round 3 (toFloat model.actionSpeed / 1000.0) ++ "s") ]
        ]


viewUpgrades : Model -> Html Msg
viewUpgrades model =
    NES.container "Upgrades" [ class "is-upgrades" ] []


viewVisualiser : Model -> Html Msg
viewVisualiser model =
    NES.container "Visualiser" [ class "is-height-2" ] []


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [] [ Html.h1 [] [ text "Piles of dirt" ] ]
        , viewActions model
        , viewStatus model
        , viewProject model.currentProject
        , viewUpgrades model
        , viewStats model
        , viewVisualiser model
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
