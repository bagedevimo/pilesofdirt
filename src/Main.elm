module Main exposing (main)

import Browser
import Html exposing (Html, button, div, hr, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import List.Extra
import NES
import Round
import Time


impossibleProject : Project
impossibleProject =
    Project "end of the game or something?" 0 9999999.9 0.0


projects : List Project
projects =
    [ Project "Zen garden" 20 0.15 0.0
    , Project "Small garden" 300 0.35 0.0
    , Project "Large garden" 800 1.1 0.0
    , Project "Kids playgroud" 2000 3.2 0.0
    , Project "Cul de sac" 5000 9.3 0.0
    ]


nextProject : Int -> Project
nextProject numberOfProjectCompletions =
    Maybe.withDefault impossibleProject (List.Extra.getAt (floor (toFloat numberOfProjectCompletions / 10.0)) projects)


type alias Project =
    { name : String
    , payment : Int
    , amountOfDirtRequired : Float
    , amountOfDirtDelivered : Float
    }


type alias Model =
    { lastTick : Maybe Time.Posix
    , amountOfDirt : Float
    , numberOfWorkers : Int
    , dirtPerAction : Float
    , dirtPerManualAction : Float
    , actionSpeed : Float
    , currency : Int
    , workerCost : Int
    , dirtCost : Int
    , currentProject : Project
    , projectCompletions : Int
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { amountOfDirt = 1.0
      , numberOfWorkers = 0
      , dirtPerAction = 0.01
      , dirtPerManualAction = 0.01
      , actionSpeed = 5000.0
      , lastTick = Nothing
      , currency = 0
      , workerCost = 100
      , dirtCost = 100
      , currentProject = nextProject 0
      , projectCompletions = 0
      }
    , Cmd.none
    )


type Msg
    = MoveDirt
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

                projectRequredDirt =
                    model.currentProject.amountOfDirtRequired - model.currentProject.amountOfDirtDelivered

                proposedDirtDelivery =
                    min projectRequredDirt proposedMovingAmount

                currentProject =
                    model.currentProject

                updatedProject =
                    { currentProject | amountOfDirtDelivered = model.currentProject.amountOfDirtDelivered + proposedDirtDelivery }
            in
            ( { model | amountOfDirt = model.amountOfDirt - proposedDirtDelivery, currentProject = updatedProject }, Cmd.none )

        PurchaseWorker ->
            if model.currency >= model.workerCost then
                ( { model | numberOfWorkers = model.numberOfWorkers + 1, currency = model.currency - model.workerCost }, Cmd.none )

            else
                ( model, Cmd.none )

        BuyDirt ->
            if model.currency >= model.dirtCost then
                ( { model | amountOfDirt = model.amountOfDirt + 1, currency = model.currency - model.dirtCost }, Cmd.none )

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
                            toFloat model.numberOfWorkers * model.dirtPerAction * (durationInMs / model.actionSpeed)

                        proposedMovingAmount =
                            min model.amountOfDirt amountOfDirtCanMoveThisTick

                        projectRequiredDirt =
                            model.currentProject.amountOfDirtRequired - model.currentProject.amountOfDirtDelivered

                        proposedDirtDelivery =
                            min projectRequiredDirt proposedMovingAmount

                        currentProject =
                            model.currentProject
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
                                { currentProject | amountOfDirtDelivered = model.currentProject.amountOfDirtDelivered + proposedDirtDelivery }
                        in
                        ( { model | lastTick = Just now, amountOfDirt = model.amountOfDirt - proposedDirtDelivery, currentProject = updatedProject }, Cmd.none )


viewActions : Model -> Html Msg
viewActions model =
    NES.container "Actions"
        [ button [ onClick MoveDirt ] [ text "Move some dirt" ]
        , button [ onClick PurchaseWorker, disabled (model.currency < model.workerCost) ] [ text ("Hire a Will ($" ++ String.fromInt model.workerCost ++ ")") ]
        , button [ onClick BuyDirt, disabled (model.currency < model.dirtCost) ] [ text ("Buy 1m^3 of dirt ($" ++ String.fromInt model.dirtCost ++ ")") ]
        ]


viewStatus : Model -> Html Msg
viewStatus model =
    NES.container "You"
        [ div [] [ text ("Amount of dirt: " ++ Round.roundCom 2 model.amountOfDirt ++ " m^3") ]
        , div [] [ text ("Number of Wills: " ++ String.fromInt model.numberOfWorkers) ]
        , div [] [ text ("Money: " ++ String.fromInt model.currency) ]
        ]


viewProject : Project -> Html Msg
viewProject project =
    NES.container "Project"
        [ div [] [ text ("Current project: Flatten a " ++ project.name ++ ": (+$" ++ String.fromInt project.payment ++ ")") ]
        , div [] [ text ("Dirt required: " ++ Round.roundCom 2 project.amountOfDirtRequired) ]
        , div [] [ text ("Dirt delivered: " ++ Round.roundCom 2 project.amountOfDirtDelivered) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.h1 [] [ text "Piles of dirt" ]
        , viewActions model
        , Html.br [] []
        , viewStatus model
        , Html.br [] []
        , viewProject model.currentProject
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
