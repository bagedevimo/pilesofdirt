module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import List.Extra
import NES
import Round
import Time



-- STANDING


impossibleProject : Project
impossibleProject =
    Project "end of the game or something?" 0 999999999999 0 99999999


makeProject : String -> Float -> Int -> Int -> Project
makeProject name m3 reward requiredCompletions =
    Project name reward (round (m3 * 1000.0 * 1000000.0)) 0 requiredCompletions


projects : List Project
projects =
    [ makeProject "Plant a flower pot" 0.035 80 5
    , makeProject "Setup a planter box" 0.2 350 10
    , makeProject "Fill a garden bed" 1 350 20
    , makeProject "Build a small garden" 3 350 20
    , makeProject "Level a back yard" 8 1100 20
    , makeProject "Prepare a site for Playgroud" 20 3200 20
    , makeProject "Break ground on a cul de sac" 50 9300 50
    ]


type alias UpgradeKey =
    String


upgrades : Dict UpgradeKey Upgrade
upgrades =
    Dict.fromList
        [ ( "SHMA1", Upgrade "Shovels" "Moving some dirt moves twice as much" False 50 2 0 )
        , ( "SHA1", Upgrade "Shovels for Wills" "Equip your Wills with shovels, tripe dirt moved per action" False 150 0 3 )
        , ( "SHMA2", Upgrade "Shovels" "Moving some dirt moves twice as much, again!" False 200 0 3 )
        , ( "SHA2", Upgrade "Shovels for Wills" "Equip your Wills with shovels, tripe dirt moved per action again" False 400 0 3 )
        ]



-- MODEL


type alias Upgrade =
    { name : String
    , description : String
    , purchased : Bool
    , price : Int
    , dirtPerManualActionMultiplier : Int
    , dirtPerActionMultiplier : Int
    }


type alias Project =
    { name : String
    , payment : Int
    , mm3Required : Int
    , mm3Delivered : Int
    , requiredCompletions : Int
    }


type alias Model =
    { lastTick : Maybe Time.Posix
    , amountOfDirt : Int
    , numberOfWorkers : Int
    , currency : Int
    , currentProject : Project
    , projectCompletions : Int
    , upgrades : Dict UpgradeKey Upgrade
    }


nextProject : Int -> Project
nextProject numberOfProjectCompletions =
    Maybe.withDefault impossibleProject (List.Extra.getAt (floor (toFloat numberOfProjectCompletions / 10.0)) projects)


remainingDirtRequired : Project -> Int
remainingDirtRequired project =
    project.mm3Required - project.mm3Delivered


workerCost : Model -> Int
workerCost model =
    100 + (2 ^ model.numberOfWorkers) - 1


dirtCost : Model -> Int
dirtCost _ =
    100


dirtPerAction : Model -> Int
dirtPerAction model =
    Dict.foldl (\_ upgrade curr -> curr * dirtPerActionMultiplier upgrade) 100000 model.upgrades


dirtPerManualAction : Model -> Int
dirtPerManualAction model =
    Dict.foldl (\_ upgrade curr -> curr * dirtPerManualActionMultiplier upgrade) 1000000 model.upgrades


dirtPerManualActionMultiplier : Upgrade -> Int
dirtPerManualActionMultiplier upgrade =
    if upgrade.purchased && upgrade.dirtPerManualActionMultiplier > 0 then
        upgrade.dirtPerManualActionMultiplier

    else
        1


dirtPerActionMultiplier : Upgrade -> Int
dirtPerActionMultiplier upgrade =
    if upgrade.purchased && upgrade.dirtPerActionMultiplier > 0 then
        upgrade.dirtPerActionMultiplier

    else
        1


actionSpeed : Model -> Int
actionSpeed _ =
    5000


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { amountOfDirt = 0
      , numberOfWorkers = 0
      , lastTick = Nothing
      , currency = 0
      , currentProject = nextProject 0
      , projectCompletions = 0
      , upgrades = upgrades
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MoveDirt
    | GatherDirt
    | PurchaseWorker
    | BuyDirt
    | BuyUpgrade UpgradeKey
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveDirt ->
            let
                proposedMovingAmount =
                    min model.amountOfDirt (dirtPerManualAction model)

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
            ( { model | amountOfDirt = model.amountOfDirt + dirtPerManualAction model }, Cmd.none )

        PurchaseWorker ->
            if model.currency >= workerCost model then
                ( { model | numberOfWorkers = model.numberOfWorkers + 1, currency = model.currency - workerCost model }, Cmd.none )

            else
                ( model, Cmd.none )

        BuyUpgrade key ->
            let
                updatedUpgrades =
                    Dict.update key
                        (Maybe.map
                            (\upgrade ->
                                if model.currency >= upgrade.price then
                                    { upgrade | purchased = True }

                                else
                                    upgrade
                            )
                        )
                        model.upgrades
            in
            ( { model | upgrades = updatedUpgrades }, Cmd.none )

        BuyDirt ->
            if model.currency >= dirtCost model then
                ( { model | amountOfDirt = model.amountOfDirt + 1 * 1000 * 1000000, currency = model.currency - dirtCost model }, Cmd.none )

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
                            round (toFloat (model.numberOfWorkers * dirtPerAction model) * durationInMs / toFloat (actionSpeed model))

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


canMoveDirt : Model -> Bool
canMoveDirt model =
    model.amountOfDirt >= dirtPerManualAction model


canPurchaseWorker : Model -> Bool
canPurchaseWorker model =
    model.currency >= workerCost model


canBuyDirt : Model -> Bool
canBuyDirt model =
    model.currency >= dirtCost model



-- VIEW


viewActionButton : Model -> Msg -> (Model -> Bool) -> List (Html Msg) -> Html Msg
viewActionButton model msg isEnabled body =
    NES.button
        (isEnabled model)
        [ onClick msg
        , not (isEnabled model) |> disabled
        ]
        body


viewActions : Model -> Html Msg
viewActions model =
    NES.container "Actions"
        [ class "is-height-2" ]
        [ viewActionButton model GatherDirt (\_ -> True) [ text "Scrounge some dirt" ]
        , Html.br [] []
        , Html.br [] []
        , viewActionButton model MoveDirt canMoveDirt [ text "Move some dirt" ]
        , Html.br [] []
        , Html.br [] []
        , viewActionButton model PurchaseWorker canPurchaseWorker [ text ("Hire a Will ($" ++ String.fromInt (workerCost model) ++ ")") ] -- [ onClick PurchaseWorker, disabled (model.currency < workerCost model) ] [ text ("Hire a Will ($" ++ String.fromInt (workerCost model) ++ ")") ]
        , Html.br [] []
        , Html.br [] []
        , viewActionButton model
            BuyDirt
            canBuyDirt
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
        [ div [] [ text (project.name ++ ": ($" ++ String.fromInt project.payment ++ ")") ]
        , div [] (viewDirtVolume "Dirt required: " (remainingDirtRequired project))
        , Html.br [] []
        , NES.progress ((toFloat project.mm3Delivered / toFloat project.mm3Required) * 100)
        ]


viewStats : Model -> Html Msg
viewStats model =
    NES.container "Nerds"
        [ class "is-span-2 is-height-2" ]
        [ div [] (viewDirtVolume "Dirt per manual action: " (dirtPerManualAction model))
        , div [] (viewDirtVolume "Dirt per Will action: " (dirtPerAction model))
        , div [] [ text ("Will actions take: " ++ Round.round 3 (toFloat (actionSpeed model) / 1000.0) ++ "s") ]
        ]


viewUpgrades : Model -> Html Msg
viewUpgrades model =
    NES.container "Upgrades"
        [ class "is-upgrades" ]
        (List.concatMap
            (\( key, upgrade ) ->
                [ Html.span [ class "nes-text is-primary" ] [ text upgrade.name ]
                , Html.br [] []
                , Html.span [ class "nes-text" ] [ text upgrade.description ]
                , Html.span [ class "nes-text is-warning" ] [ " ($" ++ String.fromInt upgrade.price ++ ")" |> text ]
                , Html.br [] []
                , NES.button (upgrade.price <= model.currency) [ onClick (BuyUpgrade key) ] [ text "Buy" ]
                , Html.br [] []
                , Html.br [] []
                ]
            )
            (Dict.toList model.upgrades |> List.filter (\( _, upgrade ) -> not upgrade.purchased) |> List.sortBy (\( _, upgrade ) -> upgrade.price))
        )


viewVisualiser : Model -> Html Msg
viewVisualiser model =
    NES.container "visualiser" [ class "is-height-2" ] []


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



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick



-- ENTRY


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
