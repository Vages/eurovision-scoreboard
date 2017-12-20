module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Europe exposing (..)
import Set exposing (Set)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Country =
    { name : String
    , abbreviation : String
    }


type alias Participant =
    { country : Country
    , points : Int
    }


type alias Model =
    { participants : List Participant
    , pointQueue : List Int
    , namesOfCountriesThatHaveGottenPointsInThisRound : Set String
    }


initialModel : Model
initialModel =
    let
        initialPoints =
            0

        initialPointQueue =
            [ 1, 2, 3, 4, 5, 6, 7, 8, 10, 12 ]

        initialParticipants =
            Europe.countries |> List.map (\country -> { country = country, points = initialPoints })
    in
        { participants = initialParticipants
        , pointQueue = initialPointQueue
        , namesOfCountriesThatHaveGottenPointsInThisRound = Set.empty
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type alias PointAward =
    { country : Country
    , points : Int
    }


type Msg
    = NoOp
    | GivePoints Country


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GivePoints country ->
            let
                givePointsToParticipantIfCountryHasSameName : PointAward -> Participant -> Participant
                givePointsToParticipantIfCountryHasSameName pointAward participant =
                    if pointAward.country == participant.country then
                        { participant | points = participant.points + pointAward.points }
                    else
                        participant

                pointsToBeGiven =
                    case List.head model.pointQueue of
                        Just points ->
                            points

                        _ ->
                            0

                rotatedPointQueue =
                    List.drop 1 model.pointQueue ++ List.take 1 model.pointQueue

                pointAward =
                    { country = country
                    , points = pointsToBeGiven
                    }

                newSetOfCountriesThatHaveReceivedPoints =
                    case pointsToBeGiven of
                        12 ->
                            Set.empty

                        _ ->
                            Set.insert country.name model.namesOfCountriesThatHaveGottenPointsInThisRound
            in
                ( { model
                    | participants =
                        model.participants
                            |> List.map (givePointsToParticipantIfCountryHasSameName pointAward)
                    , pointQueue = rotatedPointQueue
                    , namesOfCountriesThatHaveGottenPointsInThisRound = newSetOfCountriesThatHaveReceivedPoints
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        shouldBeDisabled : Participant -> Bool
        shouldBeDisabled participant =
            Set.member participant.country.name <| model.namesOfCountriesThatHaveGottenPointsInThisRound

        displayParticipant : Participant -> Html Msg
        displayParticipant participant =
            div []
                [ button
                    [ onClick <| GivePoints participant.country
                    , style
                        [ ( "display", "flex" )
                        , ( "justify-content", "flex-start" )
                        , ( "width", "100%" )
                        , ( "font-size", "1.8rem" )
                        , ( "padding", "10px" )
                        ]
                    , disabled <| shouldBeDisabled participant
                    ]
                    [ div
                        [ style
                            [ ( "width", "50px" )
                            , ( "height", "50px" )
                            , ( "background-image", "url('assets/flags-normal/" ++ participant.country.abbreviation ++ ".png')" )
                            , ( "border-radius", "50px" )
                            , ( "background-position", "center center" )
                            , ( "background-size", "cover" )
                            ]
                        ]
                        []
                    , div
                        [ style [ ( "width", "100%" ) ] ]
                        [ text participant.country.name ]
                    , div [] [ text <| toString participant.points ]
                    ]
                ]

        pointsToBeAwarded =
            case (model.pointQueue |> List.head) of
                Just points ->
                    points

                _ ->
                    0
    in
        div []
            [ h1 [] [ text "Eurovision Scoreboard" ]
            , h2 [] [ text <| toString pointsToBeAwarded ++ " points goes to" ]
            , div
                []
                (model.participants
                    |> List.sortBy .points
                    |> List.reverse
                    |> List.map
                        displayParticipant
                )
            ]
