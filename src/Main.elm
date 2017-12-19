module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


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
    }


type alias Participant =
    { country : Country
    , points : Int
    }


type alias Model =
    { participants : List Participant
    , pointQueue : List Int
    }


initialModel : Model
initialModel =
    let
        initialPoints =
            0

        initialPointQueue =
            [ 1, 2, 3, 4, 5, 6, 7, 8, 10, 12 ]
    in
        { participants =
            [ { country = { name = "Norway" }, points = initialPoints }
            , { country = { name = "Sweden" }, points = initialPoints }
            , { country = { name = "Denmark" }, points = initialPoints }
            ]
        , pointQueue = initialPointQueue
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
    let
        givePointsToParticipantIfCountryHasSameName : PointAward -> Participant -> Participant
        givePointsToParticipantIfCountryHasSameName pointAward participant =
            if pointAward.country == participant.country then
                { participant | points = participant.points + pointAward.points }
            else
                participant
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            GivePoints country ->
                let
                    pointsToBeGiven =
                        case List.head model.pointQueue of
                            Just points ->
                                points

                            _ ->
                                0

                    rotatedPointQueue =
                        List.drop 1 model.pointQueue ++ List.take 1 model.pointQueue

                    pa =
                        { country = country
                        , points = pointsToBeGiven
                        }
                in
                    ( { model
                        | participants = List.map (givePointsToParticipantIfCountryHasSameName pa) model.participants
                        , pointQueue = rotatedPointQueue
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
        displayCountry : Participant -> Html Msg
        displayCountry c =
            div []
                [ button [ onClick (GivePoints { name = c.country.name }) ] [ text (c.country.name ++ " " ++ toString c.points) ]
                ]

        pointsToBeAwarded =
            case (model.pointQueue |> List.head) of
                Just points ->
                    points

                _ ->
                    0
    in
        div []
            [ h1 [] [ text "Eurovision ScoreBoard" ]
            , h2 [] [ text (toString pointsToBeAwarded ++ " points goes to") ]
            , div
                []
                (model.participants
                    |> List.sortBy .points
                    |> List.reverse
                    |> List.map
                        displayCountry
                )
            ]
