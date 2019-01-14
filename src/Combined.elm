module Main exposing (Girl, Model, Msg(..), State(..), extractColor, generatedInt, generatedPair, getRandomNames, init, main, nameDecoder, possibleEyeColors, possibleHairColors, returnMP, subscriptions, update, view, viewGirl)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, index, list, map2, string)
import Random
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Card.Block as Block
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Button as Button
-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" "" "" 0 Loading, Random.generate SetFeatures generatedPair )



-- MODEL


type alias Model =
    { firstName : String
    , lastName : String
    , hairColor : String
    , eyeColor : String
    , age : Int
    , status : State
    }


type State
    = Failure
    | Loading
    | Success



-- UPDATE
-- color picker helper


possibleHairColors =
    [ "blue", "pink", "red", "green" ]


possibleEyeColors =
    [ "black", "brown", "hazel", "green", "blue" ]


extractColor pos list =
    Maybe.withDefault "black" (List.head (List.drop (pos - 1) (List.take pos list)))



-- -> "black"
-- random generated helper


generatedPair : Random.Generator ( Int, Int )
generatedPair =
    Random.pair (Random.int 1 4) (Random.int 1 5)


generatedInt : Random.Generator Int
generatedInt =
    Random.int 18 41



-- getRandomNames Http get command


getRandomNames : Cmd Msg
getRandomNames =
    Http.get
        { url = "https://randomuser.me/api/?gender=female"

        -- { url = "http://localhost:8001/data/names.json"
        , expect = Http.expectJson GotName nameDecoder
        }


type alias Girl =
    { firstName : String
    , lastName : String
    }


nameDecoder : Decoder Girl
nameDecoder =
    map2 Girl
        (field "results" (index 0 (field "name" (field "first" string))))
        (field "results" (index 0 (field "name" (field "last" string))))



-- helper function to take in a Girl and Return a ManicPixieDG.


returnMP : Girl -> Model -> Model
returnMP gorl model =
    Model gorl.firstName gorl.lastName model.hairColor model.eyeColor model.age Success



-- updateupdate


type Msg
    = MorePlease
    | RollFeatures
    | RollAge
    | SetFeatures ( Int, Int )
    | SetAge Int
    | GotName (Result Http.Error Girl)
    | ChainMsgs (List Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChainMsgs msgs ->
            let
                chain msg1 ( model1, cmds ) =
                    let
                        ( model2, cmds1 ) =
                            update msg1 model1
                    in
                    ( model, Cmd.batch [ cmds, cmds1 ] )
            in
            List.foldl chain ( model, Cmd.batch [] ) msgs

        MorePlease ->
            ( { model | status = Loading }, getRandomNames )

        GotName result ->
            case result of
                Ok girl ->
                    ( returnMP girl model, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        RollFeatures ->
            ( model
            , Random.generate SetFeatures generatedPair
            )

        RollAge ->
            ( model
            , Random.generate SetAge generatedInt
            )

        SetFeatures twoNums ->
            ( Model model.firstName model.lastName (extractColor (Tuple.first twoNums) possibleHairColors) (extractColor (Tuple.second twoNums) possibleEyeColors) (Tuple.second twoNums) Success
            , getRandomNames
            )

        SetAge age ->
            ( Model model.firstName model.lastName model.hairColor model.eyeColor age Success
            , getRandomNames
            )



-- VIEW


view : Model -> Html Msg
view model =    -- Responsive fixed width container
        Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ viewGirl model ]
            ]

        ]


viewGirl : Model -> Html Msg
viewGirl model =
    case model.status of
        Failure ->
            div []
                [ text "I could not load the names for some reason "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success ->
            div []
                [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Manic Pixie Dream Girl Generator" ]
                |> Card.listGroup
                    [ ListGroup.li [ ListGroup.success ] [ text ("hair color: " ++ model.hairColor)]
                        , ListGroup.li [ ListGroup.info ] [ text ("eye color: " ++ model.eyeColor)  ]
                        , ListGroup.li [ ListGroup.info ] [ text ("first name: " ++ model.firstName)  ]
                        , ListGroup.li [ ListGroup.info ] [ text ("last name: " ++ model.lastName)  ]
                        , ListGroup.li [ ListGroup.warning ] [ text ("age: " ++ String.fromInt model.age) ]
                    ]
                |> Card.block []
                    [ Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ onClick (ChainMsgs [ RollAge, RollFeatures, MorePlease ]) ]]
                            [ text "Roll" ]
                    ]
                |> Card.view
                ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
