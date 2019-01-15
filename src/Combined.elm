module Combined exposing (Model, State(..), init, main, returnMP, subscriptions, update, view, viewGirl)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, index, list, map2, string)
import Random
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card.Block as Block
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Button as Button
import RandomUtils exposing (..)
import GirlDrawing as GirlDrawing
import Rest exposing (Girl, Msg(..), getRandomNames, getRandomQuote, nameDecoder)
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" "" "" 0 "" "" "" Loading, Random.generate SetFeatures generatedPair )



-- MODEL


type alias Model =
    { firstName : String
    , lastName : String
    , hairColor : String
    , eyeColor : String
    , age : Int
    , firstWords : String
    , city : String
    , state : String
    , status : State
    }


type State
    = Failure
    | Loading
    | Success




returnMP : Rest.Girl -> Model -> Model
returnMP gorl model =
    Model gorl.firstName gorl.lastName model.hairColor model.eyeColor model.age model.firstWords gorl.city gorl.state Success




-- updateupdate
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
                    ( returnMP girl model, getRandomQuote )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        GotQuote result ->
            case result of
                Ok quote ->
                    ( {model | firstWords = quote}, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        RollFeatures ->
            ( model
            , Random.generate SetAge generatedInt
            )

        RollAge ->
            ( model
            , Random.generate SetFeatures generatedPair 
            )

        SetFeatures twoNums ->
            ( Model model.firstName model.lastName (extractColor (Tuple.first twoNums) possibleHairColors) (extractColor (Tuple.second twoNums) possibleEyeColors) model.age "" model.city model.state Success
            , getRandomNames
            )

        SetAge age ->
            ( Model model.firstName model.lastName model.hairColor model.eyeColor age "" model.city model.state Success
            , getRandomNames
            )



-- VIEW


view : Model -> Html Msg
view model =    -- Responsive fixed width container
        Grid.container [style "padding" "3%"]
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
                |> Card.block []
            [Block.custom <|
            Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        [  ]
                        [ h1 [style "font-family" "monospace"] [text (model.firstName ++ " " ++ model.lastName)]
                        , br [] []
                        , text ("She is ")
                        , strong [] [text (String.fromInt model.age)]
                        , text (" years old.")
                        , br [] []
                        , text ("location: " ++ model.city ++ ", " ++ model.state)]
                    , Grid.col
                        [  ]
                        [ div [style "padding-top" "1%", style "border" "2px solid black", style "border-radius" "10px" ] [GirlDrawing.drawing model.eyeColor model.hairColor]]
                    ]
                ]
            ]
                |> Card.listGroup
                    [ ListGroup.li [ ListGroup.warning ] [ h4 [style "font-family" "cursive"] [text ("first words: " ++ model.firstWords)] ]]
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
