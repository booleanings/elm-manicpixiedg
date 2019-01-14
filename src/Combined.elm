import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string, index, list, map2)
import Random



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init : () -> (Model, Cmd Msg)
init _ =
  (Model "" "" "" "" 0 Loading, Random.generate SetFeatures generatedPair)


-- MODEL

type alias Model =
  { firstName : String
  , lastName : String
  , hairColor : String
  , eyeColor : String
  , age : Int
  , status: State
  }

type State
  = Failure
  | Loading
  | Success


-- UPDATE

-- color picker helper
possibleHairColors = ["blue", "pink", "red", "green"]
possibleEyeColors = ["black", "brown", "hazel", "green", "blue"]
extractColor pos list =
    Maybe.withDefault "black" ( List.head ( List.drop (pos-1) ( List.take (pos) list ) )) -- -> "black"

-- random generated helper
generatedPair : Random.Generator (Int, Int)
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
    , expect = Http.expectJson GotName ( nameDecoder)
    }

type alias Girl =
  { firstName : String
  , lastName : String
  }

nameDecoder : Decoder Girl
nameDecoder =
  map2 Girl
      ( field "results" ( index 0 (field "name" (field "first" string))  ) )
      ( field "results" ( index 0 (field "name" (field "last" string))  ) )

-- helper function to take in a Girl and Return a ManicPixieDG.
returnMP : Girl -> Model -> Model
returnMP gorl model = Model gorl.firstName gorl.lastName model.hairColor model.eyeColor model.age Success
-- updateupdate



type Msg
  = MorePlease
  | RollFeatures
  | RollAge
  | SetFeatures (Int, Int)
  | SetAge (Int)
  | GotName (Result Http.Error Girl)
  | ChainMsgs (List Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChainMsgs msgs ->
      let
        chain msg1 (model1, cmds) =
          let (model2, cmds1) = update msg1 model1
          in (model, Cmd.batch [cmds, cmds1])
      in
        List.foldl chain (model, Cmd.batch []) msgs
    MorePlease ->
      ( {model | status = Loading}, getRandomNames)

    GotName result ->
      case result of
        Ok girl ->
          (returnMP girl model, Cmd.none)

        Err _ ->
          ( {model | status = Failure}, Cmd.none)

    RollFeatures ->
      ( model
      , Random.generate SetFeatures generatedPair
      )

    RollAge ->
      ( model
      , Random.generate SetAge generatedInt
      )

    SetFeatures twoNums ->
      (  Model model.firstName model.lastName ( extractColor (Tuple.first twoNums) possibleHairColors ) ( extractColor (Tuple.second twoNums) possibleEyeColors ) (Tuple.second twoNums)  Success
      , getRandomNames
      )

    SetAge age ->
      (  Model model.firstName model.lastName model.hairColor model.eyeColor age Success
      , getRandomNames
      )

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "ManicPixieDreamGirl Generator" ]
    , viewGirl model
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
        [ h1 [] [ text ( "hairColor: " ++ model.hairColor ) ]
        , h1 [] [ text ( "age: " ++ String.fromInt model.age ) ]
        , h1 [] [ text ( "eye color: " ++ model.eyeColor ) ]
        , h2 [] [ text ( "first name: " ++ model.firstName ) ]
        , h2 [] [ text ( "last name: " ++ model.lastName ) ]
        , button [  onClick (ChainMsgs [RollAge, RollFeatures, MorePlease]) ] [ text "RollFeatures" ]
        ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
