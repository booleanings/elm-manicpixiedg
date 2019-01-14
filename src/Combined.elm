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
  (Model "" "" "" "" 0 Loading, Random.generate NewGirl generatedPair)


-- MODEL

type alias Model =
  { firstName : String
  , lastName : String
  , hairColor : String
  , eyeColor : String
  , age : Int
  , status: State
  }

type alias ManicPixieDG =
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
      Random.pair (Random.int 1 4) (Random.int 18 41)

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
returnMP : Girl -> Model -> ManicPixieDG
returnMP gorl model = ManicPixieDG gorl.firstName gorl.lastName model.hairColor model.eyeColor model.age Success
-- updateupdate



type Msg
  = MorePlease
  | Roll
  | NewGirl (Int, Int)
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

    Roll ->
      ( model
      , Random.generate NewGirl generatedPair
      )

    NewGirl twoNums ->
      (  Model model.firstName model.lastName ( extractColor (Tuple.first twoNums) possibleHairColors ) "" (Tuple.second twoNums)  Success
      , getRandomNames
      )

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Random Cats" ]
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
        [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , h1 [] [ text ( "hairColor: " ++ model.hairColor ) ]
        , h1 [] [ text ( "age: " ++ String.fromInt model.age ) ]
        , h2 [] [ text model.firstName ]
        , h2 [] [ text model.lastName ]
        , button [  onClick (ChainMsgs [Roll, MorePlease]) ] [ text "Roll" ]
        ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
