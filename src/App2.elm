import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { hairColor : String,
    age : Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" 0
  , Cmd.none
  )

colors=["blue", "pink", "red", "green"]
-- yay did this two different ways!!
extractColor pos = 
    let colorMaybe = ( List.head ( List.drop (pos-1) ( List.take (pos) colors ) ))
    in
    case colorMaybe of
        Nothing -> "Hello darkness my old friend"
        Just val ->
            val
    -- Maybe.withDefault "No val" ( List.head ( List.drop (pos-1) ( List.take (pos) colors ) )) -- -> "black"


-- UPDATE


type Msg
  = Roll
  | NewGirl (Int, Int)


point : Random.Generator (Int, Int)
point =
      Random.pair (Random.int 1 4) (Random.int 18 41)
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewGirl point
      )

    NewGirl twoNums ->
      ( Model ( extractColor (Tuple.first twoNums) ) (Tuple.second twoNums)
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ( model.hairColor ) ]
    , h1 [] [ text ( String.fromInt model.age ) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]