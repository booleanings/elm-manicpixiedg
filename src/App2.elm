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
  { hairColor : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model ""
  , Cmd.none
  )

colors=["blue", "pink", "red", "green"]


-- UPDATE


type Msg
  = Roll
  | NewColor Int

extractColor pos = 
    Maybe.withDefault "No val" ( List.head ( List.drop (pos-1) ( List.take (pos) colors ) )) -- -> "black"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewColor (Random.int 1 4)
      )

    NewColor randomNum ->
      ( Model ( extractColor randomNum )
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
    , button [ onClick Roll ] [ text "Roll" ]
    ]