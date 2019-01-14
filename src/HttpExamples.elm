module Main exposing (Girl, Model(..), Msg(..), getRandomNames, init, main, nameDecoder, subscriptions, update, view, viewGirl)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, index, list, map2, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success Girl


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRandomNames )



-- UPDATE


type Msg
    = MorePlease
    | GotName (Result Http.Error Girl)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getRandomNames )

        GotName result ->
            case result of
                Ok girl ->
                    ( Success girl, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Random Girl" ]
        , viewGirl model
        ]


viewGirl : Model -> Html Msg
viewGirl model =
    case model of
        Failure ->
            div []
                [ text "I could not load the names for some reason "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success girl ->
            div []
                [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
                , h2 [] [ text girl.firstName ]
                , h2 [] [ text girl.lastName ]
                ]



-- HTTP


getRandomNames : Cmd Msg
getRandomNames =
    Http.get
        { url = "https:/localhost:8013/names.json"
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
