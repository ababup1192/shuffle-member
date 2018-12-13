module Main exposing (Model, Msg(..), selectNumView, selectShuffleListTypeView)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events as Event
import Json.Decode as Decode exposing (Decoder)



---- MODEL ----


type alias Model =
    { maxSelectNum : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { maxSelectNum = 2 }, Cmd.none )



---- UPDATE ----


type Msg
    = ChangeNum String
    | ChangeShuffleType String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { maxSelectNum } =
    section []
        [ article []
            [ selectNumView maxSelectNum
            , selectShuffleListTypeView
            , button [] [ text "シャッフル♪" ]
            ]
        , article []
            [ input [ placeholder "新規メンバー" ] []
            , button [] [ text "追加" ]
            , button [] [ text "クリア" ]
            ]
        , article []
            [ ul [] <|
                List.repeat 10 <|
                    li []
                        [ ul [] <|
                            List.repeat 10 <|
                                li []
                                    [ text "あああ"
                                    , button [ class "destroy" ] []
                                    ]
                        ]
            ]
        ]


selectNumView : Int -> Html Msg
selectNumView maxSelectNum =
    select [ onChange ChangeNum ]
        (List.range 2 maxSelectNum
            |> List.map
                (\n ->
                    let
                        nText =
                            String.fromInt n
                    in
                    option [ value nText ] [ text nText ]
                )
        )


selectShuffleListTypeView : Html Msg
selectShuffleListTypeView =
    select []
        [ option [ value "people" ] [ text "人" ]
        , option [ value "team" ] [ text "チーム" ]
        ]


onChange : (String -> Msg) -> Attribute Msg
onChange handler =
    Event.on "change" (Decode.map handler Event.targetValue)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
