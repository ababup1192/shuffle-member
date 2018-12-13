module Main exposing
    ( Model
    , Msg(..)
    , ShuffleListType(..)
    , groupedMembersList
    , membersListView
    , selectNumView
    , selectShuffleListTypeView
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, selected, value)
import Html.Events as Event exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)



---- MODEL ----


type alias Model =
    { maxSelectNum : Int
    , selectedNum : Int
    , shuffleListType : ShuffleListType
    , memberList : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { maxSelectNum = 2
      , selectedNum = 2
      , shuffleListType = People
      , memberList = []
      }
    , Cmd.none
    )


type ShuffleListType
    = People
    | Team



---- UPDATE ----


type Msg
    = ChangeNum String
    | ChangeShuffleType String
    | ToggleShuffle
    | UpdateNewMember String
    | AddMember
    | ClearMember


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { maxSelectNum, shuffleListType, selectedNum, memberList } =
    section []
        [ article []
            [ selectNumView maxSelectNum selectedNum
            , selectShuffleListTypeView shuffleListType
            , button [ onClick ToggleShuffle ] [ text "シャッフル♪" ]
            ]
        , article []
            [ input [ placeholder "新規メンバー", onInput UpdateNewMember ] []
            , button [ onClick AddMember ] [ text "追加" ]
            , button [ onClick ClearMember ] [ text "クリア" ]
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


selectNumView : Int -> Int -> Html Msg
selectNumView maxSelectNum selectedNum =
    select [ onChange ChangeNum ]
        (List.range 2 maxSelectNum
            |> List.map
                (\n ->
                    let
                        nText =
                            String.fromInt n
                    in
                    option [ value nText, selected <| selectedNum == n ] [ text nText ]
                )
        )


selectShuffleListTypeView : ShuffleListType -> Html Msg
selectShuffleListTypeView shuffleListType =
    select [ onChange ChangeShuffleType ]
        [ option [ value "people", selected <| shuffleListType == People ] [ text "人" ]
        , option [ value "team", selected <| shuffleListType == Team ] [ text "チーム" ]
        ]


membersListView : List (List String) -> Html Msg
membersListView membersList =
    ul [ class "membersList" ] <|
        List.map
            (\members ->
                li []
                    [ ul [] <|
                        List.map
                            (\member ->
                                li []
                                    [ text member
                                    , button [ class "destroy" ] []
                                    ]
                            )
                            members
                    ]
            )
            membersList


groupedMembersList : ShuffleListType -> Int -> List String -> List (List String)
groupedMembersList shuffleListType num members =
    []


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



--- List Util ---


groupsOf : Int -> List a -> List (List a)
groupsOf size xs =
    groupsOfWithStep size size xs


groupsOfWithStep : Int -> Int -> List a -> List (List a)
groupsOfWithStep size step xs =
    let
        thisGroup =
            List.take size xs

        xs_ =
            List.drop step xs

        okayArgs =
            size > 0 && step > 0

        okayLength =
            size == List.length thisGroup
    in
    if okayArgs && okayLength then
        thisGroup :: groupsOfWithStep size step xs_

    else
        []
