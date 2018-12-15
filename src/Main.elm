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
import List.Extra as List
import Random as Random exposing (Generator)
import Random.List exposing (shuffle)
import Time



---- MODEL ----


type alias Model =
    { maxSelectNum : Int
    , selectedNum : Int
    , shuffleListType : ShuffleListType
    , memberList : List String
    , name : String
    , shuffleMode : ShuffleMode
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { maxSelectNum = 2
      , selectedNum = 2
      , shuffleListType = People
      , memberList = []
      , name = ""
      , shuffleMode = Stop
      }
    , Cmd.none
    )


type ShuffleListType
    = People
    | Team


type ShuffleMode
    = Shuffle
    | Stop



---- UPDATE ----


type Msg
    = ChangeNum String
    | ChangeShuffleType String
    | ToggleShuffle
    | UpdateNewMember String
    | AddMember Int
    | ClearMember
    | LetsShuffle Time.Posix
    | ListShuffle (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ memberList, name, shuffleMode } as model) =
    case msg of
        ChangeNum numText ->
            ( { model
                | selectedNum =
                    Maybe.withDefault 2 <| String.toInt numText
              }
            , Cmd.none
            )

        ChangeShuffleType typeText ->
            if typeText == "people" then
                ( { model
                    | shuffleListType = People
                    , selectedNum = 2
                    , maxSelectNum = maxPeopleNum memberList
                  }
                , Cmd.none
                )

            else
                ( { model
                    | shuffleListType = Team
                    , selectedNum = 2
                  }
                , Cmd.none
                )

        ToggleShuffle ->
            let
                nextMode =
                    case shuffleMode of
                        Shuffle ->
                            Stop

                        Stop ->
                            Shuffle
            in
            ( { model | shuffleMode = nextMode }, Cmd.none )

        UpdateNewMember n ->
            ( { model | name = n }, Cmd.none )

        AddMember keyCode ->
            let
                trimedName =
                    String.trim name

                newMemberList =
                    trimedName :: memberList
            in
            if keyCode == enterKeyCode && not (String.isEmpty trimedName) then
                ( { model
                    | memberList = newMemberList
                    , name = ""
                    , maxSelectNum = maxPeopleNum newMemberList
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ClearMember ->
            ( { model
                | memberList = []
                , maxSelectNum = 2
                , selectedNum = 2
              }
            , Cmd.none
            )

        LetsShuffle _ ->
            ( model, Cmd.batch [ Random.generate ListShuffle <| shuffle memberList ] )

        ListShuffle shuffledMemberList ->
            ( { model | memberList = shuffledMemberList }, Cmd.none )


maxPeopleNum : List String -> Int
maxPeopleNum members =
    let
        peopleNum =
            List.length members // 2
    in
    if peopleNum < 2 then
        2

    else
        peopleNum


enterKeyCode : Int
enterKeyCode =
    13



---- VIEW ----


view : Model -> Html Msg
view { maxSelectNum, shuffleListType, selectedNum, memberList, name, shuffleMode } =
    section []
        [ article []
            [ selectNumView maxSelectNum selectedNum
            , selectShuffleListTypeView shuffleListType
            , case shuffleMode of
                Stop ->
                    button [ onClick ToggleShuffle, class "shuffle" ] [ text "シャッフル♪" ]

                Shuffle ->
                    button [ onClick ToggleShuffle, class "stop" ] [ text "ストップ！" ]
            ]
        , article []
            [ input
                [ placeholder "新規メンバー"
                , value name
                , onInput UpdateNewMember
                , onKeyDown AddMember
                ]
                []
            , button [ onClick ClearMember ] [ text "クリア" ]
            ]
        , article []
            [ memberList
                |> List.reverse
                |> groupedMembersList shuffleListType selectedNum
                |> membersListView
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
    case shuffleListType of
        People ->
            let
                groupedMembers =
                    List.groupsOf num members
            in
            groupedMembers
                ++ [ List.drop (List.length <| List.concat groupedMembers) members ]
                |> List.filter (\l -> not <| List.isEmpty l)

        Team ->
            let
                numOfMember =
                    List.length members

                teamNumsBase =
                    List.repeat num (numOfMember // num)

                remainder =
                    modBy num numOfMember

                teamNums =
                    let
                        adder =
                            List.map2 (+) teamNumsBase (List.repeat remainder 1)

                        tail =
                            List.drop remainder teamNumsBase
                    in
                    adder ++ tail
            in
            List.groupsOfVarying teamNums members


onChange : (String -> Msg) -> Attribute Msg
onChange handler =
    Event.on "change" (Decode.map handler Event.targetValue)


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Event.on "keydown" (Decode.map tagger Event.keyCode)


subscriptions : Model -> Sub Msg
subscriptions { shuffleMode } =
    case shuffleMode of
        Shuffle ->
            Time.every 50 LetsShuffle

        Stop ->
            Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
