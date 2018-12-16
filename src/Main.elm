port module Main exposing
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



---- ports ----


type alias StorageModel =
    { maxSelectNum : Int
    , selectedNum : Int
    , memberList : List String
    , name : String
    }


port setStorage : StorageModel -> Cmd msg



---- MODEL ----


type alias Model =
    { maxSelectNum : Int
    , selectedNum : Int
    , shuffleListType : ShuffleListType
    , memberList : List String
    , name : String
    , shuffleMode : ShuffleMode
    }


init : Maybe StorageModel -> ( Model, Cmd Msg )
init maybeStorageModel =
    let
        { maxSelectNum, selectedNum, memberList, name } =
            Maybe.withDefault (StorageModel 2 2 [] "") maybeStorageModel
    in
    ( { maxSelectNum = maxSelectNum
      , selectedNum = selectedNum
      , shuffleListType = People
      , memberList = memberList
      , name = name
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
    | DeleteMember Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ maxSelectNum, selectedNum, memberList, name, shuffleMode } as model) =
    case msg of
        ChangeNum numText ->
            let
                snum =
                    Maybe.withDefault 2 <| String.toInt numText
            in
            ( { model | selectedNum = snum }
            , Cmd.batch [ setStorage <| StorageModel maxSelectNum snum memberList name ]
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
                    , maxSelectNum = maxPeopleNum memberList
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
            ( { model | name = n }
            , Cmd.batch [ setStorage <| StorageModel maxSelectNum selectedNum memberList n ]
            )

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
                , Cmd.batch [ setStorage <| StorageModel maxSelectNum selectedNum newMemberList "" ]
                )

            else
                ( model, Cmd.none )

        DeleteMember idx ->
            let
                list =
                    List.reverse memberList

                deletedMemberList =
                    (List.take idx list ++ List.drop (idx + 1) list)
                        |> List.reverse
            in
            ( { model | memberList = deletedMemberList }
            , Cmd.batch [ setStorage <| StorageModel maxSelectNum selectedNum deletedMemberList name ]
            )

        ClearMember ->
            ( { model
                | memberList = []
                , maxSelectNum = 2
                , selectedNum = 2
              }
            , Cmd.batch [ setStorage <| StorageModel 2 2 [] name ]
            )

        LetsShuffle _ ->
            ( model, Cmd.batch [ Random.generate ListShuffle <| shuffle memberList ] )

        ListShuffle shuffledMemberList ->
            ( { model | memberList = shuffledMemberList }
            , Cmd.batch [ setStorage <| StorageModel maxSelectNum selectedNum shuffledMemberList name ]
            )


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
                |> membersListView shuffleListType selectedNum
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


membersListView : ShuffleListType -> Int -> List (List String) -> Html Msg
membersListView shuffleListType selectedNum membersList =
    let
        perNum =
            groupPerNum shuffleListType selectedNum (List.concat membersList)
    in
    ul [ class "membersList" ] <|
        List.indexedMap
            (\lindex members ->
                li []
                    [ ul [] <|
                        List.indexedMap
                            (\mindex member ->
                                li []
                                    [ text member
                                    , button
                                        [ onClick <| DeleteMember (lindex * perNum + mindex)
                                        , class "destroy"
                                        ]
                                        []
                                    ]
                            )
                            members
                    ]
            )
            membersList


groupPerNum : ShuffleListType -> Int -> List String -> Int
groupPerNum shuffleListType num members =
    let
        numOfMember =
            List.length members
    in
    case shuffleListType of
        People ->
            num

        Team ->
            numOfMember // num


groupedMembersList : ShuffleListType -> Int -> List String -> List (List String)
groupedMembersList shuffleListType num members =
    case shuffleListType of
        People ->
            let
                groupedMembers =
                    List.groupsOf num members

                ungroupedMembersLength =
                    List.length <| List.concat groupedMembers

                ungroupedMember =
                    List.drop ungroupedMembersLength members
            in
            if List.isEmpty ungroupedMember then
                groupedMembers

            else
                groupedMembers ++ [ ungroupedMember ]

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
