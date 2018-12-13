module Tests exposing (suite)

import Expect exposing (Expectation)
import Html exposing (option, select, text)
import Html.Attributes exposing (value)
import Json.Encode as Encode exposing (Value)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "The Main module"
        [ describe "selectNumView"
            [ test "最大数が2のとき、選べる項目は 2 である" <|
                \_ ->
                    selectNumView 2 2
                        |> Query.fromHtml
                        |> Query.find [ Selector.tag "option" ]
                        |> Query.has
                            [ Selector.attribute <| value "2"
                            , Selector.text "2"
                            ]
            , describe "最大数が4のとき" <|
                let
                    optionList =
                        selectNumView 4 2
                            |> Query.fromHtml
                            |> Query.findAll [ Selector.tag "option" ]

                    optionTest idx numText =
                        test
                            ("選べる項目の " ++ String.fromInt idx ++ "番目は " ++ numText ++ "である")
                        <|
                            \_ ->
                                optionList
                                    |> Query.index idx
                                    |> Query.has
                                        [ Selector.attribute <| value numText
                                        , Selector.text numText
                                        ]
                in
                [ optionTest 0 "2"
                , optionTest 1 "3"
                , optionTest 2 "4"
                ]
            , test "最大値が3のとき、3を選んだとき ChangeNum '3' の Msgが発行される" <|
                \_ ->
                    selectNumView 3 2
                        |> Query.fromHtml
                        |> Event.simulate (Event.custom "change" <| simulatedStringEventObject "3")
                        |> Event.expect (ChangeNum "3")
            ]
        , describe "selectShuffleListTypeView"
            [ test "peopleを選択したら ChangeShuffleType 'people' の Msgが発行される " <|
                \_ ->
                    selectShuffleListTypeView People
                        |> Query.fromHtml
                        |> Event.simulate (Event.custom "change" <| simulatedStringEventObject "people")
                        |> Event.expect (ChangeShuffleType "people")
            , test "teamを選択したら ChangeShuffleType 'team' の Msgが発行される " <|
                \_ ->
                    selectShuffleListTypeView People
                        |> Query.fromHtml
                        |> Event.simulate (Event.custom "change" <| simulatedStringEventObject "team")
                        |> Event.expect (ChangeShuffleType "team")
            ]
        , describe "membersListView"
            [ describe "6人のメンバーを2人ずつに分けたとき" <|
                let
                    memsListView =
                        membersListView [ [ "a", "b" ], [ "c", "d" ], [ "e", "f" ] ]
                            |> Query.fromHtml
                            |> Query.children [ Selector.tag "li" ]

                    membersListViewTest numOfList numOfMember name =
                        test (String.fromInt numOfList ++ "番目のリストの" ++ String.fromInt numOfMember ++ "番目は" ++ name ++ "である") <|
                            \_ ->
                                memsListView
                                    |> Query.index numOfList
                                    |> Query.children [ Selector.tag "li" ]
                                    |> Query.index numOfMember
                                    |> Query.has [ Selector.text name ]
                in
                [ membersListViewTest 0 0 "a"
                , membersListViewTest 0 1 "b"
                , membersListViewTest 1 0 "c"
                , membersListViewTest 1 1 "d"
                , membersListViewTest 2 0 "e"
                , membersListViewTest 2 1 "f"
                ]
            ]
        , describe "groupedMembersList"
            [ test "6人のメンバーを2人ずつで分けた" <|
                \_ ->
                    let
                        expected =
                            [ [ "a", "b" ], [ "c", "d" ], [ "e", "f" ] ]

                        actual =
                            [ "a", "b", "c", "d", "e", "f" ]
                                |> groupedMembersList People 2
                    in
                    Expect.equal expected actual
            , test "7人のメンバーを2人ずつで分けた" <|
                \_ ->
                    let
                        expected =
                            [ [ "a", "b" ], [ "c", "d" ], [ "e", "f" ], [ "g" ] ]

                        actual =
                            [ "a", "b", "c", "d", "e", "f", "g" ]
                                |> groupedMembersList People 2
                    in
                    Expect.equal expected actual
            , test "6人のメンバーを2チームで分けた" <|
                \_ ->
                    let
                        expected =
                            [ [ "a", "b", "c" ], [ "d", "e", "f" ] ]

                        actual =
                            [ "a", "b", "c", "d", "e", "f" ]
                                |> groupedMembersList Team 2
                    in
                    Expect.equal expected actual
            , test "7人のメンバーを2チームで分けた" <|
                \_ ->
                    let
                        expected =
                            [ [ "a", "b", "c" ], [ "d", "e", "f", "g" ] ]

                        actual =
                            [ "a", "b", "c", "d", "e", "f", "g" ]
                                |> groupedMembersList Team 2
                    in
                    Expect.equal expected actual
            , test "11人のメンバーを3チームで分けた" <|
                \_ ->
                    let
                        expected =
                            [ [ "a", "b", "c" ], [ "d", "e", "f" ], [ "g", "h", "i", "j", "k" ] ]

                        actual =
                            [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k" ]
                                |> groupedMembersList Team 3
                    in
                    Expect.equal expected actual
            ]
        ]


simulatedStringEventObject : String -> Value
simulatedStringEventObject str =
    Encode.object
        [ ( "target"
          , Encode.object [ ( "value", Encode.string str ) ]
          )
        ]
