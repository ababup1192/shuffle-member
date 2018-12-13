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
                    selectNumView 2
                        |> Query.fromHtml
                        |> Query.find [ Selector.tag "option" ]
                        |> Query.has
                            [ Selector.attribute <| value "2"
                            , Selector.text "2"
                            ]
            , describe "最大数が4のとき" <|
                let
                    optionList =
                        selectNumView 4
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
                    selectNumView 3
                        |> Query.fromHtml
                        |> Event.simulate (Event.custom "change" <| simulatedStringEventObject "3")
                        |> Event.expect (ChangeNum "3")
            ]
        , describe "selectShuffleListTypeView"
            [ test "peopleを選択したら ChangeShuffleType 'people' の Msgが発行される " <|
                \_ ->
                    selectShuffleListTypeView
                        |> Query.fromHtml
                        |> Event.simulate (Event.custom "change" <| simulatedStringEventObject "people")
                        |> Event.expect (ChangeShuffleType "people")
            , test "teamを選択したら ChangeShuffleType 'team' の Msgが発行される " <|
                \_ ->
                    selectShuffleListTypeView
                        |> Query.fromHtml
                        |> Event.simulate (Event.custom "change" <| simulatedStringEventObject "team")
                        |> Event.expect (ChangeShuffleType "team")
            ]
        ]


simulatedStringEventObject : String -> Value
simulatedStringEventObject str =
    Encode.object
        [ ( "target"
          , Encode.object [ ( "value", Encode.string str ) ]
          )
        ]
