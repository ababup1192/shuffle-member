module Tests exposing (suite)

import Expect exposing (Expectation)
import Html exposing (option, select, text)
import Html.Attributes exposing (value)
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
            ]
        ]
