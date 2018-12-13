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
                        |> Query.contains
                            [ select []
                                [ option [ value "2" ] [ text "2" ]
                                ]
                            ]
            , test "最大数が4のとき、選べる項目は 2, 3, 4 である" <|
                \_ ->
                    selectNumView 4
                        |> Query.fromHtml
                        |> Query.contains
                            [ select []
                                [ option [ value "2" ] [ text "2" ]
                                , option [ value "3" ] [ text "3" ]
                                , option [ value "4" ] [ text "4" ]
                                ]
                            ]
            ]
        ]
