module DictTests exposing (..)

import AVL.Dict as Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Internal exposing (AVLDict(..), Node(..))
import List.Extra
import Test exposing (Test, describe, fuzz, fuzz2, test)


draw : (key -> String) -> (value -> String) -> AVLDict key value -> String
draw keyToString valueToString (AVLDict _ _ root) =
    String.join "\n" (drawHelp keyToString valueToString root)


shiftRight : List String -> List String
shiftRight lines =
    case lines of
        [] ->
            []

        first :: rest ->
            "| | "
                :: ("| +-" ++ first)
                :: List.map ((++) "|   ") rest


shiftLeft : List String -> List String
shiftLeft lines =
    case lines of
        [] ->
            []

        first :: rest ->
            "|   "
                :: ("+---" ++ first)
                :: List.map ((++) "    ") rest


drawHelp : (key -> String) -> (value -> String) -> Node key value -> List String
drawHelp keyToString valueToString node =
    case node of
        RBEmpty_elm_builtin ->
            [ "Null" ]

        RBNode_elm_builtin height key value left right ->
            ("height: " ++ String.fromInt height ++ ", key: " ++ keyToString key ++ ", value: " ++ valueToString value)
                :: shiftRight (drawHelp keyToString valueToString right)
                ++ shiftLeft (drawHelp keyToString valueToString left)


validate : (comparable -> String) -> AVLDict comparable value -> Result String (AVLDict comparable value)
validate keyToString (AVLDict comparator size root) =
    Result.andThen
        (\( _, s ) ->
            if s == size then
                Ok (AVLDict comparator size root)

            else
                Err ("tracking size [" ++ String.fromInt size ++ "] does not match with real one [" ++ String.fromInt s ++ "]")
        )
        (validateHelp keyToString root)


validateHelp : (comparable -> String) -> Node comparable value -> Result String ( Int, Int )
validateHelp keyToString node =
    case node of
        RBEmpty_elm_builtin ->
            Ok ( 0, 0 )

        RBNode_elm_builtin _ key _ left right ->
            if Maybe.withDefault False (Maybe.map ((<) key) (extract left)) then
                Err ("key [" ++ keyToString key ++ "] is less than left")

            else if Maybe.withDefault False (Maybe.map ((>) key) (extract right)) then
                Err ("key [" ++ keyToString key ++ "] is more than right")

            else
                Result.andThen
                    (\( ( lh, ls ), ( rh, rs ) ) ->
                        if abs (lh - rh) > 1 then
                            Err ("height diff [" ++ keyToString key ++ "]: " ++ String.fromInt lh ++ " vs " ++ String.fromInt rh)

                        else
                            Ok ( 1 + max lh rh, 1 + ls + rs )
                    )
                    (Result.map2 Tuple.pair
                        (validateHelp keyToString left)
                        (validateHelp keyToString right)
                    )


extract : Node key value -> Maybe key
extract node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ key _ _ _ ->
            Just key



-- D E C O N S T R U C T I O N


keysSuite : Test
keysSuite =
    describe "AVL.Dict.keys"
        [ test "empty" <|
            \_ ->
                Dict.empty
                    |> Dict.keys
                    |> Expect.equalLists []

        --
        , test "singleton" <|
            \_ ->
                Dict.singleton 0 'A'
                    |> Dict.keys
                    |> Expect.equalLists [ 0 ]

        --
        , test "inserts" <|
            \_ ->
                Dict.empty
                    |> Dict.insert 2 'A'
                    |> Dict.insert 0 'B'
                    |> Dict.insert 5 'C'
                    |> Dict.insert 3 'E'
                    |> Dict.insert 1 'F'
                    |> Dict.insert 4 'G'
                    |> Dict.insert 3 'H'
                    |> Dict.keys
                    |> Expect.equalLists [ 0, 1, 2, 3, 4, 5 ]

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "fromList" <|
            \list ->
                Dict.fromList list
                    |> Dict.keys
                    |> Expect.equalLists
                        (list
                            |> List.map Tuple.first
                            |> List.sort
                            |> List.Extra.unique
                        )
        ]


valuesSuite : Test
valuesSuite =
    describe "AVL.Dict.values"
        [ test "empty" <|
            \_ ->
                Dict.empty
                    |> Dict.values
                    |> Expect.equalLists []

        --
        , test "singleton" <|
            \_ ->
                Dict.singleton 0 'A'
                    |> Dict.values
                    |> Expect.equalLists [ 'A' ]

        --
        , test "inserts" <|
            \_ ->
                Dict.empty
                    |> Dict.insert 2 'A'
                    |> Dict.insert 0 'B'
                    |> Dict.insert 5 'C'
                    |> Dict.insert 3 'E'
                    |> Dict.insert 1 'F'
                    |> Dict.insert 4 'G'
                    |> Dict.insert 3 'B'
                    |> Dict.values
                    |> Expect.equalLists [ 'B', 'F', 'A', 'B', 'G', 'C' ]

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "fromList" <|
            \list ->
                Dict.fromList list
                    |> Dict.values
                    |> Expect.equalLists
                        (list
                            -- the last added value goes to Dict
                            |> List.reverse
                            -- keeps the first value
                            |> List.Extra.uniqueBy Tuple.first
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.second
                        )
        ]


toListSuite : Test
toListSuite =
    describe "AVL.Dict.toList"
        [ test "empty" <|
            \_ ->
                Dict.empty
                    |> Dict.toList
                    |> Expect.equalLists []

        --
        , test "singleton" <|
            \_ ->
                Dict.singleton 0 'A'
                    |> Dict.toList
                    |> Expect.equalLists [ ( 0, 'A' ) ]

        --
        , test "inserts" <|
            \_ ->
                Dict.empty
                    |> Dict.insert 2 'A'
                    |> Dict.insert 0 'B'
                    |> Dict.insert 5 'C'
                    |> Dict.insert 3 'E'
                    |> Dict.insert 1 'F'
                    |> Dict.insert 4 'G'
                    |> Dict.insert 3 'B'
                    |> Dict.toList
                    |> Expect.equalLists
                        [ ( 0, 'B' )
                        , ( 1, 'F' )
                        , ( 2, 'A' )
                        , ( 3, 'B' )
                        , ( 4, 'G' )
                        , ( 5, 'C' )
                        ]

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "fromList" <|
            \list ->
                Dict.fromList list
                    |> Dict.toList
                    |> Expect.equalLists
                        (list
                            -- the last added value goes to Dict
                            |> List.reverse
                            -- keeps the first value
                            |> List.Extra.uniqueBy Tuple.first
                            |> List.sortBy Tuple.first
                        )
        ]



-- M A N I P U L A T I O N


insertSuite : Test
insertSuite =
    describe "AVL.Dict.insert"
        [ fuzz2 Fuzz.int Fuzz.string "AVL.Dict.empty" <|
            \key value ->
                Dict.empty
                    |> Dict.insert key value
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to left" <|
            \_ ->
                Dict.singleton 10 'a'
                    |> Dict.insert 5 'b'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to left left" <|
            \_ ->
                Dict.singleton 10 'a'
                    |> Dict.insert 5 'b'
                    |> Dict.insert 2 'c'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to left right" <|
            \_ ->
                Dict.singleton 10 'a'
                    |> Dict.insert 5 'b'
                    |> Dict.insert 8 'c'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to right" <|
            \_ ->
                Dict.singleton 10 'a'
                    |> Dict.insert 15 'b'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to right left" <|
            \_ ->
                Dict.singleton 10 'a'
                    |> Dict.insert 15 'b'
                    |> Dict.insert 12 'c'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to right right" <|
            \_ ->
                Dict.singleton 10 'a'
                    |> Dict.insert 15 'b'
                    |> Dict.insert 20 'c'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "M-N-O-L-K-Q-P-H-I-A" <|
            \_ ->
                "MNOLKQPHIA"
                    |> String.toList
                    |> List.foldl
                        (\char -> Result.andThen (validate String.fromChar << Dict.insert char (Char.toCode char)))
                        (Ok Dict.empty)
                    |> Expect.ok

        --
        , fuzz2 (Fuzz.intRange -400 -100) (Fuzz.intRange 100 400) "descending keys" <|
            \lo hi ->
                List.range lo hi
                    |> List.foldr
                        (\i -> Result.andThen (validate String.fromInt << Dict.insert i (String.fromInt i)))
                        (Ok Dict.empty)
                    |> Expect.ok

        --
        , fuzz2 (Fuzz.intRange -400 -100) (Fuzz.intRange 100 400) "ascending keys" <|
            \lo hi ->
                List.range lo hi
                    |> List.foldl
                        (\i -> Result.andThen (validate String.fromInt << Dict.insert i (String.fromInt i)))
                        (Ok Dict.empty)
                    |> Expect.ok

        --
        , fuzz (Fuzz.list (Fuzz.intRange -200 200)) "random keys" <|
            \list ->
                list
                    |> List.foldl
                        (\i -> Result.andThen (validate String.fromInt << Dict.insert i (String.fromInt i)))
                        (Ok Dict.empty)
                    |> Expect.ok
        ]


removeSuite : Test
removeSuite =
    describe "AVL.Dict.remove"
        [ fuzz Fuzz.int "AVL.Dict.empty" <|
            \key ->
                Dict.empty
                    |> Dict.remove key
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , fuzz2 Fuzz.int Fuzz.int "AVL.Dict.singleton" <|
            \put delete ->
                Dict.singleton put ()
                    |> Dict.remove delete
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , fuzz2 (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) (Fuzz.list Fuzz.int) "AVL.Dict.fromList" <|
            \puts deletes ->
                List.foldl
                    (\key -> Result.andThen (validate String.fromInt << Dict.remove key))
                    (Ok (Dict.fromList puts))
                    deletes
                    |> Expect.ok

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "clear" <|
            \list ->
                List.foldl
                    (\( key, _ ) -> Result.andThen (validate String.fromInt << Dict.remove key))
                    (Ok (Dict.fromList list))
                    list
                    |> Result.map Dict.isEmpty
                    |> Result.withDefault False
                    |> Expect.equal True
        ]


updateSuite : Test
updateSuite =
    describe "AVL.Dict.update"
        [ test "Nothing to Nothing" <|
            \_ ->
                Dict.fromList [ ( 0, "A" ), ( 1, "B" ) ]
                    |> Dict.update 2 (always Nothing)
                    |> Dict.toList
                    |> Expect.equalLists [ ( 0, "A" ), ( 1, "B" ) ]

        --
        , test "Nothing to Just" <|
            \_ ->
                Dict.fromList [ ( 0, "A" ), ( 1, "B" ) ]
                    |> Dict.update 2 (always (Just "C"))
                    |> Dict.toList
                    |> Expect.equalLists [ ( 0, "A" ), ( 1, "B" ), ( 2, "C" ) ]

        --
        , test "Just to Nothing" <|
            \_ ->
                Dict.fromList [ ( 0, "A" ), ( 1, "B" ) ]
                    |> Dict.update 1 (always Nothing)
                    |> Dict.toList
                    |> Expect.equalLists [ ( 0, "A" ) ]

        --
        , test "Just to Just" <|
            \_ ->
                Dict.fromList [ ( 0, "A" ), ( 1, "B" ) ]
                    |> Dict.update 1 (Maybe.map ((++) "C"))
                    |> Dict.toList
                    |> Expect.equalLists [ ( 0, "A" ), ( 1, "CB" ) ]
        ]


clearSuite : Test
clearSuite =
    describe "AVL.Dict.clear"
        [ test "AVL.Dict.empty" <|
            \_ ->
                Dict.empty
                    |> Dict.clear
                    |> Dict.toList
                    |> Expect.equalLists []

        --
        , fuzz2 Fuzz.int Fuzz.char "AVL.Dict.singleton" <|
            \key value ->
                Dict.singleton key value
                    |> Dict.clear
                    |> Dict.toList
                    |> Expect.equalLists []

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "AVL.Dict.fromList" <|
            \list ->
                Dict.fromList list
                    |> Dict.clear
                    |> Dict.toList
                    |> Expect.equalLists []
        ]



-- Q U E R Y


isEmptySuite : Test
isEmptySuite =
    describe "AVL.Dict.isEmpty"
        [ test "AVL.Dict.empty" <|
            \_ ->
                Dict.empty
                    |> Dict.isEmpty
                    |> Expect.equal True

        --
        , fuzz2 Fuzz.char Fuzz.string "AVL.Dict.singleton" <|
            \key value ->
                Dict.singleton key value
                    |> Dict.isEmpty
                    |> Expect.equal False

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.char Fuzz.int)) "AVL.Dict.fromList" <|
            \list ->
                Dict.fromList list
                    |> Dict.isEmpty
                    |> Expect.equal (List.isEmpty list)

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "AVL.Dict.clear" <|
            \list ->
                Dict.fromList list
                    |> Dict.clear
                    |> Dict.isEmpty
                    |> Expect.equal True
        ]


sizeSuite : Test
sizeSuite =
    describe "AVL.Dict.size"
        [ test "AVL.Dict.empty" <|
            \_ ->
                Dict.empty
                    |> Dict.size
                    |> Expect.equal 0

        --
        , fuzz2 Fuzz.char Fuzz.string "AVL.Dict.singleton" <|
            \key value ->
                Dict.singleton key value
                    |> Dict.size
                    |> Expect.equal 1

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.string Fuzz.int)) "AVL.Dict.fromList" <|
            \list ->
                let
                    uniq =
                        List.Extra.uniqueBy Tuple.first list
                in
                Dict.fromList list
                    |> Dict.size
                    |> Expect.equal (List.length uniq)

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "AVL.Dict.clear" <|
            \list ->
                Dict.fromList list
                    |> Dict.clear
                    |> Dict.size
                    |> Expect.equal 0
        ]


memberSuite : Test
memberSuite =
    describe "AVL.Dict.member"
        [ fuzz Fuzz.int "AVL.Dict.empty" <|
            \key ->
                Dict.empty
                    |> Dict.member key
                    |> Expect.equal False

        --
        , fuzz2 Fuzz.int Fuzz.int "AVL.Dict.singleton" <|
            \x y ->
                Dict.singleton x ()
                    |> Dict.member y
                    |> Expect.equal (x == y)

        --
        , fuzz2 Fuzz.int (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "AVL.Dict.fromList" <|
            \key list ->
                Dict.fromList list
                    |> Dict.member key
                    |> Expect.equal (List.member key (List.map Tuple.first list))
        ]


getSuite : Test
getSuite =
    describe "AVL.Dict.get"
        [ fuzz Fuzz.int "AVL.Dict.empty" <|
            \key ->
                Dict.empty
                    |> Dict.get key
                    |> Expect.equal Nothing

        --
        , fuzz2 Fuzz.int Fuzz.int "AVL.Dict.singleton" <|
            \x y ->
                Dict.singleton x ()
                    |> Dict.get y
                    |> Expect.equal
                        (if x == y then
                            Just ()

                         else
                            Nothing
                        )

        --
        , fuzz2 Fuzz.int (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "AVL.Dict.fromList" <|
            \key list ->
                Dict.fromList list
                    |> Dict.get key
                    |> Expect.equal
                        (List.reverse list
                            |> List.Extra.find ((==) key << Tuple.first)
                            |> Maybe.map Tuple.second
                        )
        ]


minimumSuite : Test
minimumSuite =
    describe "AVL.Dict.minimum"
        [ test "AVL.Dict.empty" <|
            \_ ->
                Dict.empty
                    |> Dict.minimum
                    |> Expect.equal Nothing

        --
        , fuzz2 Fuzz.int Fuzz.char "AVL.Dict.singleton" <|
            \key value ->
                Dict.singleton key value
                    |> Dict.minimum
                    |> Expect.equal (Just ( key, value ))

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "AVL.Dict.fromList" <|
            \list ->
                Dict.fromList list
                    |> Dict.minimum
                    |> Expect.equal
                        (List.foldl
                            (\( key, value ) acc ->
                                case acc of
                                    Nothing ->
                                        Just ( key, value )

                                    Just ( min, _ ) ->
                                        if key > min then
                                            acc

                                        else
                                            Just ( key, value )
                            )
                            Nothing
                            list
                        )

        --
        , test "Docs" <|
            \_ ->
                [ ( "Bob", { name = "Bob", age = 19, height = 1.82 } )
                , ( "Alice", { name = "Alice", age = 28, height = 1.65 } )
                , ( "Chuck", { name = "Chuck", age = 33, height = 1.75 } )
                ]
                    |> Dict.fromList
                    |> Dict.minimum
                    |> Expect.equal (Just ( "Alice", { name = "Alice", age = 28, height = 1.65 } ))
        ]


maximumSuite : Test
maximumSuite =
    describe "AVL.Dict.maximum"
        [ test "AVL.Dict.empty" <|
            \_ ->
                Dict.empty
                    |> Dict.maximum
                    |> Expect.equal Nothing

        --
        , fuzz2 Fuzz.int Fuzz.char "AVL.Dict.singleton" <|
            \key value ->
                Dict.singleton key value
                    |> Dict.maximum
                    |> Expect.equal (Just ( key, value ))

        --
        , fuzz (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.char)) "AVL.Dict.fromList" <|
            \list ->
                Dict.fromList list
                    |> Dict.maximum
                    |> Expect.equal
                        (List.foldl
                            (\( key, value ) acc ->
                                case acc of
                                    Nothing ->
                                        Just ( key, value )

                                    Just ( min, _ ) ->
                                        if key < min then
                                            acc

                                        else
                                            Just ( key, value )
                            )
                            Nothing
                            list
                        )

        --
        , test "Docs" <|
            \_ ->
                [ ( "Bob", { name = "Bob", age = 19, height = 1.82 } )
                , ( "Alice", { name = "Alice", age = 28, height = 1.65 } )
                , ( "Chuck", { name = "Chuck", age = 33, height = 1.75 } )
                ]
                    |> Dict.fromList
                    |> Dict.maximum
                    |> Expect.equal (Just ( "Chuck", { name = "Chuck", age = 33, height = 1.75 } ))
        ]



-- T R A N S F O R M


mapSuite : Test
mapSuite =
    test "AVL.Dict.map" <|
        \_ ->
            [ ( 3, "A" )
            , ( 1, "B" )
            , ( 4, "C" )
            , ( 5, "D" )
            , ( 2, "E" )
            ]
                |> Dict.fromList
                |> Dict.map (\key value -> String.toList (String.repeat key value))
                |> Dict.toList
                |> Expect.equalLists
                    [ ( 1, [ 'B' ] )
                    , ( 2, [ 'E', 'E' ] )
                    , ( 3, [ 'A', 'A', 'A' ] )
                    , ( 4, [ 'C', 'C', 'C', 'C' ] )
                    , ( 5, [ 'D', 'D', 'D', 'D', 'D' ] )
                    ]


foldlSuite : Test
foldlSuite =
    fuzz (Fuzz.list (Fuzz.map (\x -> ( x, String.fromInt x )) Fuzz.int)) "AVL.Dict.foldl" <|
        \list ->
            Dict.fromList list
                |> Dict.foldl (\k v acc -> ( k, v ) :: acc) []
                |> Expect.equalLists
                    (list
                        |> List.Extra.uniqueBy Tuple.first
                        |> List.sortBy Tuple.first
                        |> List.reverse
                    )


foldrSuite : Test
foldrSuite =
    fuzz (Fuzz.list (Fuzz.map (\x -> ( x, String.fromInt x )) Fuzz.int)) "AVL.Dict.foldr" <|
        \list ->
            Dict.fromList list
                |> Dict.foldr (\k v acc -> ( k, v ) :: acc) []
                |> Expect.equalLists
                    (list
                        |> List.Extra.uniqueBy Tuple.first
                        |> List.sortBy Tuple.first
                    )


filterSuite : Test
filterSuite =
    test "AVL.Dict.filter" <|
        \_ ->
            [ ( 3, 'A' )
            , ( 1, 'B' )
            , ( 4, 'C' )
            , ( 5, 'D' )
            , ( 2, 'E' )
            ]
                |> Dict.fromList
                |> Dict.filter (\key value -> key > 3 || value == 'B')
                |> Dict.toList
                |> Expect.equalLists
                    [ ( 1, 'B' )
                    , ( 4, 'C' )
                    , ( 5, 'D' )
                    ]


partitionSuite : Test
partitionSuite =
    test "AVL.Dict.partition" <|
        \_ ->
            [ ( 3, 'A' )
            , ( 1, 'B' )
            , ( 4, 'C' )
            , ( 5, 'D' )
            , ( 2, 'E' )
            ]
                |> Dict.fromList
                |> Dict.partition (\key value -> key > 3 || value == 'B')
                |> Tuple.mapBoth Dict.toList Dict.toList
                |> Expect.equal
                    ( [ ( 1, 'B' )
                      , ( 4, 'C' )
                      , ( 5, 'D' )
                      ]
                    , [ ( 2, 'E' )
                      , ( 3, 'A' )
                      ]
                    )



-- C O M B I N E


unionSuite : Test
unionSuite =
    describe "AVL.Dict.union"
        [ fuzz (dictFuzz Fuzz.int Fuzz.char) "left is empty" <|
            \side ->
                Dict.union Dict.empty side
                    |> expectEqualDicts side

        --
        , fuzz (dictFuzz Fuzz.int Fuzz.char) "right is empty" <|
            \side ->
                Dict.union side Dict.empty
                    |> expectEqualDicts side

        --
        , test "unions" <|
            \_ ->
                Dict.union
                    (Dict.fromList
                        [ ( 0, 'A' )
                        , ( 1, 'B' )
                        , ( 2, 'C' )
                        , ( 3, 'D' )
                        ]
                    )
                    (Dict.fromList
                        [ ( 2, 'c' )
                        , ( 3, 'd' )
                        , ( 4, 'e' )
                        , ( 5, 'f' )
                        ]
                    )
                    |> Dict.toList
                    |> Expect.equalLists
                        [ ( 0, 'A' )
                        , ( 1, 'B' )
                        , ( 2, 'C' )
                        , ( 3, 'D' )
                        , ( 4, 'e' )
                        , ( 5, 'f' )
                        ]
        ]


intersectSuite : Test
intersectSuite =
    describe "AVL.Dict.intersect"
        [ fuzz (dictFuzz Fuzz.int Fuzz.char) "left is empty" <|
            \side ->
                Dict.intersect Dict.empty side
                    |> expectEqualDicts Dict.empty

        --
        , fuzz (dictFuzz Fuzz.int Fuzz.char) "right is empty" <|
            \side ->
                Dict.intersect side Dict.empty
                    |> expectEqualDicts Dict.empty

        --
        , test "intersects" <|
            \_ ->
                Dict.intersect
                    (Dict.fromList
                        [ ( 0, 'A' )
                        , ( 1, 'B' )
                        , ( 2, 'C' )
                        , ( 3, 'D' )
                        ]
                    )
                    (Dict.fromList
                        [ ( 2, 'c' )
                        , ( 3, 'd' )
                        , ( 4, 'e' )
                        , ( 5, 'f' )
                        ]
                    )
                    |> Dict.toList
                    |> Expect.equalLists
                        [ ( 2, 'C' )
                        , ( 3, 'D' )
                        ]
        ]


diffSuite : Test
diffSuite =
    describe "AVL.Dict.diff"
        [ fuzz (dictFuzz Fuzz.int Fuzz.char) "left is empty" <|
            \side ->
                Dict.diff Dict.empty side
                    |> expectEqualDicts Dict.empty

        --
        , fuzz (dictFuzz Fuzz.int Fuzz.char) "right is empty" <|
            \side ->
                Dict.diff side Dict.empty
                    |> expectEqualDicts side

        --
        , test "example" <|
            \_ ->
                Dict.diff
                    (Dict.fromList
                        [ ( 0, 'A' )
                        , ( 1, 'B' )
                        , ( 2, 'C' )
                        , ( 3, 'D' )
                        ]
                    )
                    (Dict.fromList
                        [ ( 2, 'c' )
                        , ( 3, 'd' )
                        , ( 4, 'e' )
                        , ( 5, 'f' )
                        ]
                    )
                    |> Dict.toList
                    |> Expect.equalLists
                        [ ( 0, 'A' )
                        , ( 1, 'B' )
                        ]
        ]


mergeSuite : Test
mergeSuite =
    let
        mergeTest =
            Dict.merge
                (\key left acc -> String.fromList (List.repeat key left) :: acc)
                (\key left right acc -> (String.fromList (List.repeat key left) ++ right) :: acc)
                (\key right acc -> (String.fromInt key ++ right) :: acc)
    in
    describe "AVL.Dict.merge"
        [ test "left is empty" <|
            \_ ->
                mergeTest
                    Dict.empty
                    (Dict.singleton 0 "A")
                    []
                    |> Expect.equalLists
                        [ "0A"
                        ]

        --
        , test "right is empty" <|
            \_ ->
                mergeTest
                    (Dict.singleton 3 'A')
                    Dict.empty
                    []
                    |> Expect.equalLists
                        [ "AAA"
                        ]

        --
        , test "merges" <|
            \_ ->
                mergeTest
                    (Dict.fromList
                        [ ( 0, 'A' )
                        , ( 1, 'B' )
                        , ( 2, 'C' )
                        , ( 3, 'D' )
                        ]
                    )
                    (Dict.fromList
                        [ ( 2, "c" )
                        , ( 3, "d" )
                        , ( 4, "e" )
                        , ( 5, "f" )
                        ]
                    )
                    []
                    |> Expect.equalLists
                        [ "5f"
                        , "4e"
                        , "DDDd"
                        , "CCc"
                        , "B"
                        , ""
                        ]
        ]


expectEqualDicts : AVLDict key value -> (AVLDict key value -> Expectation)
expectEqualDicts expected =
    \actual ->
        Expect.equalLists (Dict.toList expected) (Dict.toList actual)


dictFuzz :
    Fuzzer comparableKey
    -> Fuzzer value
    -> Fuzzer (AVLDict comparableKey value)
dictFuzz keyFuzz valueFuzz =
    Fuzz.map Dict.fromList <|
        Fuzz.list (Fuzz.pair keyFuzz valueFuzz)
