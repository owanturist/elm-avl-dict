module DictTests exposing (..)

import AVL.Dict as Dict
import Expect
import Fuzz
import Internal exposing (AVL(..), Node(..))
import List.Extra
import Test exposing (Test, describe, fuzz, fuzz2, test)


draw : (key -> String) -> (value -> String) -> AVL key value -> String
draw keyToString valueToString (AVL _ _ root) =
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


validate : (comparable -> String) -> AVL comparable value -> Result String (AVL comparable value)
validate keyToString (AVL comparator size root) =
    Result.andThen
        (\( _, s ) ->
            if s == size then
                Ok (AVL comparator size root)

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
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.int, Fuzz.char ))) "fromList" <|
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
        , fuzz2 (Fuzz.intRange -400 -100) (Fuzz.intRange 100 400) "ascending keys" <|
            \lo hi ->
                List.range lo hi
                    |> List.foldr
                        (\i -> Result.andThen (validate String.fromInt << Dict.insert i (String.fromInt i)))
                        (Ok Dict.empty)
                    |> Expect.ok

        --
        , fuzz2 (Fuzz.intRange -400 -100) (Fuzz.intRange 100 400) "descending keys" <|
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
        , fuzz2 (Fuzz.list (Fuzz.tuple ( Fuzz.int, Fuzz.char ))) (Fuzz.list Fuzz.int) "AVL.Dict.fromList" <|
            \puts deletes ->
                List.foldl
                    (\key -> Result.andThen (validate String.fromInt << Dict.remove key))
                    (Ok (Dict.fromList puts))
                    deletes
                    |> Expect.ok

        --
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.int, Fuzz.char ))) "clear" <|
            \list ->
                List.foldl
                    (\( key, _ ) -> Result.andThen (validate String.fromInt << Dict.remove key))
                    (Ok (Dict.fromList list))
                    list
                    |> Result.map Dict.isEmpty
                    |> Result.withDefault False
                    |> Expect.equal True
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
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.char, Fuzz.int ))) "AVL.Dict.fromList" <|
            \list ->
                Dict.fromList list
                    |> Dict.isEmpty
                    |> Expect.equal (List.isEmpty list)
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
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.string, Fuzz.int ))) "AVL.Dict.fromList" <|
            \list ->
                let
                    uniq =
                        List.Extra.uniqueBy Tuple.first list
                in
                Dict.fromList list
                    |> Dict.size
                    |> Expect.equal (List.length uniq)
        ]



-- T R A N S F O R M


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
