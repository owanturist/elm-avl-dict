module AVLTests exposing (..)

import AVL
import Expect
import Fuzz
import Internal exposing (AVL(..), Node(..))
import List.Extra
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


draw : (key -> String) -> (value -> String) -> AVL key value -> String
draw keyToString valueToString (AVL _ root) =
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
validate keyToString (AVL size root) =
    Result.andThen
        (\( _, s ) ->
            if s == size then
                Ok (AVL size root)

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



-- M A N I P U L A T I O N


insertSuite : Test
insertSuite =
    describe "AVL.insert"
        [ fuzz2 Fuzz.int Fuzz.string "AVL.empty" <|
            \key value ->
                AVL.empty
                    |> AVL.insert key value
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to left" <|
            \_ ->
                AVL.singleton 10 'a'
                    |> AVL.insert 5 'b'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to left left" <|
            \_ ->
                AVL.singleton 10 'a'
                    |> AVL.insert 5 'b'
                    |> AVL.insert 2 'c'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to left right" <|
            \_ ->
                AVL.singleton 10 'a'
                    |> AVL.insert 5 'b'
                    |> AVL.insert 8 'c'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to right" <|
            \_ ->
                AVL.singleton 10 'a'
                    |> AVL.insert 15 'b'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to right left" <|
            \_ ->
                AVL.singleton 10 'a'
                    |> AVL.insert 15 'b'
                    |> AVL.insert 12 'c'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "to right right" <|
            \_ ->
                AVL.singleton 10 'a'
                    |> AVL.insert 15 'b'
                    |> AVL.insert 20 'c'
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , test "M-N-O-L-K-Q-P-H-I-A" <|
            \_ ->
                "MNOLKQPHIA"
                    |> String.toList
                    |> List.foldl
                        (\char -> Result.andThen (validate String.fromChar << AVL.insert char (Char.toCode char)))
                        (Ok AVL.empty)
                    |> Expect.ok

        --
        , fuzz2 (Fuzz.intRange -400 -100) (Fuzz.intRange 100 400) "ascending keys" <|
            \lo hi ->
                List.range lo hi
                    |> List.foldr
                        (\i -> Result.andThen (validate String.fromInt << AVL.insert i (String.fromInt i)))
                        (Ok AVL.empty)
                    |> Expect.ok

        --
        , fuzz2 (Fuzz.intRange -400 -100) (Fuzz.intRange 100 400) "descending keys" <|
            \lo hi ->
                List.range lo hi
                    |> List.foldl
                        (\i -> Result.andThen (validate String.fromInt << AVL.insert i (String.fromInt i)))
                        (Ok AVL.empty)
                    |> Expect.ok

        --
        , fuzz (Fuzz.list (Fuzz.intRange -200 200)) "random keys" <|
            \list ->
                list
                    |> List.foldl
                        (\i -> Result.andThen (validate String.fromInt << AVL.insert i (String.fromInt i)))
                        (Ok AVL.empty)
                    |> Expect.ok
        ]


removeSuite : Test
removeSuite =
    describe "AVL.remove"
        [ fuzz Fuzz.int "AVL.empty" <|
            \key ->
                AVL.empty
                    |> AVL.remove key
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , fuzz2 Fuzz.int Fuzz.int "AVL.singleton" <|
            \put delete ->
                AVL.singleton put ()
                    |> AVL.remove delete
                    |> validate String.fromInt
                    |> Expect.ok

        --
        , fuzz2 (Fuzz.list (Fuzz.tuple ( Fuzz.int, Fuzz.char ))) (Fuzz.list Fuzz.int) "AVL.fromList" <|
            \puts deletes ->
                List.foldl
                    (\key -> Result.andThen (validate String.fromInt << AVL.remove key))
                    (Ok (AVL.fromList puts))
                    deletes
                    |> Expect.ok

        --
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.int, Fuzz.char ))) "clear" <|
            \list ->
                List.foldl
                    (\( key, _ ) -> Result.andThen (validate String.fromInt << AVL.remove key))
                    (Ok (AVL.fromList list))
                    list
                    |> Result.map AVL.isEmpty
                    |> Result.withDefault False
                    |> Expect.equal True
        ]


removeMinSuite : Test
removeMinSuite =
    describe "AVL.removeMin"
        [ test "AVL.empty" <|
            \_ ->
                AVL.empty
                    |> AVL.removeMin
                    |> validate identity
                    |> Expect.ok

        --
        , fuzz2 Fuzz.char Fuzz.int "AVL.singleton" <|
            \key value ->
                AVL.singleton key value
                    |> AVL.removeMin
                    |> Expect.all
                        [ Expect.ok << validate String.fromChar
                        , Expect.equal Nothing << AVL.get key
                        ]

        --
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.char, Fuzz.int ))) "AVL.fromList" <|
            \list ->
                let
                    avl =
                        AVL.fromList list
                in
                Expect.all
                    [ Expect.ok << validate String.fromChar
                    , case AVL.getMin avl of
                        Nothing ->
                            always Expect.pass

                        Just ( key, _ ) ->
                            Expect.equal Nothing << AVL.get key
                    ]
                    (AVL.removeMin avl)

        --
        , fuzz3
            (Fuzz.map (\x -> ( x, String.fromInt x )) (Fuzz.intRange -1000 0))
            (Fuzz.list (Fuzz.map (\x -> ( x, String.fromInt x )) (Fuzz.intRange 1 1000)))
            (Fuzz.list (Fuzz.map (\x -> ( x, String.fromInt x )) (Fuzz.intRange 1 1000)))
            "AVL.fromList + AVL.insert"
          <|
            \( minKey, minValue ) left right ->
                (left ++ ( minKey, minValue ) :: right)
                    |> AVL.fromList
                    |> AVL.removeMin
                    |> Expect.all
                        [ Expect.ok << validate String.fromInt
                        , Expect.equal Nothing << AVL.get minKey
                        ]

        --
        , fuzz
            (Fuzz.map2 List.range
                (Fuzz.intRange -400 -100)
                (Fuzz.intRange 100 400)
                |> Fuzz.map (List.indexedMap Tuple.pair)
            )
            "clear"
          <|
            \list ->
                List.foldl
                    (\_ -> Result.andThen (validate String.fromInt << AVL.removeMin))
                    (Ok (AVL.fromList list))
                    list
                    |> Result.map AVL.isEmpty
                    |> Result.withDefault False
                    |> Expect.equal True
        ]


removeMaxSuite : Test
removeMaxSuite =
    describe "AVL.removeMax"
        [ test "AVL.empty" <|
            \_ ->
                AVL.empty
                    |> AVL.removeMax
                    |> validate identity
                    |> Expect.ok

        --
        , fuzz2 Fuzz.char Fuzz.int "AVL.singleton" <|
            \key value ->
                AVL.singleton key value
                    |> AVL.removeMax
                    |> Expect.all
                        [ Expect.ok << validate String.fromChar
                        , Expect.equal Nothing << AVL.get key
                        ]

        --
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.int, Fuzz.string ))) "AVL.fromList" <|
            \list ->
                let
                    avl =
                        AVL.fromList list
                in
                Expect.all
                    [ Expect.ok << validate String.fromInt
                    , case AVL.getMax avl of
                        Nothing ->
                            always Expect.pass

                        Just ( key, _ ) ->
                            Expect.equal Nothing << AVL.get key
                    ]
                    (AVL.removeMax avl)

        --
        , fuzz3
            (Fuzz.map (\x -> ( x, String.fromInt x )) (Fuzz.intRange 1001 2000))
            (Fuzz.list (Fuzz.map (\x -> ( x, String.fromInt x )) (Fuzz.intRange 1 1000)))
            (Fuzz.list (Fuzz.map (\x -> ( x, String.fromInt x )) (Fuzz.intRange 1 1000)))
            "AVL.fromList + AVL.insert"
          <|
            \( maxKey, maxValue ) left right ->
                (left ++ ( maxKey, maxValue ) :: right)
                    |> AVL.fromList
                    |> AVL.removeMax
                    |> Expect.all
                        [ Expect.ok << validate String.fromInt
                        , Expect.equal Nothing << AVL.get maxKey
                        ]

        --
        , fuzz
            (Fuzz.map2 List.range
                (Fuzz.intRange -400 -100)
                (Fuzz.intRange 100 400)
                |> Fuzz.map (List.indexedMap Tuple.pair)
            )
            "clear"
          <|
            \list ->
                List.foldl
                    (\_ -> Result.andThen (validate String.fromInt << AVL.removeMax))
                    (Ok (AVL.fromList list))
                    list
                    |> Result.map AVL.isEmpty
                    |> Result.withDefault False
                    |> Expect.equal True
        ]



-- Q U E R Y


isEmptySuite : Test
isEmptySuite =
    describe "AVL.isEmpty"
        [ test "AVL.empty" <|
            \_ ->
                AVL.empty
                    |> AVL.isEmpty
                    |> Expect.equal True

        --
        , fuzz2 Fuzz.char Fuzz.string "AVL.singleton" <|
            \key value ->
                AVL.singleton key value
                    |> AVL.isEmpty
                    |> Expect.equal False

        --
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.char, Fuzz.int ))) "AVL.fromList" <|
            \list ->
                AVL.fromList list
                    |> AVL.isEmpty
                    |> Expect.equal (List.isEmpty list)
        ]


sizeSuite : Test
sizeSuite =
    describe "AVL.size"
        [ test "AVL.empty" <|
            \_ ->
                AVL.empty
                    |> AVL.size
                    |> Expect.equal 0

        --
        , fuzz2 Fuzz.char Fuzz.string "AVL.singleton" <|
            \key value ->
                AVL.singleton key value
                    |> AVL.size
                    |> Expect.equal 1

        --
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.string, Fuzz.int ))) "AVL.fromList" <|
            \list ->
                let
                    uniq =
                        List.Extra.uniqueBy Tuple.first list
                in
                AVL.fromList list
                    |> AVL.size
                    |> Expect.equal (List.length uniq)

        --
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.string, Fuzz.int ))) "AVL.removeMin" <|
            \list ->
                let
                    uniq =
                        List.Extra.uniqueBy Tuple.first list
                in
                AVL.fromList list
                    |> AVL.removeMin
                    |> AVL.size
                    |> Expect.equal (max 0 (List.length uniq - 1))

        --
        , fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.string, Fuzz.int ))) "AVL.removeMax" <|
            \list ->
                let
                    uniq =
                        List.Extra.uniqueBy Tuple.first list
                in
                AVL.fromList list
                    |> AVL.removeMax
                    |> AVL.size
                    |> Expect.equal (max 0 (List.length uniq - 1))
        ]
