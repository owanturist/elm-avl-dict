module AVLTests exposing (..)

import AVL
import Expect exposing (Expectation)
import Fuzz
import Internal exposing (AVL(..), Node(..))
import List.Extra
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


draw : (key -> String) -> (value -> String) -> AVL key value -> String
draw keyToString valueToString (AVL _ root) =
    String.join "\n" (drawHelp keyToString valueToString root)


shiftLeft : List String -> List String
shiftLeft lines =
    case lines of
        [] ->
            []

        first :: rest ->
            "| | "
                :: ("| +-" ++ first)
                :: List.map ((++) "|   ") rest


shiftRight : List String -> List String
shiftRight lines =
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
                :: shiftLeft (drawHelp keyToString valueToString left)
                ++ shiftRight (drawHelp keyToString valueToString right)


expectValid : (key -> String) -> AVL key value -> Expectation
expectValid keyToString avl =
    case validate keyToString avl of
        Err error ->
            Expect.fail error

        Ok _ ->
            Expect.pass


validate : (key -> String) -> AVL key value -> Result String (AVL key value)
validate keyToString ((AVL _ root) as avl) =
    Result.map (always avl) (validateHelp keyToString root)


validateHelp : (key -> String) -> Node key value -> Result String Int
validateHelp keyToString node =
    case node of
        RBEmpty_elm_builtin ->
            Ok 0

        RBNode_elm_builtin _ key _ left right ->
            Result.andThen
                (\( lh, rh ) ->
                    if abs (lh - rh) > 1 then
                        Err ("height diff [" ++ keyToString key ++ "]: " ++ String.fromInt lh ++ " vs " ++ String.fromInt rh)

                    else
                        Ok (1 + max lh rh)
                )
                (Result.map2 Tuple.pair
                    (validateHelp keyToString left)
                    (validateHelp keyToString right)
                )


size : AVL key value -> Int
size (AVL _ root) =
    sizeHelp root


sizeHelp : Node key value -> Int
sizeHelp node =
    case node of
        RBEmpty_elm_builtin ->
            0

        RBNode_elm_builtin _ _ _ left right ->
            1 + sizeHelp left + sizeHelp right



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

        -- , fuzz (Fuzz.list (Fuzz.intRange -200 200)) "random keys" <|
        --     \list ->
        --         list
        --             |> List.foldl
        --                 (\i -> Result.andThen (validate String.fromInt << AVL.insert i (String.fromInt i)))
        --                 (Ok AVL.empty)
        --             |> Expect.ok
        , test "RAND" <|
            \_ ->
                [ ( 2, 'A' ), ( 0, 'B' ), ( 3, 'C' ), ( 1, 'D' ), ( 1, 'E' ) ]
                    |> List.foldl (\( k, v ) -> AVL.insert k v) AVL.empty
                    |> AVL.size
                    |> Expect.equal 4
        , test "[5,1,2,0,6,3,4,4]" <|
            \_ ->
                [ 5, 1, 2, 0, 6, 3, 4, 4 ]
                    |> List.foldl (\i -> AVL.insert i (i * i)) AVL.empty
                    -- |> validate String.fromInt
                    |> draw String.fromInt String.fromInt
                    |> Expect.equal ""
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
                let
                    uniq =
                        List.Extra.uniqueBy Tuple.first list
                in
                AVL.fromList list
                    |> AVL.isEmpty
                    |> Expect.equal (List.isEmpty uniq)
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
        ]
