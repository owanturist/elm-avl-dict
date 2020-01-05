module AVL exposing
    ( AVL
    , empty, singleton, fromList
    , insert, remove
    , isEmpty, size, member, get, getMin, getMax
    )

{-| An AVL Tree based dictionary.


# Dictionary

@docs AVL


# Construction

@docs empty, singleton, fromList


# Manipulation

@docs insert, remove


# Query

@docs isEmpty, size, member, get, getMin, getMax

-}

import Internal exposing (Node(..))


{-| -}
type alias AVL key value =
    Internal.AVL key value



-- U T I L S


height : Node key value -> Int
height node =
    case node of
        RBEmpty_elm_builtin ->
            0

        RBNode_elm_builtin x _ _ _ _ ->
            x


nil : Node key value
nil =
    RBEmpty_elm_builtin


leaf : key -> value -> Node key value -> Node key value -> Node key value
leaf key value left right =
    RBNode_elm_builtin (1 + max (height left) (height right)) key value left right


ins : Insertion key value -> Node key value
ins { key, value, left, right } =
    leaf key value left right



-- C O N S T R U C T I O N


{-| -}
empty : AVL comparable value
empty =
    Internal.AVL 0 nil


{-| -}
singleton : comparable -> value -> AVL comparable value
singleton key value =
    Internal.AVL 1 (leaf key value nil nil)


{-| -}
fromList : List ( comparable, value ) -> AVL comparable value
fromList keyValues =
    let
        ( count, root ) =
            List.foldl fromListHelper ( 0, nil ) keyValues
    in
    Internal.AVL count root


fromListHelper : ( comparable, value ) -> ( Int, Node comparable value ) -> ( Int, Node comparable value )
fromListHelper ( key, value ) ( count, node ) =
    let
        insertion =
            insertHelp key value node
    in
    if insertion.added then
        ( count + 1, ins insertion )

    else
        ( count, ins insertion )



-- M A N I P U L A T I O N


{-| -}
insert : comparable -> value -> AVL comparable value -> AVL comparable value
insert key value (Internal.AVL count root) =
    let
        insertion =
            insertHelp key value root
    in
    if insertion.added then
        Internal.AVL (count + 1) (ins insertion)

    else
        Internal.AVL count (ins insertion)


type alias Insertion key value =
    { added : Bool
    , key : key
    , value : value
    , left : Node key value
    , right : Node key value
    }


insertHelp : comparable -> value -> Node comparable value -> Insertion comparable value
insertHelp key value node =
    case node of
        RBEmpty_elm_builtin ->
            Insertion True key value nil nil

        RBNode_elm_builtin _ k v l r ->
            case compare key k of
                LT ->
                    let
                        insertion =
                            insertHelp key value l
                    in
                    -- equal to: 1 + (max heightInsertionLeft heightInsertionRight) - heightRight > 1
                    if max (height insertion.left) (height insertion.right) > height r then
                        rotateRight k v insertion r

                    else
                        Insertion insertion.added k v (ins insertion) r

                GT ->
                    let
                        insertion =
                            insertHelp key value r
                    in
                    -- equal to: heightLeft - (1 + (max heightInsertionLeft heightInsertionRight)) < 1
                    if height l < max (height insertion.left) (height insertion.right) then
                        rotateLeft k v l insertion

                    else
                        Insertion insertion.added k v l (ins insertion)

                EQ ->
                    Insertion False key value l r


rotateLeft : comparable -> value -> Node comparable value -> Insertion comparable value -> Insertion comparable value
rotateLeft pk pv pl insertion =
    case insertion.left of
        RBEmpty_elm_builtin ->
            Insertion True insertion.key insertion.value (leaf pk pv pl insertion.left) insertion.right

        RBNode_elm_builtin lh lk lv ll lr ->
            if lh > height insertion.right then
                Insertion True lk lv (leaf pk pv pl ll) (leaf insertion.key insertion.value lr insertion.right)

            else
                Insertion True insertion.key insertion.value (leaf pk pv pl insertion.left) insertion.right


rotateRight : comparable -> value -> Insertion comparable value -> Node comparable value -> Insertion comparable value
rotateRight pk pv insertion pr =
    case insertion.right of
        RBEmpty_elm_builtin ->
            Insertion True insertion.key insertion.value insertion.left (leaf pk pv insertion.right pr)

        RBNode_elm_builtin rh rk rv rl rr ->
            if height insertion.left < rh then
                Insertion True rk rv (leaf insertion.key insertion.value insertion.left rl) (leaf pk pv rr pr)

            else
                Insertion True insertion.key insertion.value insertion.left (leaf pk pv insertion.right pr)


{-| -}
remove : comparable -> AVL comparable value -> AVL comparable value
remove key ((Internal.AVL count root) as avl) =
    case removeHelp key root of
        Nothing ->
            avl

        Just nextRoot ->
            Internal.AVL (max 0 (count - 1)) nextRoot


removeHelp : comparable -> Node comparable value -> Maybe (Node comparable value)
removeHelp key node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ k v l r ->
            case compare key k of
                LT ->
                    Nothing

                GT ->
                    Nothing

                EQ ->
                    Nothing



-- Q U E R Y


{-| -}
isEmpty : AVL key value -> Bool
isEmpty avl =
    size avl == 0


{-| -}
size : AVL key value -> Int
size (Internal.AVL count _) =
    count


{-| -}
member : comparable -> AVL comparable value -> Bool
member key avl =
    get key avl /= Nothing


{-| -}
get : comparable -> AVL comparable value -> Maybe value
get key (Internal.AVL _ root) =
    getHelper key root


getHelper : comparable -> Node comparable value -> Maybe value
getHelper target node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ key value left right ->
            case compare target key of
                LT ->
                    getHelper target left

                GT ->
                    getHelper target right

                EQ ->
                    Just value


{-| -}
getMin : AVL comparable value -> Maybe ( comparable, value )
getMin (Internal.AVL _ root) =
    getMinHelper root


getMinHelper : Node comparable value -> Maybe ( comparable, value )
getMinHelper node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ k v l _ ->
            case getMinHelper l of
                Nothing ->
                    Just ( k, v )

                min ->
                    min


{-| -}
getMax : AVL comparable value -> Maybe ( comparable, value )
getMax (Internal.AVL _ root) =
    getMaxHelper root


getMaxHelper : Node comparable value -> Maybe ( comparable, value )
getMaxHelper node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ k v _ r ->
            case getMinHelper r of
                Nothing ->
                    Just ( k, v )

                max ->
                    max
