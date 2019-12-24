module AVL exposing
    ( AVL
    , empty, singleton, fromList
    , insert
    , isEmpty, size, member, get
    )

{-| An AVL Tree based dictionary.


# Dictionary

@docs AVL


# Construction

@docs empty, singleton, fromList


# Manipulation

@docs insert


# Query

@docs isEmpty, size, member, get

-}

import Internal exposing (Node(..))


{-| -}
type alias AVL key value =
    Internal.AVL key value



-- U T I L S


h : Node key value -> Int
h node =
    case node of
        RBEmpty_elm_builtin ->
            0

        RBNode_elm_builtin x _ _ _ _ ->
            x


e : Node key value
e =
    RBEmpty_elm_builtin


s : key -> value -> Node key value
s key value =
    RBNode_elm_builtin 1 key value e e


c : key -> value -> Node key value -> Node key value -> Node key value
c key value left right =
    RBNode_elm_builtin (1 + max (h left) (h right)) key value left right



-- C O N S T R U C T I O N


{-| -}
empty : AVL comparable value
empty =
    Internal.AVL 0 e


{-| -}
singleton : comparable -> value -> AVL comparable value
singleton key value =
    Internal.AVL 1 (s key value)


{-| -}
fromList : List ( comparable, value ) -> AVL comparable value
fromList keyValues =
    let
        ( count, root ) =
            List.foldl fromListHelper ( 0, e ) keyValues
    in
    Internal.AVL count root


fromListHelper : ( comparable, value ) -> ( Int, Node comparable value ) -> ( Int, Node comparable value )
fromListHelper ( key, value ) ( count, node ) =
    let
        ( added, nextRoot ) =
            insertHelp key value node
    in
    if added then
        ( count + 1, nextRoot )

    else
        ( count, nextRoot )



-- M A N I P U L A T I O N


{-| -}
insert : comparable -> value -> AVL comparable value -> AVL comparable value
insert key value (Internal.AVL count root) =
    let
        ( added, nextRoot ) =
            insertHelp key value root
    in
    if added then
        Internal.AVL (count + 1) nextRoot

    else
        Internal.AVL count nextRoot


insertHelp : comparable -> value -> Node comparable value -> ( Bool, Node comparable value )
insertHelp key value node =
    case node of
        RBEmpty_elm_builtin ->
            ( True, s key value )

        RBNode_elm_builtin height k v left right ->
            case ( compare key k, left, right ) of
                ( LT, RBEmpty_elm_builtin, _ ) ->
                    ( True
                    , c k v (s key value) right
                    )

                ( LT, RBNode_elm_builtin _ lk lv ll lr, RBEmpty_elm_builtin ) ->
                    ( True
                    , c lk lv (s key value) (s k v)
                    )

                ( LT, _, RBNode_elm_builtin _ rk rv rl rr ) ->
                    let
                        ( added, nextLeft ) =
                            insertHelp key value left
                    in
                    ( added
                    , Debug.todo ""
                    )

                ( GT, _, _ ) ->
                    Debug.todo ""

                ( EQ, _, _ ) ->
                    ( False
                    , RBNode_elm_builtin height key value left right
                    )



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
