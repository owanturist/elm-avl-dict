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


i : Insertion key value -> Node key value
i { key, value, left, right } =
    c key value left right



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
        ( added, insertion ) =
            insertHelp key value node
    in
    if added then
        ( count + 1, i insertion )

    else
        ( count, i insertion )



-- M A N I P U L A T I O N


{-| -}
insert : comparable -> value -> AVL comparable value -> AVL comparable value
insert key value (Internal.AVL count root) =
    let
        ( added, insertion ) =
            insertHelp key value root
    in
    if added then
        Internal.AVL (count + 1) (i insertion)

    else
        Internal.AVL count (i insertion)


type alias Insertion key value =
    { key : key
    , value : value
    , left : Node key value
    , right : Node key value
    }


insertHelp : comparable -> value -> Node comparable value -> ( Bool, Insertion comparable value )
insertHelp key value node =
    case node of
        RBEmpty_elm_builtin ->
            ( True, Insertion key value e e )

        RBNode_elm_builtin _ k v l r ->
            case compare key k of
                LT ->
                    let
                        ( added, insertion ) =
                            insertHelp key value l
                    in
                    ( added
                    , if max (h insertion.left) (h insertion.right) > h r then
                        rotateRight k v insertion r

                      else
                        Insertion k v (i insertion) r
                    )

                GT ->
                    let
                        ( added, insertion ) =
                            insertHelp key value r
                    in
                    ( added
                    , if h l < max (h insertion.left) (h insertion.right) then
                        rotateLeft k v l insertion

                      else
                        Insertion k v l (i insertion)
                    )

                EQ ->
                    ( False
                    , Insertion key value l r
                    )


rotateLeft : comparable -> value -> Node comparable value -> Insertion comparable value -> Insertion comparable value
rotateLeft pk pv pl insertion =
    case insertion.left of
        RBEmpty_elm_builtin ->
            Insertion insertion.key insertion.value (c pk pv pl insertion.left) insertion.right

        RBNode_elm_builtin lh lk lv ll lr ->
            if lh > h insertion.right then
                Insertion lk lv (c pk pv pl ll) (c insertion.key insertion.value lr insertion.right)

            else
                Insertion insertion.key insertion.value (c pk pv pl insertion.left) insertion.right


rotateRight : comparable -> value -> Insertion comparable value -> Node comparable value -> Insertion comparable value
rotateRight pk pv insertion pr =
    case insertion.right of
        RBEmpty_elm_builtin ->
            Insertion insertion.key insertion.value insertion.left (c pk pv insertion.right pr)

        RBNode_elm_builtin rh rk rv rl rr ->
            if h insertion.left < rh then
                Insertion rk rv (c insertion.key insertion.value insertion.left rl) (c pk pv rr pr)

            else
                Insertion insertion.key insertion.value insertion.left (c pk pv insertion.right pr)



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
