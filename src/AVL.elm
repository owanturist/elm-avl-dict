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


diff : Node key value -> Node key value -> Order
diff left right =
    let
        d =
            h left - h right
    in
    if d < -1 then
        LT

    else if d > 1 then
        GT

    else
        EQ



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
        ( modified, nextRoot ) =
            insertHelp key value node
    in
    if modified then
        ( count + 1, nextRoot )

    else
        ( count, nextRoot )



-- M A N I P U L A T I O N


{-| -}
insert : comparable -> value -> AVL comparable value -> AVL comparable value
insert key value (Internal.AVL count root) =
    let
        ( modified, nextRoot ) =
            insertHelp key value root
    in
    if modified then
        Internal.AVL (count + 1) nextRoot

    else
        Internal.AVL count nextRoot


insertHelp : comparable -> value -> Node comparable value -> ( Bool, Node comparable value )
insertHelp key value node =
    case node of
        RBEmpty_elm_builtin ->
            ( True, s key value )

        RBNode_elm_builtin height k v left right ->
            case compare key k of
                LT ->
                    case insertHelp key value left of
                        ( True, (RBNode_elm_builtin lh lk lv ll lr) as nextLeft ) ->
                            ( True
                            , if lh - h right > 1 then
                                rotateRight k v right lk lv ll lr

                              else
                                c k v nextLeft right
                            )

                        ( _, nextLeft ) ->
                            ( False
                            , RBNode_elm_builtin height k v nextLeft right
                            )

                GT ->
                    case insertHelp key value right of
                        ( True, (RBNode_elm_builtin rh rk rv rl rr) as nextRight ) ->
                            ( True
                            , if h left - rh < -1 then
                                rotateLeft k v left rk rv rl rr

                              else
                                c k v left nextRight
                            )

                        ( _, nextRight ) ->
                            ( False
                            , RBNode_elm_builtin height k v left nextRight
                            )

                EQ ->
                    ( False
                    , RBNode_elm_builtin height key value left right
                    )


rotateLeft : comparable -> value -> Node comparable value -> comparable -> value -> Node comparable value -> Node comparable value -> Node comparable value
rotateLeft pk pv pl rk rv rl rr =
    case rl of
        RBNode_elm_builtin lh lk lv ll lr ->
            if lh > h rr then
                c lk lv (c pk pv pl ll) (c rk rv lr rr)

            else
                c rk rv (c pk pv pl rl) rr

        _ ->
            c rk rv (c pk pv pl rl) rr


rotateRight : comparable -> value -> Node comparable value -> comparable -> value -> Node comparable value -> Node comparable value -> Node comparable value
rotateRight pk pv pr lk lv ll lr =
    case lr of
        RBNode_elm_builtin rh rk rv rl rr ->
            if h ll < rh then
                c rk rv (c lk lv ll rl) (c pk pv rr pr)

            else
                c lk lv ll (c pk pv lr pr)

        _ ->
            c lk lv ll (c pk pv lr pr)



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
