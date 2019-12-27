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
        ( added, nextRoot ) =
            insertHelp key value root
    in
    if added then
        Internal.AVL (count + 1) nextRoot

    else
        Internal.AVL count nextRoot


insertHelp : comparable -> value -> Node comparable value -> ( Bool, Node comparable value )
insertHelp key value node =
    -- let
    --     _ =
    --         Debug.log "insertHelp" key
    -- in
    case node of
        RBEmpty_elm_builtin ->
            ( True, s key value )

        RBNode_elm_builtin height k v l r ->
            case compare key k of
                LT ->
                    insertLeft key value k v l r

                GT ->
                    insertRight key value k v l r

                EQ ->
                    -- let
                    --     _ =
                    --         Debug.log "ih" k
                    -- in
                    ( False
                    , RBNode_elm_builtin height key value l r
                    )


insertLeft : comparable -> value -> comparable -> value -> Node comparable value -> Node comparable value -> ( Bool, Node comparable value )
insertLeft key value pk pv pl pr =
    -- let
    --     _ =
    --         Debug.log "insertLeft" key
    -- in
    case pl of
        RBEmpty_elm_builtin ->
            ( True
            , c pk pv (s key value) pr
            )

        RBNode_elm_builtin lh lk lv ll lr ->
            if lh > h pr then
                case compare key lk of
                    LT ->
                        let
                            ( added, nextLL ) =
                                insertHelp key value ll
                        in
                        ( added
                        , c lk lv nextLL (c pk pv lr pr)
                        )

                    GT ->
                        case lr of
                            RBEmpty_elm_builtin ->
                                ( True
                                , c key value pl (c pk pv e pr)
                                )

                            RBNode_elm_builtin lrh lrk lrv lrl lrr ->
                                case compare key lrk of
                                    LT ->
                                        let
                                            ( added, nextLRL ) =
                                                insertHelp key value lrl
                                        in
                                        ( added
                                        , c lrk lrv (c lk lv ll nextLRL) (c pk pv lrr pr)
                                        )

                                    GT ->
                                        let
                                            ( added, nextLRR ) =
                                                insertHelp key value lrr
                                        in
                                        ( added
                                        , c lrk lrv (c lk lv ll lrl) (c pk pv nextLRR pr)
                                        )

                                    EQ ->
                                        ( False
                                        , c pk
                                            pv
                                            (RBNode_elm_builtin lh
                                                lk
                                                lv
                                                ll
                                                (RBNode_elm_builtin lrh key value lrl lrr)
                                            )
                                            pr
                                        )

                    EQ ->
                        -- let
                        --     _ =
                        --         Debug.log "il" lk
                        -- in
                        ( False
                        , c pk pv (RBNode_elm_builtin lh key value ll lr) pr
                        )

            else
                let
                    ( added, nextPL ) =
                        insertHelp key value pl
                in
                ( added
                , c pk pv nextPL pr
                )


insertRight : comparable -> value -> comparable -> value -> Node comparable value -> Node comparable value -> ( Bool, Node comparable value )
insertRight key value pk pv pl pr =
    -- let
    --     _ =
    --         Debug.log "insertRight" key
    -- in
    case pr of
        RBEmpty_elm_builtin ->
            ( True
            , c pk pv pl (s key value)
            )

        RBNode_elm_builtin rh rk rv rl rr ->
            if rh > h pl then
                case compare key rk of
                    LT ->
                        case rl of
                            RBEmpty_elm_builtin ->
                                ( True
                                , c key value (c pk pv pl e) pr
                                )

                            RBNode_elm_builtin rlh rlk rlv rll rlr ->
                                case compare key rlk of
                                    LT ->
                                        let
                                            ( added, nextRLL ) =
                                                insertHelp key value rll
                                        in
                                        ( added
                                        , c rlk rlv (c pk pv pl nextRLL) (c rk rv rlr rr)
                                        )

                                    GT ->
                                        let
                                            ( added, nextRLR ) =
                                                insertHelp key value rlr
                                        in
                                        ( added
                                        , c rlk rlv (c pk pv pl rll) (c rk rv nextRLR rr)
                                        )

                                    EQ ->
                                        ( False
                                        , c pk
                                            pv
                                            pl
                                            (RBNode_elm_builtin rh
                                                rk
                                                rv
                                                (RBNode_elm_builtin rlh key value rll rlr)
                                                rr
                                            )
                                        )

                    GT ->
                        let
                            ( added, nextRR ) =
                                insertHelp key value rr
                        in
                        ( added
                        , c rk rv (c pk pv pl rl) nextRR
                        )

                    EQ ->
                        -- let
                        --     _ =
                        --         Debug.log "ir" rk
                        -- in
                        ( False
                        , c pk pv pl (RBNode_elm_builtin rh key value rl rr)
                        )

            else
                let
                    ( added, nextPR ) =
                        insertHelp key value pr
                in
                ( added
                , c pk pv pl nextPR
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
