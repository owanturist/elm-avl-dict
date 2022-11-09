module Internal exposing
    ( AVLDict(..)
    , AVLSet(..)
    , Node(..)
    , Set(..)
    , dictKeyComparator
    , filter
    , foldl
    , foldr
    , fromList
    , get
    , height
    , insert
    , leaf
    , map
    , maximum
    , minimum
    , nil
    , partition
    , remove
    , setKeyComparator
    , singleton
    )


type AVLDict key value
    = AVLDict (key -> key -> Order) Int (Node key value)


type AVLSet key
    = AVLSet (key -> key -> Order) Int (Set key)


type Set key
    = Set_elm_builtin (Node key ())


type Node key value
    = RBEmpty_elm_builtin
    | RBNode_elm_builtin Int key value (Node key value) (Node key value)


type alias Comparator key =
    key -> key -> Order



-- U T I L S


dictKeyComparator : AVLDict key value -> Comparator key
dictKeyComparator (AVLDict keyComparatorInternal _ _) =
    keyComparatorInternal


setKeyComparator : AVLSet key -> Comparator key
setKeyComparator (AVLSet keyComparatorInternal _ _) =
    keyComparatorInternal


height : Node key value -> Int
height node =
    case node of
        RBEmpty_elm_builtin ->
            0

        RBNode_elm_builtin h _ _ _ _ ->
            h


nil : Node key value
nil =
    RBEmpty_elm_builtin


leaf : key -> value -> Node key value -> Node key value -> Node key value
leaf key value left right =
    RBNode_elm_builtin (1 + max (height left) (height right)) key value left right



-- C O N S T R U C T I O N


singleton : key -> value -> Node key value
singleton key value =
    RBNode_elm_builtin 1 key value nil nil


fromList :
    Comparator key
    -> (entity -> key)
    -> (entity -> value)
    -> entity
    -> ( Int, Node key value )
    -> ( Int, Node key value )
fromList comparator toKey toValue entity ( count, root ) =
    let
        ( added, nextRoot ) =
            insert comparator (toKey entity) (toValue entity) root
    in
    if added then
        ( count + 1, nextRoot )

    else
        ( count, nextRoot )



-- M A N I P U L A T I O N


insert : Comparator key -> key -> value -> Node key value -> ( Bool, Node key value )
insert comparator key value node =
    case node of
        RBEmpty_elm_builtin ->
            ( True
            , RBNode_elm_builtin 1 key value nil nil
            )

        RBNode_elm_builtin h k v l r ->
            case comparator key k of
                LT ->
                    let
                        ( added, nextL ) =
                            insert comparator key value l
                    in
                    ( added, balance k v nextL r )

                GT ->
                    let
                        ( added, nextR ) =
                            insert comparator key value r
                    in
                    ( added, balance k v l nextR )

                EQ ->
                    ( False
                    , RBNode_elm_builtin h key value l r
                    )


balance : key -> value -> Node key value -> Node key value -> Node key value
balance pk pv pl pr =
    case ( pl, pr ) of
        ( RBEmpty_elm_builtin, RBEmpty_elm_builtin ) ->
            RBNode_elm_builtin 1 pk pv nil nil

        ( RBNode_elm_builtin lh lk lv ll lr, RBEmpty_elm_builtin ) ->
            if lh > 1 then
                rotateRight pk pv lk lv ll lr pr

            else
                RBNode_elm_builtin (1 + lh) pk pv pl pr

        ( RBEmpty_elm_builtin, RBNode_elm_builtin rh rk rv rl rr ) ->
            if rh > 1 then
                rotateLeft pk pv pl rk rv rl rr

            else
                RBNode_elm_builtin (1 + rh) pk pv pl pr

        ( RBNode_elm_builtin lh lk lv ll lr, RBNode_elm_builtin rh rk rv rl rr ) ->
            if lh - rh < -1 then
                rotateLeft pk pv pl rk rv rl rr

            else if lh - rh > 1 then
                rotateRight pk pv lk lv ll lr pr

            else
                RBNode_elm_builtin (1 + max lh rh) pk pv pl pr


rotateLeft : key -> value -> Node key value -> key -> value -> Node key value -> Node key value -> Node key value
rotateLeft pk pv pl rk rv rl rr =
    case rl of
        RBEmpty_elm_builtin ->
            leaf rk rv (RBNode_elm_builtin (1 + height pl) pk pv pl nil) rr

        RBNode_elm_builtin lh lk lv ll lr ->
            if lh > height rr then
                leaf lk lv (leaf pk pv pl ll) (leaf rk rv lr rr)

            else
                leaf rk rv (leaf pk pv pl rl) rr


rotateRight : key -> value -> key -> value -> Node key value -> Node key value -> Node key value -> Node key value
rotateRight pk pv lk lv ll lr pr =
    case lr of
        RBEmpty_elm_builtin ->
            leaf lk lv ll (RBNode_elm_builtin (1 + height pr) pk pv nil pr)

        RBNode_elm_builtin rh rk rv rl rr ->
            if height ll < rh then
                leaf rk rv (leaf lk lv ll rl) (leaf pk pv rr pr)

            else
                leaf lk lv ll (leaf pk pv lr pr)


remove : Comparator key -> key -> Node key value -> Maybe (Node key value)
remove comparator key node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ k v l r ->
            case comparator key k of
                LT ->
                    Maybe.map (\nextL -> balance k v nextL r) (remove comparator key l)

                GT ->
                    Maybe.map (balance k v l) (remove comparator key r)

                EQ ->
                    if height l < height r then
                        case removeMin r of
                            Nothing ->
                                Just l

                            Just ( minK, minV, nextR ) ->
                                Just (leaf minK minV l nextR)

                    else
                        case removeMax l of
                            Nothing ->
                                Just r

                            Just ( maxK, maxV, nextL ) ->
                                Just (leaf maxK maxV nextL r)


removeMin : Node key value -> Maybe ( key, value, Node key value )
removeMin node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ k v l r ->
            case removeMin l of
                Nothing ->
                    Just ( k, v, r )

                Just ( rk, rv, nextL ) ->
                    Just ( rk, rv, balance k v nextL r )


removeMax : Node key value -> Maybe ( key, value, Node key value )
removeMax node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ k v l r ->
            case removeMax r of
                Nothing ->
                    Just ( k, v, l )

                Just ( rk, rv, nextR ) ->
                    Just ( rk, rv, balance k v l nextR )



-- Q U E R Y


get : Comparator key -> key -> Node key value -> Maybe value
get comparator target node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ key value left right ->
            case comparator target key of
                LT ->
                    get comparator target left

                GT ->
                    get comparator target right

                EQ ->
                    Just value


minimum : Node key value -> Maybe ( key, value )
minimum node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ k v l _ ->
            case minimum l of
                Nothing ->
                    Just ( k, v )

                just ->
                    just


maximum : Node key value -> Maybe ( key, value )
maximum node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ k v _ r ->
            case maximum r of
                Nothing ->
                    Just ( k, v )

                just ->
                    just



-- T R A N S F O R M


map : (key -> a -> b) -> Node key a -> Node key b
map fn node =
    case node of
        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin

        RBNode_elm_builtin h k v l r ->
            RBNode_elm_builtin h k (fn k v) (map fn l) (map fn r)


filter :
    Comparator key
    -> (key -> value -> Bool)
    -> key
    -> value
    -> ( Int, Node key value )
    -> ( Int, Node key value )
filter comparator check key value (( count, node ) as acc) =
    if check key value then
        ( count + 1
        , Tuple.second (insert comparator key value node)
        )

    else
        acc


partition :
    Comparator key
    -> (key -> value -> Bool)
    -> key
    -> value
    -> ( ( Int, Node key value ), ( Int, Node key value ) )
    -> ( ( Int, Node key value ), ( Int, Node key value ) )
partition comparator check key value ( ( leftCount, leftRoot ) as left, ( rightCount, rightRoot ) as right ) =
    if check key value then
        ( ( leftCount + 1
          , Tuple.second (insert comparator key value leftRoot)
          )
        , right
        )

    else
        ( left
        , ( rightCount + 1
          , Tuple.second (insert comparator key value rightRoot)
          )
        )


foldl : (key -> value -> acc -> acc) -> acc -> Node key value -> acc
foldl fn acc node =
    case node of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ k v l r ->
            foldl fn (fn k v (foldl fn acc l)) r


foldr : (key -> value -> acc -> acc) -> acc -> Node key value -> acc
foldr fn acc node =
    case node of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ k v l r ->
            foldr fn (fn k v (foldr fn acc r)) l
