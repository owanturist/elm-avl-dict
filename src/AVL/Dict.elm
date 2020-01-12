module AVL.Dict exposing
    ( Dict
    , Comparator
    , empty, emptyWith, singleton, singletonWith, fromList, fromListWith
    , keys, values, toList
    , insert, remove, update, clear
    , isEmpty, size, member, get
    , map, filter, partition, foldl, foldr
    , union, diff, intersect, merge
    )

{-| An AVL Tree based dictionary.


# Dictionary

@docs Dict


# Construction

@docs Comparator
@docs empty, emptyWith, singleton, singletonWith, fromList, fromListWith


# Deconstruction

@docs keys, values, toList


# Manipulation

@docs insert, remove, update, clear


# Query

@docs isEmpty, size, member, get


# Transform

@docs map, filter, partition, foldl, foldr


# Combine

@docs union, diff, intersect, merge

-}

import Internal exposing (Node(..))


{-| -}
type alias Dict key value =
    Internal.AVL key value


{-| -}
type alias Comparator key =
    key -> key -> Order



-- U T I L S


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


untuple : Comparator key -> ( Int, Node key value ) -> Dict key value
untuple comparator ( count, root ) =
    Internal.AVL comparator count root



-- C O N S T R U C T I O N


{-| Create an empty dictionary with custom key comparator.
-}
emptyWith : Comparator key -> Dict key value
emptyWith comparator =
    Internal.AVL comparator 0 nil


{-| Create an empty dictionary.
-}
empty : Dict comparable value
empty =
    emptyWith compare


{-| Create a dictionary with one key-value pair with custom key comparator.
-}
singletonWith : Comparator key -> key -> value -> Dict key value
singletonWith comparator key value =
    Internal.AVL comparator 1 (RBNode_elm_builtin 1 key value nil nil)


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable -> value -> Dict comparable value
singleton =
    singletonWith compare


{-| Convert an association list into a dictionary with custom key comparator.
-}
fromListWith : Comparator key -> List ( key, value ) -> Dict key value
fromListWith comparator list =
    untuple comparator (List.foldl (fromListHelper comparator) ( 0, nil ) list)


fromListHelper : Comparator key -> ( key, value ) -> ( Int, Node key value ) -> ( Int, Node key value )
fromListHelper comparator ( key, value ) ( count, root ) =
    let
        ( added, nextRoot ) =
            insertHelp comparator key value root
    in
    if added then
        ( count + 1, nextRoot )

    else
        ( count, nextRoot )


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, value ) -> Dict comparable value
fromList =
    fromListWith compare



-- D E C O N S T R U C T I O N


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 1, "Bob" ), ( 0, "Alice" ) ]) == [ 0, 1 ]

-}
keys : Dict key value -> List key
keys avl =
    foldr keysStep [] avl


keysStep : key -> value -> List key -> List key
keysStep key _ acc =
    key :: acc


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 1, "Bob" ), ( 0, "Alice" ) ]) == [ "Alice", "Bob" ]

-}
values : Dict key value -> List value
values avl =
    foldr valuesStep [] avl


valuesStep : key -> value -> List value -> List value
valuesStep _ value acc =
    value :: acc


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.

    toList (fromList [ ( 1, "Bob" ), ( 0, "Alice" ) ]) == [ ( 0, "Alice" ), ( 1, "Bob" ) ]

-}
toList : Dict key value -> List ( key, value )
toList avl =
    foldr toListStep [] avl


toListStep : key -> value -> List ( key, value ) -> List ( key, value )
toListStep key value acc =
    ( key, value ) :: acc



-- M A N I P U L A T I O N


{-| Insert a key-value pair into a dictionary.
Replaces value when there is a collision.
-}
insert : key -> value -> Dict key value -> Dict key value
insert key value (Internal.AVL comparator count root) =
    let
        ( added, nextRoot ) =
            insertHelp comparator key value root
    in
    if added then
        Internal.AVL comparator (count + 1) nextRoot

    else
        Internal.AVL comparator count nextRoot


insertHelp : Comparator key -> key -> value -> Node key value -> ( Bool, Node key value )
insertHelp comparator key value node =
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
                            insertHelp comparator key value l
                    in
                    ( added, balance k v nextL r )

                GT ->
                    let
                        ( added, nextR ) =
                            insertHelp comparator key value r
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


{-| Remove a key-value pair from a dictionary.
If the key is not found, no changes are made.
-}
remove : key -> Dict key value -> Dict key value
remove key ((Internal.AVL comparator count root) as avl) =
    case removeHelp comparator key root of
        Nothing ->
            avl

        Just nextRoot ->
            Internal.AVL comparator (count - 1) nextRoot


removeHelp : Comparator key -> key -> Node key value -> Maybe (Node key value)
removeHelp comparator key node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ k v l r ->
            case comparator key k of
                LT ->
                    Maybe.map (\nextL -> balance k v nextL r) (removeHelp comparator key l)

                GT ->
                    Maybe.map (balance k v l) (removeHelp comparator key r)

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


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : key -> (Maybe value -> Maybe value) -> Dict key value -> Dict key value
update key transform avl =
    case get key avl of
        Nothing ->
            case transform Nothing of
                Nothing ->
                    avl

                Just value ->
                    insert key value avl

        just ->
            case transform just of
                Nothing ->
                    remove key avl

                Just value ->
                    insert key value avl


{-| Remove all entries from a dictionary.
Useful when you need to create new empty dictionary using same comparator.
-}
clear : Dict key value -> Dict key value
clear (Internal.AVL comparator _ _) =
    emptyWith comparator



-- Q U E R Y


{-| Determine if a dictionary is empty.
-}
isEmpty : Dict key value -> Bool
isEmpty avl =
    size avl == 0


{-| Determine the number of key-value pairs in the dictionary.
It takes constant time to request the size.
-}
size : Dict key value -> Int
size (Internal.AVL _ count _) =
    count


{-| Determine if a key is in a dictionary.
-}
member : key -> Dict key value -> Bool
member key avl =
    get key avl /= Nothing


{-| Get the value associated with a key. If the key is not found, return Nothing.
This is useful when you are not sure if a key will be in the dictionary.

    animals =
        fromList [ ( "Tom", Cat ), ( "Jerry", Mouse ) ]

    get "Tom" animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : key -> Dict key value -> Maybe value
get key (Internal.AVL comparator _ root) =
    getHelper comparator key root


getHelper : Comparator key -> key -> Node key value -> Maybe value
getHelper comparator target node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ key value left right ->
            case comparator target key of
                LT ->
                    getHelper comparator target left

                GT ->
                    getHelper comparator target right

                EQ ->
                    Just value



-- T R A N S F O R M


{-| Apply a function to all values in a dictionary.
-}
map : (key -> a -> b) -> Dict key a -> Dict key b
map fn (Internal.AVL comparator count root) =
    Internal.AVL comparator count (mapHelp fn root)


mapHelp : (key -> a -> b) -> Node key a -> Node key b
mapHelp fn node =
    case node of
        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin

        RBNode_elm_builtin h k v l r ->
            RBNode_elm_builtin h k (fn k v) (mapHelp fn l) (mapHelp fn r)


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (key -> value -> Bool) -> Dict key value -> Dict key value
filter check (Internal.AVL comparator _ root) =
    let
        step : key -> value -> ( Int, Node key value ) -> ( Int, Node key value )
        step key value (( count, node ) as acc) =
            if check key value then
                ( count + 1
                , Tuple.second (insertHelp comparator key value node)
                )

            else
                acc
    in
    untuple comparator (foldlHelp step ( 0, nil ) root)


{-| Partition a dictionary according to some test.
The first dictionary contains all key-value pairs which passed the test,
and the second contains the pairs that did not.
-}
partition : (key -> value -> Bool) -> Dict key value -> ( Dict key value, Dict key value )
partition check (Internal.AVL comparator _ root) =
    let
        step : key -> value -> ( ( Int, Node key value ), ( Int, Node key value ) ) -> ( ( Int, Node key value ), ( Int, Node key value ) )
        step key value ( ( leftCount, leftRoot ) as left, ( rightCount, rightRoot ) as right ) =
            if check key value then
                ( ( leftCount + 1
                  , Tuple.second (insertHelp comparator key value leftRoot)
                  )
                , right
                )

            else
                ( left
                , ( rightCount + 1
                  , Tuple.second (insertHelp comparator key value rightRoot)
                  )
                )
    in
    Tuple.mapBoth
        (untuple comparator)
        (untuple comparator)
        (foldlHelp step ( ( 0, nil ), ( 0, nil ) ) root)


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.
-}
foldl : (key -> value -> acc -> acc) -> acc -> Dict key value -> acc
foldl fn acc (Internal.AVL _ _ root) =
    foldlHelp fn acc root


foldlHelp : (key -> value -> acc -> acc) -> acc -> Node key value -> acc
foldlHelp fn acc node =
    case node of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ k v l r ->
            foldlHelp fn (fn k v (foldlHelp fn acc l)) r


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.
-}
foldr : (key -> value -> acc -> acc) -> acc -> Dict key value -> acc
foldr fn acc (Internal.AVL _ _ root) =
    foldrHelp fn acc root


foldrHelp : (key -> value -> acc -> acc) -> acc -> Node key value -> acc
foldrHelp fn acc node =
    case node of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ k v l r ->
            foldrHelp fn (fn k v (foldrHelp fn acc r)) l



-- C O M B I N E


{-| Combine two dictionaries.
If there is a collision, preference is given to the left dictionary.
It uses comparator of the right dictionary.
-}
union : Dict key value -> Dict key value -> Dict key value
union left right =
    foldl insert right left


{-| -}
intersect : Dict key value -> Dict key value -> Dict key value
intersect left right =
    let
        step : key -> value -> Bool
        step key _ =
            member key right
    in
    filter step left


{-| -}
diff : Dict key value -> Dict key value -> Dict key value
diff left right =
    let
        step : key -> value -> Dict key value -> Dict key value
        step key _ acc =
            remove key acc
    in
    foldl step left right


{-| -}
merge :
    (key -> left -> acc -> acc)
    -> (key -> left -> right -> acc -> acc)
    -> (key -> right -> acc -> acc)
    -> Dict key left
    -> Dict key right
    -> acc
    -> acc
merge onLeft onBoth onRight left (Internal.AVL comparator _ right) acc =
    let
        stepAll : key -> right -> ( List ( key, left ), acc ) -> ( List ( key, left ), acc )
        stepAll rk rv ( list, semiacc ) =
            case list of
                [] ->
                    ( []
                    , onRight rk rv semiacc
                    )

                ( lk, lv ) :: rest ->
                    case comparator lk rk of
                        LT ->
                            stepAll rk rv ( rest, onLeft lk lv semiacc )

                        GT ->
                            ( list
                            , onRight rk rv semiacc
                            )

                        EQ ->
                            ( rest
                            , onBoth lk lv rv semiacc
                            )

        stepOverLeft : ( key, left ) -> acc -> acc
        stepOverLeft ( lk, lv ) semiacc =
            onLeft lk lv semiacc

        ( leftovers, accAll ) =
            foldlHelp stepAll ( toList left, acc ) right
    in
    List.foldl stepOverLeft accAll leftovers
