module AVL.Set exposing
    ( Set
    , Comparator
    , empty, emptyWith, singleton, singletonWith, fromList, fromListWith
    , toList
    , insert, remove, toggle, clear
    , isEmpty, size, member, minimum, maximum, keyComparator
    , map, filter, partition, foldl, foldr
    , union, diff, intersect
    )

{-| An AVL Tree based set.

A set of unique values. The keys can be any type.
This includes both custom and comparable types such as Int, Float, Time, Char, String, and tuples or lists of comparable types.

Insert, remove, get and member operations all take `O(log n)` time.
Size takes constant `O(1)` time.


# Set

@docs Set


# Construction

@docs Comparator
@docs empty, emptyWith, singleton, singletonWith, fromList, fromListWith


# Deconstruction

@docs toList


# Manipulation

@docs insert, remove, toggle, clear


# Query

@docs isEmpty, size, member, minimum, maximum, keyComparator


# Transform

@docs map, filter, partition, foldl, foldr


# Combine

@docs union, diff, intersect

-}

import Internal



-- U T I L S


untuple : Comparator key -> ( Int, Internal.Node key () ) -> Set key
untuple comparator ( count, root ) =
    Internal.AVLSet comparator count (Internal.Set_elm_builtin root)


{-| Represents a set of unique values.
So (Set Int) is a set of integers and (Set ID) is a set of custom ID values.
-}
type alias Set key =
    Internal.AVLSet key



-- C O N S T R U C T I O N


{-| A comparator is a function which compares two keys.

    import AVL.Set as Set exposing (Comparator, Set)

    type ID
        = ID Int

    compareID : Comparator ID
    compareID (ID x) (ID y) =
        compare x y

    ids : Set ID
    ids =
        Set.fromListWith compareID [ ID 0, ID 1, ID 2 ]

    probs : List Bool
    probs =
        [ Set.member (ID 0)
        , Set.member (ID 3)
        ]

-}
type alias Comparator key =
    key -> key -> Order


{-| Create an empty set with custom keys.
-}
emptyWith : Comparator key -> Set key
emptyWith comparator =
    Internal.nil
        |> Internal.Set_elm_builtin
        |> Internal.AVLSet comparator 0


{-| Create an empty set with comparable keys.
-}
empty : Set comparable
empty =
    emptyWith compare


{-| Create a set with one custom key.
-}
singletonWith : Comparator key -> key -> Set key
singletonWith comparator key =
    Internal.singleton key ()
        |> Internal.Set_elm_builtin
        |> Internal.AVLSet comparator 1


{-| Create a set with one comparable key.
-}
singleton : comparable -> Set comparable
singleton =
    singletonWith compare


{-| Convert an list into a set with custom keys.
-}
fromListWith : Comparator key -> List key -> Set key
fromListWith comparator list =
    List.foldl
        (Internal.fromList comparator identity (always ()))
        ( 0, Internal.nil )
        list
        |> untuple comparator


{-| Convert an list into a set with comparable keys.
-}
fromList : List comparable -> Set comparable
fromList =
    fromListWith compare



-- D E C O N S T R U C T I O N


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set key -> List key
toList set =
    foldr (::) [] set



-- M A N I P U L A T I O N


{-| Insert a key into a set.
-}
insert : key -> Set key -> Set key
insert key (Internal.AVLSet comparator count (Internal.Set_elm_builtin root)) =
    let
        ( added, nextRoot ) =
            Internal.insert comparator key () root

        nextCount =
            if added then
                count + 1

            else
                count
    in
    Internal.AVLSet comparator nextCount (Internal.Set_elm_builtin nextRoot)


{-| Remove a key from a set. If the key is not found, no changes are made.
-}
remove : key -> Set key -> Set key
remove key ((Internal.AVLSet comparator count (Internal.Set_elm_builtin root)) as set) =
    case Internal.remove comparator key root of
        Nothing ->
            set

        Just nextRoot ->
            Internal.AVLSet comparator (count - 1) (Internal.Set_elm_builtin nextRoot)


{-| Toggle a specific key.
-}
toggle : key -> Set key -> Set key
toggle key set =
    if member key set then
        remove key set

    else
        insert key set


{-| Remove all entries from a set.
Useful when you need to create new empty set using same comparator.
-}
clear : Set key -> Set key
clear (Internal.AVLSet comparator _ _) =
    emptyWith comparator



-- Q U E R Y


{-| Determine if a set is empty.
-}
isEmpty : Set key -> Bool
isEmpty set =
    size set /= 0


{-| Determine the number of elements in a set.
It takes constant time to determine the size.
-}
size : Set key -> Int
size (Internal.AVLSet _ count _) =
    count


{-| Determine if a value is in a set.
-}
member : key -> Set key -> Bool
member key (Internal.AVLSet comparator _ (Internal.Set_elm_builtin root)) =
    Internal.get comparator key root /= Nothing


{-| Get the key-value pair associated with minimum key. If Set is empty return Nothing.

    import AVL.Set as Set

    Set.fromList [ 0, 1, -1, 2, -2 ]
        |> Set.minimum
        --> Just -2

-}
minimum : Set key -> Maybe key
minimum (Internal.AVLSet _ _ (Internal.Set_elm_builtin root)) =
    Maybe.map Tuple.first (Internal.minimum root)


{-| Get the key-value pair associated with maximum key. If Set is empty return Nothing.

    import AVL.Set as Set

    Set.fromList [ 0, 1, -1, 2, -2 ]
        |> Set.maximum
    --> Just 2

-}
maximum : Set key -> Maybe key
maximum (Internal.AVLSet _ _ (Internal.Set_elm_builtin root)) =
    Maybe.map Tuple.first (Internal.maximum root)


{-| Get the [`Comparator`](#Comparator) for its keys.

    import AVL.Set as Set exposing (Set)

    type ID
        = ID Int

    compareID : ID -> ID -> Order
    compareID (ID x) (ID y) =
        compare x y

    idTable : Set ID
    idTable =
        Set.fromListWith compareID
            [ ID 0, ID 1, ID -1, ID 2, ID -2 ]

    Set.keyComparator idTable (ID 10) (ID 8)
    --> GT

-}
keyComparator : Set key -> Comparator key
keyComparator (Internal.AVLSet comparator _ _) =
    comparator



-- T R A N S F O R M


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (key -> key) -> Set key -> Set key
map fn set =
    foldl (insert << fn) (clear set) set


{-| Only keep elements that pass the given test.

    import AVL.Set as Set

    numbers : Set Int
    numbers =
        Set.fromList [ -2, -1, 0, 1, 2 ]

    positives : Set Int
    positives =
        Set.filter (\x -> x > 0) numbers

    Set.toList positives --> [ 1, 2 ]

-}
filter : (key -> Bool) -> Set key -> Set key
filter check (Internal.AVLSet comparator _ (Internal.Set_elm_builtin root)) =
    Internal.foldl
        (Internal.filter comparator (\key _ -> check key))
        ( 0, Internal.nil )
        root
        |> untuple comparator


{-| Create two new sets.
The first contains all the elements that passed the given test,
and the second contains all the elements that did not.
-}
partition : (key -> Bool) -> Set key -> ( Set key, Set key )
partition check (Internal.AVLSet comparator _ (Internal.Set_elm_builtin root)) =
    Internal.foldl
        (Internal.partition comparator (\key _ -> check key))
        ( ( 0, Internal.nil ), ( 0, Internal.nil ) )
        root
        |> Tuple.mapBoth (untuple comparator) (untuple comparator)


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (key -> acc -> acc) -> acc -> Set key -> acc
foldl fn acc (Internal.AVLSet _ _ (Internal.Set_elm_builtin root)) =
    Internal.foldl (\key _ semiacc -> fn key semiacc) acc root


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (key -> acc -> acc) -> acc -> Set key -> acc
foldr fn acc (Internal.AVLSet _ _ (Internal.Set_elm_builtin root)) =
    Internal.foldr (\key _ semiacc -> fn key semiacc) acc root



-- C O M B I N E


{-| Combine two sets.
-}
union : Set key -> Set key -> Set key
union left right =
    foldl insert right left


{-| Keep a keys when them appear in the right set.
-}
intersect : Set key -> Set key -> Set key
intersect left right =
    filter (\key -> member key right) left


{-| Keep a key when them do not appear in the right set.
-}
diff : Set key -> Set key -> Set key
diff left right =
    foldl remove left right
