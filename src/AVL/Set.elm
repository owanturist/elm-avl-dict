module AVL.Set exposing
    ( Set
    , Comparator
    , empty, emptyWith, singleton, singletonWith, fromList, fromListWith
    , toList
    , insert, remove, toggle, clear
    , isEmpty, size, member
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

@docs isEmpty, size, member


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


{-| -}
type alias Set key =
    Internal.AVLSet key



-- C O N S T R U C T I O N


{-| -}
type alias Comparator key =
    key -> key -> Order


{-| -}
empty : Set comparable
empty =
    emptyWith compare


{-| -}
emptyWith : Comparator key -> Set key
emptyWith comparator =
    Internal.nil
        |> Internal.Set_elm_builtin
        |> Internal.AVLSet comparator 0


{-| -}
singleton : comparable -> Set comparable
singleton =
    singletonWith compare


{-| -}
singletonWith : Comparator key -> key -> Set key
singletonWith comparator key =
    Internal.singleton key ()
        |> Internal.Set_elm_builtin
        |> Internal.AVLSet comparator 1


{-| -}
fromList : List comparable -> Set comparable
fromList =
    fromListWith compare


{-| -}
fromListWith : Comparator key -> List key -> Set key
fromListWith comparator list =
    List.foldl
        (Internal.fromList comparator identity (always ()))
        ( 0, Internal.nil )
        list
        |> untuple comparator



-- D E C O N S T R U C T I O N


{-| -}
toList : Set key -> List key
toList set =
    foldr (::) [] set



-- M A N I P U L A T I O N


{-| -}
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


{-| -}
remove : key -> Set key -> Set key
remove key ((Internal.AVLSet comparator count (Internal.Set_elm_builtin root)) as set) =
    case Internal.remove comparator key root of
        Nothing ->
            set

        Just nextRoot ->
            Internal.AVLSet comparator (count - 1) (Internal.Set_elm_builtin nextRoot)


{-| -}
toggle : key -> Set key -> Set key
toggle key set =
    if member key set then
        remove key set

    else
        insert key set


{-| -}
clear : Set key -> Set key
clear (Internal.AVLSet comparator _ _) =
    emptyWith comparator



-- Q U E R Y


{-| -}
isEmpty : Set key -> Bool
isEmpty set =
    size set /= 0


{-| -}
size : Set key -> Int
size (Internal.AVLSet _ count _) =
    count


{-| -}
member : key -> Set key -> Bool
member key (Internal.AVLSet comparator _ (Internal.Set_elm_builtin root)) =
    Internal.get comparator key root /= Nothing



-- T R A N S F O R M


{-| -}
map : (key -> key) -> Set key -> Set key
map fn set =
    foldl (insert << fn) (clear set) set


{-| -}
filter : (key -> Bool) -> Set key -> Set key
filter check (Internal.AVLSet comparator _ (Internal.Set_elm_builtin root)) =
    Internal.foldl
        (Internal.filter comparator (\key _ -> check key))
        ( 0, Internal.nil )
        root
        |> untuple comparator


{-| -}
partition : (key -> Bool) -> Set key -> ( Set key, Set key )
partition check (Internal.AVLSet comparator _ (Internal.Set_elm_builtin root)) =
    Internal.foldl
        (Internal.partition comparator (\key _ -> check key))
        ( ( 0, Internal.nil ), ( 0, Internal.nil ) )
        root
        |> Tuple.mapBoth (untuple comparator) (untuple comparator)


{-| -}
foldl : (key -> acc -> acc) -> acc -> Set key -> acc
foldl fn acc (Internal.AVLSet _ _ (Internal.Set_elm_builtin root)) =
    Internal.foldl (\key _ semiacc -> fn key semiacc) acc root


{-| -}
foldr : (key -> acc -> acc) -> acc -> Set key -> acc
foldr fn acc (Internal.AVLSet _ _ (Internal.Set_elm_builtin root)) =
    Internal.foldr (\key _ semiacc -> fn key semiacc) acc root



-- C O M B I N E


{-| -}
union : Set key -> Set key -> Set key
union left right =
    foldl insert right left


{-| -}
intersect : Set key -> Set key -> Set key
intersect left right =
    filter (\key -> member key right) left


{-| -}
diff : Set key -> Set key -> Set key
diff left right =
    foldl remove left right
