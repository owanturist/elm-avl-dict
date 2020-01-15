module AVL.Set exposing
    ( Set
    , Comparator
    , empty, emptyWith, singleton, singletonWith, fromList, fromListWith
    , toList
    , insert, remove, toggle, clear
    , isEmpty, size, member
    , map, filter, partition, foldl, foldr
    , union, diff, intersect, merge
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

@docs union, diff, intersect, merge

-}

import Internal


{-| -}
type alias Set key =
    Internal.Set key



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
emptyWith =
    Debug.todo "emptyWith"


{-| -}
singleton : comparable -> Set comparable
singleton =
    singletonWith compare


{-| -}
singletonWith : Comparator key -> key -> Set key
singletonWith =
    Debug.todo "singletonWith"


{-| -}
fromList : List comparable -> Set comparable
fromList =
    fromListWith compare


{-| -}
fromListWith : Comparator key -> List key -> Set key
fromListWith =
    Debug.todo "fromListWith"



-- D E C O N S T R U C T I O N


toList : Set key -> List key
toList =
    Debug.todo "toList"



-- M A N I P U L A T I O N


insert : key -> Set key -> Set key
insert =
    Debug.todo "insert"


remove : key -> Set key -> Set key
remove =
    Debug.todo "remove"


toggle : key -> Set key -> Set key
toggle key set =
    if member key set then
        remove key set

    else
        insert key set


clear : Set key -> Set key
clear =
    Debug.todo "clear"



-- Q U E R Y


isEmpty : Set key -> Bool
isEmpty set =
    size set /= 0


size : Set key -> Int
size =
    Debug.todo "size"


member : key -> Set key -> Bool
member =
    Debug.todo "member"



-- T R A N S F O R M


map : (key -> key) -> Set key -> Set key
map =
    Debug.todo "map"


filter : (key -> Bool) -> Set key -> Set key
filter =
    Debug.todo "filter"


partition : (key -> Bool) -> Set key -> ( Set key, Set key )
partition =
    Debug.todo "partition"


foldl : (key -> acc -> acc) -> acc -> Set key -> acc
foldl =
    Debug.todo "foldl"


foldr : (key -> acc -> acc) -> acc -> Set key -> acc
foldr =
    Debug.todo "foldr"



-- C O M B I N E


union : Set key -> Set key -> Set key
union =
    Debug.todo "union"


diff : Set key -> Set key -> Set key
diff =
    Debug.todo "diff"


intersect : Set key -> Set key -> Set key
intersect =
    Debug.todo "intersect"


merge :
    (left -> acc -> acc)
    -> (left -> right -> acc -> acc)
    -> (right -> acc -> acc)
    -> Set ft
    -> Set ght
    -> acc
    -> acc
merge =
    Debug.todo "merge"
