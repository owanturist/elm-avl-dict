module AVL.Dict exposing
    ( Dict
    , Comparator
    , empty, emptyWith, singleton, singletonWith, fromList, fromListWith
    , keys, values, toList
    , insert, remove, update, clear
    , isEmpty, size, member, get, minimum, maximum, keyComparator
    , map, filter, partition, foldl, foldr
    , union, diff, intersect, merge
    )

{-| An AVL Tree based dictionary.

A dictionary mapping unique keys to values.
The keys can be any type.
This includes both custom and comparable types such as Int, Float, Time, Char, String, and tuples or lists of comparable types.

Insert, remove, get and member operations all take `O(log n)` time.
Size takes constant `O(1)` time.


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

@docs isEmpty, size, member, get, minimum, maximum, keyComparator


# Transform

@docs map, filter, partition, foldl, foldr


# Combine

@docs union, diff, intersect, merge

-}

import Internal



-- U T I L S


untuple : Comparator key -> ( Int, Internal.Node key value ) -> Dict key value
untuple comparator ( count, root ) =
    Internal.AVLDict comparator count root


{-| A dictionary of keys and values.
So a `Dict String User` is a dictionary
that lets you look up a `String` (such as user names)
and find the associated `User`.

    import AVL.Dict as Dict exposing (Dict)

    users : Dict String User
    users =
        Dict.fromList
            [ ( "Alice", User "Alice" 28 1.65 )
            , ( "Bob", User "Bob" 19 1.82 )
            , ( "Chuck", User "Chuck" 33 1.75 )
            ]

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

-}
type alias Dict key value =
    Internal.AVLDict key value



-- C O N S T R U C T I O N


{-| A comparator is a function which compares two keys.
So a `Dict ID User` is a dictionary
that lets you look up a `ID` (such as user ids)
and find the associated `User`.

    import AVL.Dict as Dict exposing (Comparator, Dict)

    type ID
        = ID Int

    compareID : Comparator ID
    compareID (ID x) (ID y) =
        compare x y

    users : Dict ID User
    users =
        Dict.fromListWith compareID
            [ ( ID 0, User (ID 0) "Alice" 28 1.65 )
            , ( ID 1, User (ID 1) "Bob" 19 1.82 )
            , ( ID 2, User (ID 2) "Chuck" 33 1.75 )
            ]

    alice : Maybe User
    alice =
        Dict.get (ID 0) users

    type alias User =
        { id : ID
        , name : String
        , age : Int
        , height : Float
        }

-}
type alias Comparator key =
    key -> key -> Order


{-| Create an empty dictionary with custom keys.
-}
emptyWith : Comparator key -> Dict key value
emptyWith comparator =
    Internal.AVLDict comparator 0 Internal.nil


{-| Create an empty dictionary with comparable keys.
-}
empty : Dict comparable value
empty =
    emptyWith compare


{-| Create a dictionary with one custom key-value pair.
-}
singletonWith : Comparator key -> key -> value -> Dict key value
singletonWith comparator key value =
    Internal.AVLDict comparator 1 (Internal.singleton key value)


{-| Create a dictionary with one comparable key-value pair.
-}
singleton : comparable -> value -> Dict comparable value
singleton =
    singletonWith compare


{-| Convert an association list into a dictionary with custom keys.
-}
fromListWith : Comparator key -> List ( key, value ) -> Dict key value
fromListWith comparator list =
    List.foldl
        (Internal.fromList comparator Tuple.first Tuple.second)
        ( 0, Internal.nil )
        list
        |> untuple comparator


{-| Convert an association list into a dictionary with comparable keys.
-}
fromList : List ( comparable, value ) -> Dict comparable value
fromList =
    fromListWith compare



-- D E C O N S T R U C T I O N


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    import AVL.Dict as Dict

    Dict.fromList [ ( 1, "Bob" ), ( 0, "Alice" ) ]
        |> Dict.keys
    --> [ 0, 1 ]

-}
keys : Dict key value -> List key
keys dict =
    foldr (\key _ acc -> key :: acc) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    import AVL.Dict as Dict

    Dict.fromList [ ( 1, "Bob" ), ( 0, "Alice" ) ]
        |> Dict.values
    --> [ "Alice", "Bob" ]

-}
values : Dict key value -> List value
values dict =
    foldr (\_ value acc -> value :: acc) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.

    import AVL.Dict as Dict

    Dict.fromList [ ( 1, "Bob" ), ( 0, "Alice" ) ]
        |> Dict.toList
    --> [ ( 0, "Alice" ), ( 1, "Bob" ) ]

-}
toList : Dict key value -> List ( key, value )
toList dict =
    foldr (\key value acc -> ( key, value ) :: acc) [] dict



-- M A N I P U L A T I O N


{-| Insert a key-value pair into a dictionary.
Replaces value when there is a collision.
-}
insert : key -> value -> Dict key value -> Dict key value
insert key value (Internal.AVLDict comparator count root) =
    let
        ( added, nextRoot ) =
            Internal.insert comparator key value root

        nextCount =
            if added then
                count + 1

            else
                count
    in
    Internal.AVLDict comparator nextCount nextRoot


{-| Remove a key-value pair from a dictionary.
If the key is not found, no changes are made.
-}
remove : key -> Dict key value -> Dict key value
remove key ((Internal.AVLDict comparator count root) as dict) =
    case Internal.remove comparator key root of
        Nothing ->
            dict

        Just nextRoot ->
            Internal.AVLDict comparator (count - 1) nextRoot


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : key -> (Maybe value -> Maybe value) -> Dict key value -> Dict key value
update key transform dict =
    case get key dict of
        Nothing ->
            case transform Nothing of
                Nothing ->
                    dict

                Just value ->
                    insert key value dict

        just ->
            case transform just of
                Nothing ->
                    remove key dict

                Just value ->
                    insert key value dict


{-| Remove all entries from a dictionary.
Useful when you need to create new empty dictionary using same comparator.
-}
clear : Dict key value -> Dict key value
clear (Internal.AVLDict comparator _ _) =
    emptyWith comparator



-- Q U E R Y


{-| Determine if a dictionary is empty.
-}
isEmpty : Dict key value -> Bool
isEmpty dict =
    size dict == 0


{-| Determine the number of key-value pairs in the dictionary.
It takes constant time to determine the size.
-}
size : Dict key value -> Int
size (Internal.AVLDict _ count _) =
    count


{-| Determine if a key is in a dictionary.
-}
member : key -> Dict key value -> Bool
member key dict =
    get key dict /= Nothing


{-| Get the value associated with a key. If the key is not found, return Nothing.
This is useful when you are not sure if a key will be in the dictionary.

    import AVL.Dict as Dict exposing (Dict)

    type Animal
        = Cat
        | Mouse
        | Dog

    animals : Dict String Animal
    animals =
        Dict.fromList [ ( "Tom", Cat ), ( "Jerry", Mouse ) ]

    Dict.get "Tom" animals --> Just Cat
    Dict.get "Jerry" animals --> Just Mouse
    Dict.get "Spike" animals --> Nothing

-}
get : key -> Dict key value -> Maybe value
get key (Internal.AVLDict comparator _ root) =
    Internal.get comparator key root


{-| Get the key-value pair associated with minimum key. If Dict is empty return Nothing.

    import AVL.Dict as Dict

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

    Dict.fromList
        [ ( "Bob", { name = "Bob", age = 19, height = 1.82 } )
        , ( "Alice", { name = "Alice", age = 28, height = 1.65 } )
        , ( "Chuck", { name = "Chuck", age = 33, height = 1.75 } )
        ]
        |> Dict.minimum
    --> Just ( "Alice", User "Alice" 28 1.65 )

-}
minimum : Dict key value -> Maybe ( key, value )
minimum (Internal.AVLDict _ _ root) =
    Internal.minimum root


{-| Get the key-value pair associated with maximum key. If Dict is empty return Nothing.

    import AVL.Dict as Dict

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

    Dict.fromList
        [ ( "Bob", { name = "Bob", age = 19, height = 1.82 } )
        , ( "Alice", { name = "Alice", age = 28, height = 1.65 } )
        , ( "Chuck", { name = "Chuck", age = 33, height = 1.75 } )
        ]
        |> Dict.maximum
    --> Just ( "Chuck", User "Chuck" 33 1.75 )

-}
maximum : Dict key value -> Maybe ( key, value )
maximum (Internal.AVLDict _ _ root) =
    Internal.maximum root


{-| Get the [`Comparator`](#Comparator) for its keys.

    import AVL.Dict as Dict exposing (Dict)

    score : Dict String Int
    score =
        Dict.fromList
            [ ( "Bob", 1.82 )
            , ( "Alice", 1.65 )
            , ( "Chuck", 33 1.75 )
            ]

    (score |> Dict.keyComparator) "A" "Z"
    --> LT

-}
keyComparator : Dict key value -> Comparator key
keyComparator dict =
    dict |> Internal.dictKeyComparator



-- T R A N S F O R M


{-| Apply a function to all values in a dictionary.
-}
map : (key -> a -> b) -> Dict key a -> Dict key b
map fn (Internal.AVLDict comparator count root) =
    Internal.AVLDict comparator count (Internal.map fn root)


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (key -> value -> Bool) -> Dict key value -> Dict key value
filter check (Internal.AVLDict comparator _ root) =
    Internal.foldl
        (Internal.filter comparator check)
        ( 0, Internal.nil )
        root
        |> untuple comparator


{-| Partition a dictionary according to some test.
The first dictionary contains all key-value pairs which passed the test,
and the second contains the pairs that did not.
-}
partition : (key -> value -> Bool) -> Dict key value -> ( Dict key value, Dict key value )
partition check (Internal.AVLDict comparator _ root) =
    Internal.foldl
        (Internal.partition comparator check)
        ( ( 0, Internal.nil ), ( 0, Internal.nil ) )
        root
        |> Tuple.mapBoth (untuple comparator) (untuple comparator)


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.
-}
foldl : (key -> value -> acc -> acc) -> acc -> Dict key value -> acc
foldl fn acc (Internal.AVLDict _ _ root) =
    Internal.foldl fn acc root


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.
-}
foldr : (key -> value -> acc -> acc) -> acc -> Dict key value -> acc
foldr fn acc (Internal.AVLDict _ _ root) =
    Internal.foldr fn acc root



-- C O M B I N E


{-| Combine two dictionaries.
If there is a collision, preference is given to the left dictionary.
-}
union : Dict key value -> Dict key value -> Dict key value
union left right =
    foldl insert right left


{-| Keep a key-value pair when its key appears in the right dictionary.
Preference is given to values in the left dictionary.
-}
intersect : Dict key value -> Dict key value -> Dict key value
intersect left right =
    filter (\key _ -> member key right) left


{-| Keep a key-value pair when its key does not appear in the right dictionary.
-}
diff : Dict key value -> Dict key value -> Dict key value
diff left right =
    foldl (\key _ acc -> remove key acc) left right


{-| The most general way of combining two dictionaries.
You provide three accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever you want.

-}
merge :
    (key -> left -> acc -> acc)
    -> (key -> left -> right -> acc -> acc)
    -> (key -> right -> acc -> acc)
    -> Dict key left
    -> Dict key right
    -> acc
    -> acc
merge onLeft onBoth onRight left (Internal.AVLDict comparator _ right) acc =
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

        ( leftovers, accAll ) =
            Internal.foldl stepAll ( toList left, acc ) right
    in
    List.foldl (\( lk, lv ) semiacc -> onLeft lk lv semiacc) accAll leftovers
