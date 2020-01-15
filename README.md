# [AVL](https://en.wikipedia.org/wiki/AVL_tree) based Elm Dict and Set

[![Build Status](https://travis-ci.com/owanturist/elm-avl-dict.svg?branch=master)](https://travis-ci.com/owanturist/elm-avl-dict)

An implementation of easy to understand AVL algorithm.
The basic idea is to keep both sub-trees of a node almost the same height.
Almost means that heights difference must be not more that 1.
As soon as this rule is violated rebalancing happens.
It provides more strictly balance than 
[red-black-tree](https://en.wikipedia.org/wiki/Red%E2%80%93black_tree) implementation.

```bash
elm install owanturist/elm-avl-dict
```

## Example

```elm
import AVL.Dict as Dict exposing (Dict)
import AVL.Set as Set exposing (Set)


type ID
    = ID Int


compareID : ID -> ID -> Order
compareID (ID x) (ID y) =
    compare x y


users : Dict ID User
users =
    Dict.fromListWith compareID
        [ ( ID 0, User (ID 0) "Alice" 28 1.65 )
        , ( ID 1, User (ID 1) "Bob" 19 1.82 )
        , ( ID 2, User (ID 2) "Chuck" 33 1.75 )
        ]


userIDs : Set ID
userIDs =
    Set.fromListWith compareID [ ID 0, ID 1, ID 2 ]


alice : Maybe User
alice =
    Dict.get (ID 0) users


type alias User =
    { id : ID
    , name : String
    , age : Int
    , height : Float
    }

```

## Performance 

Time complexity of basic operations is the same (`O(log n)`) as in
[`Core.Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict)
except constant time `O(1)` for `AVL.Dict.size` operation.
It is achieved by tracking the size of a tree in the single place
so it spends constant additional space as well.
