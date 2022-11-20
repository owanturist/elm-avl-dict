module Main exposing (main)

import AVL.Dict as Dict exposing (Dict)
import AVL.Set as Set exposing (Set)
import Browser
import Html exposing (text)



-- ID


type ID
    = ID Int


compareID : ID -> ID -> Order
compareID (ID left) (ID right) =
    compare left right



-- U S E R


type alias User =
    { id : ID
    , name : String
    , age : Int
    , height : Float
    }



-- M O D E L


type alias Model =
    { dict : Dict ID User
    , set : Set ID
    }


init : Model
init =
    { dict =
        Dict.fromListWith compareID
            [ ( ID 0, { id = ID 0, name = "Alice", age = 28, height = 1.65 } )
            , ( ID 1, { id = ID 1, name = "Bob", age = 19, height = 1.82 } )
            , ( ID 2, { id = ID 2, name = "Chuck", age = 33, height = 1.75 } )
            ]
    , set = Set.fromListWith compareID [ ID 0, ID 1, ID 2 ]
    }



-- V I E W


{-| It helps to check that Elm Debugger shows Dict correctly.

    elm make --debug src/Main.elm

-}
main : Program () Model msg
main =
    Browser.sandbox
        { init = init
        , update = \_ model -> model
        , view = always (text "")
        }
