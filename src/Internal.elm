module Internal exposing (AVLDict(..), AVLSet(..), Node(..), Set(..))


type AVLDict key value
    = AVLDict (key -> key -> Order) Int (Node key value)


type AVLSet key
    = AVLSet (key -> key -> Order) Int (Set key)


type Set key
    = Set_elm_builtin (Node key ())


type Node key value
    = RBEmpty_elm_builtin
    | RBNode_elm_builtin Int key value (Node key value) (Node key value)
