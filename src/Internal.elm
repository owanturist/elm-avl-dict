module Internal exposing (AVL(..), Node(..))


type AVL key value
    = AVL Int (Node key value)


type Node key value
    = RBEmpty_elm_builtin
    | RBNode_elm_builtin Int key value (Node key value) (Node key value)
