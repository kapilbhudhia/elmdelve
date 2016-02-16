module Fibonacci where

addTopTwo : List Int -> Maybe(List Int)
addTopTwo lst =
  let
    newValue = Maybe.map2 (+) (List.head lst) (Maybe.andThen (List.tail lst) List.head)
  in
    Maybe.map2 (::) newValue (Just lst)

{--}
fibonacci : Int -> Int -> Int -> List Int
fibonacci n a b =
  let fib' goal n a b lst =
    if n >= goal
    then lst
    else fib' goal (n + 1) b (a + b) (a :: lst)
  in
    fib' n 0 a b [] |> List.reverse
--}
