module Fibonacci exposing (..)


fibonacci : Int -> Int -> Int -> List Int
fibonacci n a b =
    let
        fib_ goal n a b lst =
            if n >= goal then
                lst
            else
                fib_ goal (n + 1) b (a + b) (a :: lst)
    in
        fib_ n 0 a b [] |> List.reverse
