module ListExtra exposing ((!!))


(!!) : List a -> Int -> Maybe a
(!!) list n =
    List.drop n list |> List.head
