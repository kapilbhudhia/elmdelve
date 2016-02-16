module ListExtra where

(!!) list n =
  List.drop n list |> List.head
