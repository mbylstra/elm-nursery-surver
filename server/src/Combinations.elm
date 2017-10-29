module Combinations exposing (..)

{-| given a list of lists, get each possible list that can be created
that has one element from each list
-}


combinations : List (List a) -> List (List a)
combinations inputLists =
    -- inputList = [1]
    -- outputLists = []
    -- it's becoming an empty list,
    case inputLists of
        [] ->
            []

        firstInputList :: [] ->
            [ firstInputList ]

        firstInputList :: otherInputLists ->
            otherInputLists
                |> List.foldl foldFunction (firstInputList |> List.map List.singleton)


foldFunction : List a -> List (List a) -> List (List a)
foldFunction inputList outputLists =
    inputList
        |> List.map
            (\inputElement ->
                outputLists
                    |> List.map (\outputList -> outputList ++ [ inputElement ])
            )
        |> List.concat
