module Impfix.Helpers exposing (..)

import List exposing (any, concat, map, sortWith)
import Regex exposing (HowMany(..), regex, replace)
import String exposing (dropLeft, dropRight, indices, join, left, length, repeat, right, split, toLower, toUpper, trim)

among xs y =
    List.member y xs

bracketed txt =
    left 1 txt == "(" && right 1 txt == ")"

bracket txt =
    "(" ++ txt ++ ")"

bracketIfSpaced txt =
    case indices " " txt of
        []->
            txt
        x::xs->
            bracket txt

capitalize txt =
    (toUpper <| left 1 txt) ++ (dropLeft 1 txt)

civilize txt =
    let
        to_ = replace All (regex "[(){}:,]") (\_ -> "_")
        deleteSpace = replace All (regex " ") (\_ -> "")
    in
        to_ <| deleteSpace txt

clean: List (Maybe a) -> List a
clean xs =
  --clean [Just a, Nothing, Just b] == [a, b]
    let
        dropJust a =
            case a of
                Nothing->
                    []
                Just b->
                    [b]
    in
        concat <| map dropJust xs

components: String -> List String 
components txt = --helper for Destructuring.deunion
    componentsHelp (indices " " txt) 0 txt

componentsHelp idxs start txt =
    case idxs of
        []->
            [dropLeft start txt]
        x::xs->
            case outsideBrackets x txt && outsideCurly x txt of
                True->
                    let
                        component = dropLeft start <| left x txt
                        newStart = x + 1
                    in
                        component :: componentsHelp xs newStart txt
                False->
                    componentsHelp xs start txt

debracket txt =
    let
        trimmed = trim txt
    in
        if left 1 trimmed == "(" && right 1 trimmed == ")"
            then
                trim <| dropLeft 1 <| dropRight 1 trimmed
            else
                trimmed

decomment txt = 
    let
        singleComment = regex "--.*"
        multiComment = regex "\\{\\-(.|\\n)*?\\-\\}"      
    in
        remove singleComment <| remove multiComment txt

dropWord word txt =
    trim <| dropLeft (length word + 1) txt

inCurly txt = inThese "{" "}" txt

inBrackets txt = inThese "(" ")" txt

inThese a b txt =
    let
        trimmed = trim txt
    in
        if left 1 trimmed == a && right 1 trimmed == b
            then Just (dropLeft 1 <| dropRight 1 trimmed)
            else Nothing

isPrefix: String -> List String -> Bool
isPrefix a bs =
    let
        n = String.length a
    in
        case bs of
            []->
                False
            x::xs->
                case a == left n x of
                    True->
                        True
                    False->
                        isPrefix a xs

occurs a txt =
    List.length <| indices a txt

outside: String -> String-> Int -> String -> Bool
outside a b idx txt = 
    let
        chunk = left idx txt
    in
        occurs a chunk == occurs b chunk

outsideBrackets txt = outside "(" ")" txt

outsideCurly txt = outside "{" "}" txt

quote txt =
    "\""++txt++"\""

remove regex txt = replace All regex (\a->"") txt

removeColons txt = remove (regex "\\.") txt

removeStringLiterals txt = remove stringsQuoted <| remove stringsTripleQuoted txt

sortCaseInsensitive xs =
    let
        comp a b = compare (toLower a) (toLower b)
    in
        sortWith comp xs

stringsQuoted = regex "\".*?\""

stringsTripleQuoted = regex "\"\"\"(?:\\n|.)*?\"\"\""

singleLine txt =
    singleSpace <| replace All (regex "[\\r\\n]") (\a->" ") txt

singleSpace txt = 
    replace All (regex "[ ]+") (\_ -> " ") txt

tab n txt =
    (repeat (3*n) " ") ++ txt

tabLines n txt =
    join "\n" <| map (tab n) <| split "\n" txt

unique: List a -> List a
unique xs =
    uniqueHelp [] xs

uniqueHelp: List a -> List a -> List a
uniqueHelp checked remaining =
    case remaining of
        []->
            checked
        x::xs->
            case any (\a->(a==x)) (checked++xs) of
                False->
                    uniqueHelp (x::checked) xs
                True->
                   uniqueHelp checked xs 



