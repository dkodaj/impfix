module Impfix.Unqualified exposing (uqImports)

import Impfix.Helpers exposing (clean, decomment, removeStringLiterals, unique)
import Impfix.ImpTypes exposing (Constructors(Constructors), Expose(Simple, Complex), ExposeList(Qualified), Import)
import Impfix.Imports exposing (exposeList)
import Impfix.TypeExtract exposing (grabTypeDefs, Type(..), TypeDef)
import List exposing (filter, map, member)
import Regex exposing (find, HowMany(..), Match, regex)
import String

--== Make unqualified imports explicit ==--

-- Unqualified imports are things like this:
-- "import List exposing (..)""
--  The present module helps make such imports explicit by 
--  looking into the code of imported module and checking
--  what it exposes.

{- NB:
   Only understands the following export format:

      module MyModule exposing (a, b, MyType1(..), MyType2(C,D))
   
   Doesn't understand things like this

      module Submodule (Animal(..)) where ...
-}

exposeAll: String -> List Expose
exposeAll sourceTxt =
    let
        toSimple a = Simple a
        funcs = map toSimple <| funcsDefined sourceTxt
        types = map exposeType <| grabTypeDefs sourceTxt
    in
        funcs ++ types

exposeType: TypeDef -> Expose
exposeType typeDef =
    let
        name = typeDef.name
    in
        case typeDef.theType of
            TypeProduct (a, b)->
                Complex name (Constructors [a])
            TypeUnion xs->
                let
                    grabConstructor (x,y) = x
                in
                    Complex name (Constructors <| map grabConstructor xs)
            _->
                Simple name

funcsDefined: String -> List String
funcsDefined sourceTxt =
    let
        extractName submatch =
            case submatch of
                Just a::whatever->
                    Just a
                _->
                    Nothing
        submatches = map .submatches <| find All funcsRegex <| removeStringLiterals <| decomment sourceTxt
        notReserved a = not <| member a ["port","module","import","type"]
    in
        unique <| filter notReserved <| clean <| map extractName submatches

funcsRegex = regex "(?:^|\\n)(?:port\\s+)?(\\w+)"
--funcsRegex = regex "(?<=(?:^|\\n))(?:port\\s+)?(\\w+)"

uqImports: String -> List Import
uqImports txt = 
    let
        cleanTxt = removeStringLiterals <| decomment txt
    in
        clean <| map uqImportGrab <| find All uqImportRegex cleanTxt

uqImportGrab: Match -> Maybe Import
uqImportGrab match =
    case match.submatches of
        Just ""::whatever->
            Nothing
        Just a::Nothing::bs->
            Just { fullName = a
                 , shortName = Nothing
                 , exposes = Qualified []
                 }
        Just a::Just ".."::bs->
            Just { fullName = a
                 , shortName = Nothing
                 , exposes = Qualified (exposeAll match.match)
                 }
        Just a::Just b::cs->
            Just { fullName = a
                 , shortName = Nothing
                 , exposes = Qualified (exposeList b)
                 }
        _->
            Nothing

uqImportRegex = regex "\\n*(?:port\\s+)?module\\s+((?:\\w|\\.)+)\\s+exposing\\s*\\(((?:.|\\n)+?)\\)\\s*?(?=(?:\\n$|\\n\\w))(?:(?:.|\\n)*?(?=(?:\\nmodule\\s+|$)))"
-- Grabs the whole of a module, submatching the module name and the thing inside "exposed(...)".
-- Can handle multiple modules copied after one another.

