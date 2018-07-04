module Impfix.Unqualified exposing (uqImports)

import Impfix.Helpers exposing (bracketed, clean, debracket, decomment, removeStringLiterals, unique)
import Impfix.ImpTypes exposing (Constructors(Constructors, DotDot), Expose(Simple, Complex, Operator), ExposeList(Qualified), Import)
import Impfix.TypeExtract exposing (grabTypeDefs, Type(..), TypeDef)
import List exposing (filter, map, member)
import Regex exposing (find, HowMany(All), Match, regex)
import String exposing (split, trim)

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

exposeAll: List Expose -> String -> List Expose
exposeAll allTypes sourceTxt =
    let
        toSimple a = Simple a
        funcs = map toSimple <| funcsDefined sourceTxt
    in
        funcs ++ allTypes

exposeGrab: List Expose -> List (Maybe String) -> Maybe Expose
exposeGrab allTypes submatch =
    case submatch of
        Just x::Nothing::whatever->
            let
                name = trim x
            in
                case bracketed name of
                    True->
                        Just <| Operator (debracket name)
                    False->
                        Just <| (Simple name)
        Just x::Just y::whatever->
            let
                name = trim x
                relevantType = filter typeFilter allTypes
                typeFilter a = 
                  case a of
                    Simple b->
                        b == name
                    Complex b _->
                        b == name
                    Operator _->
                        False
                words = map trim <| split "," y
            in
                case words of
                    [".."]->
                        case relevantType of
                          []->
                            Just <| Complex name DotDot
                          a::bs->
                            Just a
                    []->
                        Just <| Simple name
                    _->
                        Just <| Complex name (Constructors words)
        _->
            Nothing

exposeList: List Expose -> String -> List Expose
exposeList allTypes exposeTxt  =
    clean <| map (exposeGrab allTypes) <| map .submatches <| find All expListRegex exposeTxt

expListRegex = regex "(.+?)\\s*(?:\\(((?:\\w|\\s|\\.|,)*?)\\)\\s*)?(?:,\\s*|$)"

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
        allTypes = map exposeType <| grabTypeDefs cleanTxt
    in
        clean <| map (uqImportGrab allTypes) <| find All uqImportRegex cleanTxt

uqImportGrab: List Expose -> Match -> Maybe Import
uqImportGrab allTypes match =
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
                 , exposes = Qualified (exposeAll allTypes match.match)
                 }
        Just a::Just b::cs->
            Just { fullName = a
                 , shortName = Nothing
                 , exposes = Qualified (exposeList allTypes b)
                 }
        _->
            Nothing

uqImportRegex = regex "\\n*(?:(?:port|effect)\\s+)?module\\s+((?:\\w|\\.)+)\\s+(?:where\\s+\\{.+?\\}\\s+)?exposing\\s*\\(((?:.|\\n)+?)\\)\\s*?(?=(?:$|--|{-|\\w))(?:.|\\n)*"
-- Grabs the whole of a module, submatching the module name and the thing inside "exposed(...)".
--"\\n*(?:port\\s+)?module\\s+((?:\\w|\\.)+)\\s+exposing\\s*\\(((?:.|\\n)+?)\\)\\s*?(?=(?:\\n$|\\n\\w))(?:(?:.|\\n)*)"

