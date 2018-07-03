module Impfix.Imports exposing (..)

import Impfix.Helpers exposing (bracketed, clean, debracket, decomment, removeStringLiterals)
import Impfix.ImpTypes exposing (Constructors(..), Expose(..), ExposeList(..), Import)
import List exposing (map)
import Regex exposing (HowMany(..), find, regex)
import String exposing (split, trim)

exposeGrab: List (Maybe String) -> Maybe Expose
exposeGrab submatch =
    case submatch of
        Just x::Nothing::whatever->
            case bracketed (trim x) of
                True->
                    Just <| Operator (debracket <| trim x)
                False->
                    Just <| (Simple <| trim x)
        Just x::Just y::whatever->
            let
                words = map trim <| split "," y
                constructors = 
                    case words of
                        [".."]->
                            DotDot
                        _->
                            Constructors words
            in
                Just <| (Complex (trim x) constructors)
        _->
            Nothing

exposeList: String -> List Expose
exposeList txt =
    clean <| map exposeGrab <| map .submatches <| find All expListRegex txt

expListRegex = regex "(.+?)\\s*(?:\\(((?:\\w|\\s|\\.|,)*?)\\)\\s*)?(?:,\\s*|$)"

imports: String -> List Import
imports sourceTxt =
    clean <| map importGrab <| map .submatches <| find All importsRegex <| decomment <| removeStringLiterals sourceTxt

importGrab: List (Maybe String) -> Maybe Import
importGrab submatch =
    case submatch of
        Just ""::whatever->
            Nothing
        Just a::b::Nothing::cs->
            Just { fullName = a
                 , shortName = b
                 , exposes = Qualified []
                 }
        Just a::b::Just ".."::cs->
            Just { fullName = a
                 , shortName = b
                 , exposes = Unqualified
                 }
        Just a::b::Just c::ds->
            Just { fullName = a
                 , shortName = b
                 , exposes = Qualified (exposeList c)
                 }
        _->
            Nothing

importsRegex =
    regex "import\\s+((?:\\w|\\.)+)\\s*?(?: as (\\w+)\\s*?)?(?: exposing\\s*\\(((?:.|\\n)+?)\\)\\s*)?(?=(?:\\n$|\\n\\w)|\\s*--|\\s*\\{-)"
--grabs module name, nickname ("as ...") and the things inside "exposing (...)", if any