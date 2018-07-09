module Impfix exposing (impfix, impfixGraft, output)

import Impfix.Helpers exposing (among, clean, decomment, isPrefix, occurs, remove, removeStringLiterals, sortCaseInsensitive, unique)
import Impfix.ImpTypes exposing (Constructors(Constructors, DotDot), Expose(Simple, Complex, Operator), ExposeList(Qualified, Unqualified), Import)
import Impfix.Imports exposing (imports, importsRegex)
import Impfix.Unqualified exposing (uqImports)
import List exposing (concat, filter, map, sortBy)
import Regex exposing (find, HowMany(..), regex)
import String exposing (contains, dropLeft, indices, join, left, trim)

body: String -> String
body sourceTxt =
    let
        (header,rawBody) = headerBody sourceTxt
    in
        remove importsRegex <| decomment <| removeStringLiterals rawBody

{-
funcsTypesInvoked: String -> List String
funcsTypesInvoked sourceTxt =
    let
        funcOrType = regex <| "(?<="++separator++")((?:\\w|\\.)+)(?=(?:\\n|"++separator++"))"        
        separator = "[ <\\|\\[\\]\\->\\(\\),+]" -- < | [ ] - > ( ) , +
    in
        map .match <| find All funcOrType <| body sourceTxt
-}

funcsTypesInvoked: String -> List String
funcsTypesInvoked sourceTxt =
    funcsTypesHelp (body sourceTxt) [] "" True

funcsTypesHelp txt found nextWord waitForSeparator =
    -- This monster is a coded-up version of a regex (see fincOrType above above).
    -- Lookbehind were first implemented in ES2018...
    let
        nextChar = left 1 txt
        notReserved a = not <| among reservedWords a
        separator a = among [" ", "|", "[", "]", "(", ")", "-", ">", "<", ",", "+", "*", ":", "/"] a
    in
        case txt of
            ""->
                filter notReserved (nextWord::found)
            _->
                case separator nextChar of
                    True->
                        case nextWord of
                            ""->
                                funcsTypesHelp (dropLeft 1 txt) found "" False
                            _->
                                funcsTypesHelp (dropLeft 1 txt) (nextWord::found) "" False
                    False->
                        case nextChar=="\n" of
                            True->
                                case nextWord of
                                    ""->
                                        funcsTypesHelp (dropLeft 1 txt) found "" True
                                    _->
                                        funcsTypesHelp (dropLeft 1 txt) (nextWord::found) "" True
                            False->   
                                case waitForSeparator of
                                    True->
                                        funcsTypesHelp (dropLeft 1 txt) found "" True
                                    False->
                                        funcsTypesHelp (dropLeft 1 txt) found (nextWord++nextChar) False

reservedWords =
    [ "as", "case"
    , "else", "exposing"
    , "if", "in"
    , "infix", "infixl", "infixr"
    , "let"
    , "module", "of", "port"
    , "then", "type"]

headerBody sourceTxt =
    case map .submatches (find (AtMost 1) headerBodyRegex sourceTxt) of
        (Just a::Just b::cs)::rest->
            (trim a,b)
        (Just a::Nothing::cs)::rest->
            (trim a,"")
        (Nothing::Just a::bs)::rest->
            ("",a)
        _->
            ("","")

headerBodyRegex = regex "\\n*((?:(?:port|effect)\\s+)?module\\s+(?:\\w|\\.)+\\s+(?:where\\s+\\{.+?\\}\\s+)?exposing\\s*\\((?:.|\\n)+?\\)\\s*?)(?=(?:$|--|{-|\\w))((?:.|\\n)*)"

impfix: String -> List String -> String
impfix srcTxt importTxts =
    let
        funcsTypes = funcsTypesInvoked srcTxt
        imps = unqualifiedModules ++ (filter (not << unqualified) impsFromSrc)
        impsFromSrc = imports srcTxt
        relevant a = among relevantNames a.fullName
        relevantNames = map .fullName <| filter unqualified impsFromSrc
        srcBody = body srcTxt
        unqualified a = a.exposes == Unqualified
        unqualifiedModules = filter relevant <| concat <| map uqImports importTxts
        unqualifiedNames = map .fullName unqualifiedModules
    in
        join "\n" <| map output <| sortBy .fullName <| clean <| map (using funcsTypes srcBody) imps

impfixGraft: String->List String->String
impfixGraft srcTxt importTxts =
    let
        (header, rawBody) = headerBody srcTxt
        imps = impfix srcTxt importTxts
        rest = trim <| remove importsRegex rawBody
    in
        header ++ "\n\n" ++ imps ++ "\n\n" ++ rest

using: List String -> String -> Import -> Maybe Import
using funcsTypesInvoked srcBody imp =
    let
        name =
            case imp.shortName of
                Nothing->
                    imp.fullName ++ "."
                Just a->
                    a ++ "."
        namespaced = isPrefix name funcsTypesInvoked
        occurs x = among funcsTypesInvoked x
        expCheck x =
            case x of
                Simple a->
                    case occurs a of
                        True->
                            Just (Simple a)
                        False->
                            Nothing
                Complex b c->
                    useType b c
                Operator a->
                    case contains a srcBody of
                        True->
                            Just (Operator a)
                        False->
                            Nothing
        useType x y =
           let
                constructors = useConstructors y
                default = Complex x constructors
           in 
               case occurs x of
                    True->
                        Just default
                    False->
                        case constructors of
                            DotDot->
                                Just default
                            Constructors []->
                                Nothing
                            _->
                                Just default
        useConstructors x  =
            case x of
                DotDot->
                    DotDot
                Constructors ys->
                    let
                        filtered = filter occurs ys
                    in
                        Constructors filtered
        used =
            case imp.exposes of
                Unqualified->
                    Unqualified
                Qualified a->
                    Qualified <| clean <| map expCheck a
    in
        case used of
            Qualified []->
                case namespaced of
                    True->
                        Just { imp | exposes = used }
                    False->
                        Nothing
            _->
                Just { imp | exposes = used }

output: Import -> String
output imp =
    let
        short =
            case imp.shortName of
                Nothing->
                    ""
                Just a->
                    " as " ++ a
        exposes =
            case imp.exposes of
                Unqualified->
                    " exposing (..)"
                Qualified []->
                    ""
                Qualified xs->
                    " exposing (" ++ join ", " (sortCaseInsensitive <| unique <| map stringify xs) ++ ")"
        stringify x =
            case x of
                Simple a->
                    a
                Complex b c->
                    case c of
                        DotDot->
                            b++"(..)"
                        Constructors []->
                            b
                        Constructors ds->
                            b ++ "(" ++ (join ", " ds) ++ ")"
                Operator a->
                    "("++a++")"
    in
        "import "++imp.fullName++short++exposes