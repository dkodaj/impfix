module Impfix exposing (..)

import Impfix.Helpers exposing (among, clean, decomment, isPrefix, occurs, remove, removeStringLiterals, sortCaseInsensitive, unique)
import Impfix.ImpTypes exposing (Constructors(..), Expose(..), ExposeList(..), Import)
import Impfix.Imports exposing (imports, importsRegex)
import Impfix.Unqualified exposing (uqImports)
import List exposing (concat, filter, map, sortBy)
import Regex exposing (HowMany(..), regex)
import String exposing (contains, dropLeft, indices, join, left, trim)

body: String -> String
body sourceTxt =
    let
        headerRegex = regex "(?:port\\s+)?module (?:.|\\n)+?(?=(?:\\n\\w|$))"
    in
        remove importsRegex <| decomment <| remove headerRegex <| removeStringLiterals sourceTxt

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
    let
        nextChar = left 1 txt
        notReserved a = not <| among reservedWords a
        separator a = among [" ", "|", "[", "]", "(", ")", "-", ">", "<", ",", "+", "*", ":", "/"] a
    in
        case txt of
            ""->
                filter notReserved found
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
    let
        n =
            case indices "module " sourceTxt of
                []->
                    0
                x::xs->
                    x
    in
        headerHelp ("", dropLeft n sourceTxt)

headerHelp (header,txt) =
    let
        nextChar = left 1 txt
    in
        case nextChar of
            "\n"->
                (header, txt)
            ""->
                (header, txt)
            _->
                headerHelp (header++nextChar, dropLeft 1 txt)

impfix: String -> List String -> String
impfix srcTxt importTxts =
    let
        funcsTypes = funcsTypesInvoked srcTxt
        imps = unqualifiedModules ++ (filter (not << unqualified) impsFromSrc)
        impsFromSrc = imports srcTxt
        relevant a = among relevantNames a.fullName
        relevantNames = map .fullName impsFromSrc
        srcBody = body srcTxt
        unqualified a = among unqualifiedNames a.fullName
        unqualifiedModules = filter relevant <| concat <| map uqImports importTxts
        unqualifiedNames = map .fullName unqualifiedModules
    in
        join "\n" <| map output <| sortBy .fullName <| clean <| map (using funcsTypes srcBody) imps

impfixGraft: String->List String->String
impfixGraft srcTxt importTxts =
    let
        (header,body) = headerBody srcTxt
        imps = impfix srcTxt importTxts
        rest = trim <| remove importsRegex body
    in
        header ++ "\n\n" ++ imps ++ "\n\n" ++ rest

{-
impfixWorker: String->List String->String
impfixWorker srcTxt importTxts =
    let
        header = 
            case map .match <| find (AtMost 1) headerRegex srcTxt of
                []->
                    ""
                x::whatever->
                    x
        imps = impfix srcTxt importTxts
        rest = remove importsRegex <| remove headerRegex srcTxt
    in
        header ++ "\\n" ++ imps
-}
using: List String -> String -> Import -> Maybe Import
using funcsTypesInvoked srcBody imp =
    let
        name =
            case imp.shortName of
                Nothing->
                    imp.fullName
                Just a->
                    a
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