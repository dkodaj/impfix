module  Impfix exposing (impfix)

import List exposing (any, concat, filter, map, sortBy, sortWith)
import List.Extra exposing (splitWhen)
import Regex exposing (find, HowMany(..), regex, replace)
import String exposing (contains, dropLeft, dropRight, join, left, right, split, toLower, trim)

type alias Import = {
      fullName: String
    , shortName: Maybe String
    , exposes: List String  
    }

body sourceTxt =
    remove importsRegex <| decomment <| remove header <| removeStringLiterals sourceTxt

funcsTypesDefined sourceTxt =
    let
        sourceBody = body sourceTxt
        funcs = filter notReserved <| grabNames <| find All funcDef sourceBody
        funcDef =  regex "(?:\\n|^)(\\w+)\\s+[\\w|\\s]*="
        -- "find all sequences consisting of letters,  beginning on a newline and followed by (some words plus) "=""
        notReserved = not << among reserved
        types = grabNames <| find All typeDef sourceBody
        typeDef = regex "(?:\\n|^)type (?:alias )?(\\w+)\\s+[\\w|\\s]*="
        -- "find all sequences beginning with "type" on a newline and ending with "=""
        grabName submatch =
            case submatch of
                Just x::xs->
                    Just x
                _->
                    Nothing
        grabNames x = clean <| map grabName <| map .submatches x
    in
        List.sort <| unique <| funcs ++ types
        
funcsTypesInvoked sourceTxt =
    let
        funcOrType = regex <| "(?<="++separator++")((?:\\w|\\.)+)(?=(?:\\n|"++separator++"))"
    in
        map .match <| find All funcOrType <| body sourceTxt

header = regex "module .*"

impfix sourceTxt modulesTxt =
    let
        addImport bs a = 
            case splitWhen (sameName a) bs of
                Nothing->
                    a::bs
                Just (xs,ys)->
                    case ys of
                        y::zs->
                            xs ++ {y | exposes = a.exposes}::zs 
                        []->
                            a::bs
        funcsTypes = funcsTypesInvoked sourceTxt
        importsBase = imports sourceTxt
        importsPooled = pool importsBase mods
        mods = modules modulesTxt
        pool xs ys =
            case ys of
                []->
                    xs
                y::zs->
                    pool (addImport xs y) zs
        sameName x y = x.fullName == y.fullName
    in
        join "\n" <| map output <| sortBy .fullName <| clean <| map (using funcsTypes sourceTxt) importsPooled

importGrab submatch =
    let
        exposes z =
            case z of
                Nothing->
                    []
                Just a->
                    map trim <| split "," <| debracket a
    in
        case submatch of
            x::y::z::vs->
                case x of
                    Nothing->
                        Nothing
                    Just ""->
                        Nothing
                    Just a->
                        Just {fullName = a, shortName = y, exposes = exposes z}
            _->
                Nothing

imports sourceTxt =
    clean <| map importGrab <| map .submatches <| find All importsRegex <| decomment <| removeStringLiterals sourceTxt

importsRegex =
    regex "import\\s+((?:\\w|\\.)+)\\s*(?: as (\\w+))?\\s*(?:exposing\\s*(\\((?:.|\\n)+?\\)(?=(?:\\n+\\w|\\n*$))))?(?=(?:\\n+\\w|$))"

moduleGrab match =
    let
        exposes = 
            case match.submatches of
                _::Just x::ys->
                    case trim x of
                        "(..)"->
                            funcsTypesDefined <| body match.match
                        _->
                            map trim <| split "," <| debracket x
                _->
                    []
    in
        case match.submatches of
            Just x::xs->
                Just { fullName = x, shortName = Nothing, exposes = exposes }
            _->
                Nothing

modules txt = clean <| map moduleGrab <| find All modulesRegex <| removeStringLiterals <| decomment txt

modulesRegex = regex "\\n*module\\s+((?:\\w|\\.)+)\\s+exposing\\s*(\\((?:.|\\n)+?\\)(?=(?:\\n+\\w|\\n*$)))(?:(?:.|\\n)*?(?=(?:\\nmodule\\s+|$)))"

namespaced moduleName sourceTxt =
    let
        prefixed = regex <| "(?<="++separator++")"++moduleName++"\\."
    in
        find (AtMost 1) prefixed (body sourceTxt) /= []

using funcsTypes sourceTxt mod =
    let
        allExposed = mod.exposes == [".."]
        exposedDotted = filter (contains "..") mod.exposes
        exposedNormal = filter (not << bracketed) <| filter (not << contains "..") mod.exposes
        exposedOperators = filter bracketed mod.exposes
        exposedUsed = filter (among funcsTypes) exposedNormal ++ filter usingOperator exposedOperators
        moduleName = case mod.shortName of
            Nothing->
                mod.fullName
            Just a->
                a
        namespacing = namespaced moduleName sourceTxt
        usingOperator op = contains (debracket op) (body sourceTxt)
    in
        case allExposed of
            True->
                Just mod
            False->              
                case exposedUsed of
                    []->
                        case namespacing of
                            True->
                                Just { mod | exposes = sortCaseInsensitive <| exposedDotted }
                            False->
                                case exposedDotted of
                                    []->
                                        Nothing
                                    _->
                                        Just { mod | exposes = sortCaseInsensitive <| exposedDotted }
                    _->
                        Just { mod | exposes = sortCaseInsensitive <| exposedUsed ++ exposedDotted }

--== Helpers ==--

among xs y =
    List.member y xs

bracketed txt =
    left 1 txt == "(" && right 1 txt == ")"

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

decomment txt = 
    let
        singleComment = regex "--.*"
        multiComment = regex "\\{\\-(.|\\n)*?\\-\\}"       
    in
        remove singleComment <| remove multiComment txt

debracket txt =
    let
        trimmed = trim txt
    in
        if left 1 trimmed == "(" && right 1 trimmed == ")"
            then
                trim <| dropLeft 1 <| dropRight 1 trimmed
            else
                trimmed

output mod =
    let
        short =
            case mod.shortName of
                Nothing->
                    ""
                Just a->
                    " as " ++ a
        exposes =
            case mod.exposes of
                []->
                    ""
                _->
                    " exposing (" ++ join ", " mod.exposes ++ ")"

    in
        "import "++mod.fullName++short++exposes

remove regex txt = replace All regex (\a->"") txt

removeStringLiterals txt = remove stringsDoubleQuoted <| remove stringsTripleQuoted txt

reserved =
    [ "as", "case"
    , "else", "exposing"
    , "if", "in"
    , "infix", "infixl", "infixr"
    , "let"
    ,"module", "of", "port"
    ,"then", "type"]

separator = "[ <\\|\\[\\]\\->\\(\\),+]" -- < | [ ] - > ( ) , +

sortCaseInsensitive xs =
    let
        comp a b = compare (toLower a) (toLower b)
    in
        sortWith comp xs

stringsDoubleQuoted = regex "\".*?\""

stringsTripleQuoted = regex "\"\"\"(?:\\n|.)+?\"\"\""

unique xs =
    uniqueHelp [] xs

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


