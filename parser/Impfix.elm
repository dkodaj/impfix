module Impfix exposing (impfix)

import Impfix.Helpers exposing (among, clean, decomment, isPrefix, occurs, remove, removeStringLiterals, sortCaseInsensitive, unique)
import Impfix.ImpTypes exposing (Constructors(Constructors, DotDot), Expose(Simple, Complex, Operator), ExposeList(Qualified, Unqualified), Import)
import Impfix.Imports exposing (imports, importsRegex)
import Impfix.Unqualified exposing (uqImports)
import List exposing (concat, filter, foldl, map, member, sortBy)
import Regex exposing (find, HowMany(..), regex, replace)
import String exposing (contains, dropLeft, dropRight, indices, length, join, left, right, trim)

body: String -> String
body sourceTxt =
    let
        (header,rawBody) = headerBody sourceTxt
    in
        remove importsRegex <| decomment <| removeStringLiterals rawBody

check: List String -> String -> List Import -> Import -> (String, Maybe Import)
check funcsTypes srcBody qualify originalImp =
    let
        sameName a b = a.fullName == b.fullName
        imp =
            if originalImp.exposes == Unqualified
                then
                    case filter (sameName originalImp) qualify of
                        []->
                            originalImp
                        x::xs->
                            x
                else
                    originalImp
        name =
            case imp.shortName of
                Nothing->
                    imp.fullName ++ "."
                Just a->
                    a ++ "."
        namespaced = isPrefix name funcsTypes
        occurs x = among funcsTypes x
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
                        (originalImp.match, Just { imp | exposes = used })
                    False->
                        (originalImp.match, Nothing)
            _->
                (originalImp.match, Just { imp | exposes = used })


format: Import -> String
format imp =
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


funcsTypesInvoked: String -> List String
funcsTypesInvoked sourceTxt =
    funcsTypesHelp (body sourceTxt) [] "" True

funcsTypesHelp txt found nextWord waitForSeparator =
    -- This monster is a coded-up version of the following regex:
    -- regex "(?<="++separator++")((?:\\w|\\.)+)(?=(?:\\n|"++separator++"))"        
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
        fix imp = check funcsTypes srcBody qualify imp
        fixed = map fix originalImps
        funcsTypes = funcsTypesInvoked srcTxt
        originalImps = imports srcTxt
        qualify = filter relevant <| concat <| map uqImports importTxts
        relevant a = member a.fullName (map .fullName originalImps) 
        srcBody = body srcTxt
        graft (originalMatch,fixedImp) txt = 
            case fixedImp of
                Nothing->
                    remove1 originalMatch txt
                Just newImp->
                    replace1 originalMatch (format newImp) txt
    in
        (foldl (<<) identity <| map graft fixed) srcTxt 


newLines txt output =
    case right 1 txt of
        "\n"->
            newLines (dropRight 1 txt) (output ++ "\n")
        _->
            output

remove1: String -> String -> String
remove1 what here =
    let
        len = length what    
    in
        case indices what here of
            []->
                here
            n::whatever->
                left n here ++ dropLeft (n+len) here

replace1: String -> String -> String-> String
replace1 what with here =
    let
        len = length what    
    in
        case indices what here of
            []->
                here
            n::whatever->
                left n here ++ with ++ (newLines what "") ++ dropLeft (n+len) here

