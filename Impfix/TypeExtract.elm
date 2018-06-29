module Impfix.TypeExtract exposing (..)

import Impfix.Helpers exposing (clean, components, debracket, decomment, dropWord, inBrackets, inCurly, outsideBrackets, outsideCurly, removeColons, removeStringLiterals, singleLine, singleSpace)
import List exposing (map)
import Regex exposing (HowMany(..), Match, find, regex)
import String exposing (dropLeft, indices, left, split, trim, words)

type Type = 
      TypeArray Type
    | TypeBool
    | TypeDict (Type, Type)
    | TypeFloat
    | TypeInt
    | TypeList Type
    | TypeMaybe Type
    | TypeOpaque String --a type not defined in the source
    | TypeProduct (String, List Type)
    | TypeRecord (List Field)
    | TypeString
    | TypeTuple (Type, Type)
    | TypeUnion ( List (String, List Type) )

type alias Field = { 
      name: String
    , fieldType: Type 
    }

type alias RawType = {
      name: String
    , def: String
    }

type alias TypeDef = {
      name: String
    , theType: Type  
    }

grabTypeDefs: String -> List TypeDef
grabTypeDefs txt =
    let
        toTypeDef a =
            { name = a.name, theType = typeOf True a.def }
    in
        map toTypeDef <| grabRawTypes txt 

grabRawType: List (Maybe String) -> Maybe RawType
grabRawType submatches =
    case submatches of
        Just a:: Just b ::cs->
            Just { name = trim a, def = trim <| singleLine b }
        _->
            Nothing

grabRawTypes: String -> List RawType
grabRawTypes txt =
    clean <| map grabRawType <| map .submatches <| regexIt <| decomment <| removeStringLiterals txt

regexIt: String -> List Match
regexIt txt = find All typeRegex txt

typeRegex = regex "type\\s+(?:alias )?\\s*(\\w+)\\s*=([\\w(){},|.:_ \\r\\n]+)(?=(?:\\r\\w|\\n\\w)|$)"


--== Recognize types ==--

typeOf: Bool-> String  -> Type
typeOf maybeUnion def  = 
--  typeOf True "List String" == TypeList TypeString
--  typeOf False "List String" == TypeList TypeString
--  typeOf True "MyType | String" == TypeUnion [TypeOpaque "MyType", TypeString]
--  typeOf True "MyType" == TypeUnion [TypeOpaque "MyType"]
--  typeOf False "MyType" == TypeOpaque "MyType"
    let
        subType a = typeOf False a
    in
        case detuple def of
            Just (a,b)->
                TypeTuple (subType a, subType b)
            Nothing->
                case derecord def of
                    x::xs->
                        let
                            makeField (a,b) = Field a (subType b)
                        in
                            TypeRecord <| map makeField (x::xs)
                    []->
                        case words (debracket def) of
                            []->
                                TypeOpaque "Type conversion error: empty string"
                            x::xs->
                                case x of
                                    "Array"->
                                        TypeArray (subType <| dropWord x <| debracket def)
                                    "Bool"->
                                        TypeBool
                                    "Dict"->
                                        case deunion (debracket def) of
                                            (_,x::y::zs)::vs->
                                                TypeDict (subType x, subType y)
                                            _->
                                                TypeOpaque "Error parsing def as a Dict"
                                    "Dict.Dict"->
                                        case deunion (debracket def) of
                                            (_,x::y::zs)::vs->
                                                TypeDict (subType x, subType y)
                                            _->
                                                TypeOpaque "Error parsing def as a Dict"
                                    "Float"->
                                        TypeFloat
                                    "Int"->
                                        TypeInt
                                    "List"->
                                        TypeList (subType <| dropWord x <| debracket def)
                                    "Maybe"->
                                        TypeMaybe (subType <| dropWord x <| debracket def)
                                    "String"->
                                        TypeString
                                    _->
                                        case maybeUnion of
                                            True->
                                                let
                                                    constructor (a,b) = 
                                                        case b of
                                                            [""]->
                                                                (a, [])
                                                            _->
                                                                (a, map subType b)
                                                in
                                                    case deunion def of
                                                        x::[]->
                                                            TypeProduct (constructor x)
                                                        x::xs->
                                                            TypeUnion <| map constructor (x::xs)
                                                        []->
                                                            TypeOpaque "Union type conversion error: empty string"
                                            False->
                                                TypeOpaque (removeColons x)


--== Destructuring records, tuples, union types ==--

-- derecord "{x: Int, y: String}" == [("x","Int"), ("y","String")]
-- detuple "(String, Int)" == Just ("String", "Int")
-- deunion "A String Int | B Int" == [("A", ["String", "Int"]), ("B", ["Int"])]

derecord: String -> List (String, String)
derecord txt =
    case inCurly txt of
        Nothing->
            []
        Just x->
            derecordHelp (indices "," x) x

derecordHelp: List Int -> String -> List (String, String)
derecordHelp idxs txt =
    case idxs of
        []->
            case toField txt of
                Just (a,b)->
                    [(a,b)]
                Nothing->
                    []
        x::xs->
            case outsideBrackets x txt && outsideCurly x txt of
                True->
                    let
                        remains = dropLeft (x+1) txt
                        newIdxs = indices "," remains
                    in
                        case toField (left x txt) of
                            Just (a,b)->
                                (a,b) :: derecordHelp newIdxs remains
                            Nothing->
                                derecordHelp newIdxs remains
                False->
                    derecordHelp xs txt

toField: String -> Maybe (String, String)
toField def =
    case split ":" def of
        x::xs->
            Just (trim x, dropWord x def)
        []->
            Nothing

detuple: String -> Maybe (String,String)
detuple txt =
    case inBrackets txt of
        Nothing->
            Nothing
        Just x->
            detupleHelp (indices "," x) x

detupleHelp: List Int -> String -> Maybe (String, String)
detupleHelp idxs txt =
    case idxs of
        []->
            Nothing
        x::xs->
            case outsideBrackets x txt && outsideCurly x txt of
                True->
                    Just (left x txt, dropLeft (x+1) txt)
                False->
                    detupleHelp xs txt

deunion: String -> List (String, List String)
deunion txt =
    clean <| map deunionHelp <| split "|" <| singleSpace txt

deunionHelp: String -> Maybe (String, List String)
deunionHelp txt =
    case words (trim txt) of
        x::ys->
            Just (x, components <| dropWord x (trim txt))
        []->
            Nothing
