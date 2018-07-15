module Impfix.ImpTypes exposing (..)

type Constructors = Constructors (List String) | DotDot

type Expose = Simple String | Complex String Constructors | Operator String

type ExposeList = Qualified (List Expose) | Unqualified

type alias Import = {
      fullName: String
    , shortName: Maybe String
    , exposes: ExposeList
    , match: String
    }

