port module ImpworkerPorts exposing (..)

import Json.Decode exposing (Value)

--from JavaScript to Elm
port deliverImport : (String -> msg) -> Sub msg

port deliverSource : (Value -> msg) -> Sub msg

--from Elm to JavaScript
port error: String -> Cmd msg

port output : Value -> Cmd msg