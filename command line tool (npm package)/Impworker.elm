module ImpWorker exposing (..)

import Impfix exposing (impfixGraft)
import ImpworkerPorts exposing (deliverImport, deliverSource, error, output)
import Json.Decode as Dec
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Enc exposing (object, Value)
import Platform exposing (program)

type alias Model =
    { imports: List String
    }

type Msg = 
      DecodeError String
    | Import String
    | Source File

type alias File =
    { name: String
    , code: String
    }

main =
  program
    { init = init
    , subscriptions = subscriptions
    , update = update
    }

init =
    { imports = [] 
    } ! []

subscriptions model =
    Sub.batch
      [ deliverImport Import
      , deliverSource decodeFile
      ]

update msg model = 
    case msg of
        DecodeError err->
             model ! [error err]
        Import txt ->
            { model | imports = txt::model.imports } ! []
        Source file ->
            model ! [output <| fixed file model.imports]

-- == --

decodeFile: Value -> Msg
decodeFile json =
  let
    fileDecoder = 
      decode
          File
             |> required "name" Dec.string
             |> required "code" Dec.string
  in
    case Dec.decodeValue fileDecoder json of
        Ok file ->
          Source file
        Err err->
          DecodeError err
         
fixed: File -> List String -> Value
fixed file unqualifiedImports = 
   object
      [ ("name", Enc.string file.name)
      , ("code", Enc.string <| impfixGraft file.code unqualifiedImports)
      ]