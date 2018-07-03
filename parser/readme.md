# Elm imports cleanup tool

Removes unused imports from Elm code. You must pass in the source code as text or install the command line tool from npm (see below).

```elm
import Impfix exposing (impfix)

sampleModule = 
	"""
module MyApp exposing (..)

import Html
import Json.Encode as Enc
import List exposing (any, concat, filter, map)
import MyModule exposing (..)

myFunc xs =
	map Enc.int <| map f xs

	"""

impfix sampleModule [] ==
	"""
module MyApp exposing (..)

import Json.Encode as Enc
import List exposing (map)
import MyModule exposing (..)
	"""

sampleImport = 
	"""
module MyImport exposing (..)

f x = x + 1

g x = x - 1 
	"""

impfix sampleModule [sampleImport] == 
	"""
module MyApp exposing (..)

import Json.Encode as Enc
import List exposing (map)
import MyModule exposing (f)
	"""
```

## Command line tool

Install the [npm package](https://www.npmjs.com/package/elm-impfix):

```
npm install -g elm-impfix
```

Then:

```
$ elm-impfix MyModule.elm
```

## Try it in the browser

Copy/paste your code into the [live version](https://dkodaj.github.io/impfix).