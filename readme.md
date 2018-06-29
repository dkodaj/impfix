# Elm imports fix

Removes unused imports from Elm code. You must pass in the source code as text.

```elm
import Impfix exposing (impfix)

sampleCode = 
	"""
module MyApp exposing (..)

import Html
import Json.Encode as Enc
import List exposing (any, concat, filter, map)
import MyModule exposing (..)

myFunc xs =
	map Enc.int <| map f xs

	"""

impfix sampleCode [] ==
	"""
import Json.Encode as Enc
import List exposing (map)
import MyModule exposing (..)
	"""

sampleModule = 
	"""
module MyModule exposing (..)

f x = x + 1

g x = x - 1 
	"""

impfix sampleCode [sampleModule] == 
	"""
import Json.Encode as Enc
import List exposing (map)
import MyModule exposing (f)
	"""
```

## Use it from the browser

Copy/paste your code into the [live version](https://dkodaj.github.io/impfix).