# Command line tool

## Install
```
$ npm install -g elm-impfix
```

## Usage

```
$ elm-impfix MyModule.elm -o ../build/MyModule.elm
```

## Flags
### Output (optional)

```
$ elm-impfix MyModule.elm -o path/to/output.elm
```

Defaults to name_Impfix.ext (if source = name.ext). Use "/" in paths.

### Qualifying unqualified imports (optional)
```
$ elm-impfix MyModule.elm -q path/to/ImportedModule.elm
$ elm-impfix MyModule.elm -q "Imported1.elm Imported2.elm"
```

Makes unqualified imports explicit ("import ImportedModule exposing (..)" > "import ImportedModule exposing (this, that)").

## Wildcards

All params support wildcards:

```
$ elm-impfix "*.elm"  -o "../build/*.*"  -q "./Imports/*.elm"
```

Wildcard params must be quoted. Directory wildcards can't be used in -o.

## Bugs

Bug reports are welcome and likely.

