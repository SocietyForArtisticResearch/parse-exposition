# Parse Exposition
Convert Graphical RC Expositions to Structured Haskell Data
(c) 2019 by Luc Döbereiner

The program outputs JSON data on standard out.

It only works for published expositions at the moment

## Requirements

## Build
```stack build```

## Build and run
```stack build --exec "parse-exposition https://www.researchcatalogue.net/view/343349/343350"```

## Run only
```stack exec parse-exposition https://www.researchcatalogue.net/view/343349/343350```

## TODO
* Only text tools are being read at the moment.
* Optional conversion to markdow using pandoc lib
* Make metadata from jsonld meta block optional in order to enable non-published expos