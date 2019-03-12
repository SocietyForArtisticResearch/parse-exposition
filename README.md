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

## Options
* `-m` for markdown conversion
* `-d` for download of media files (into "./media" folder)

```stack build --exec "parse-exposition -d -m https://www.researchcatalogue.net/view/384876/384878"```


## Run only
```stack exec parse-exposition https://www.researchcatalogue.net/view/343349/343350```

## TODO
* Download all weaves of an exposition
* Make metadata from jsonld meta block optional in order to enable non-published expos (perhaps get metadata from menubar)