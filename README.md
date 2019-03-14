# Parse Exposition
Convert Graphical RC Expositions to Structured Haskell Data
(c) 2019 by Luc Döbereiner

The program outputs JSON data on standard out and downloads media into a "media" folder.

## Requirements

## Build
```stack build```

## Build and run
```stack build --exec "parse-exposition 343349"```

## Options
* `-m` for markdown conversion
* `-d` for download of media files (into "./media" folder)

```stack build --exec "parse-exposition -d -m 384876"```


## Run only
```stack exec parse-exposition 343349```

## TODO
* Conversion to svg (perhaps in elm)
* CSS Grid Quantizer

