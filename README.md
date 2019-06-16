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
 Usage: ```parse-exposition [-epub] [-textmd] [-md] [-d] exposition-id```


## Run only
```stack exec parse-exposition 343349```

## TODO
* Deal with links within the exposition, position and weave
* Deal with popovers
* epub output

* Conversion to svg (perhaps in elm)
* CSS Grid Quantizer

