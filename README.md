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
 Usage: ```parse-exposition [-epub] [-textmd] [-md] [-d] [-latex] exposition-id```


## Run only
```stack exec parse-exposition 343349```

## PDF Conversion
First build the program once. `xelatex` is a requirement.

```$ ./export2pdf.sh <expositionId>```

## TODO and Ideas
* Don't download audio and video when exporting to latex
* Deal with links within the exposition, position and weave
* Deal with popovers
* Conversion to svg (perhaps in elm)
* CSS Grid Quantizer

