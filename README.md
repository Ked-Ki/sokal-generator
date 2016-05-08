# sokal-generator
Postmodern fill text generation system written for Lab 1 in CMSC 23311 at UChicago 

## Building
Simply run the command:

```cabal build```

while in the project directory. Note that you may have to install some cabal
packages.

This cabal package produces two executables, `suck` and `spew`.

## suck
The `suck` program takes a list of urls, extracts the text from them, and builds a
model for later text generation. This output file is saved as `sokal.model` in the
directory suck was run from.

```usage: suck [url file]```

## spew
The `spew` program takes an argument specifying approximately how many words of
output are desired. Then, it generates the requested amount of text. `spew` must
be run in a directory with a pre-existing `sokal.model` file.

```usage: spew [word count (int)]```
