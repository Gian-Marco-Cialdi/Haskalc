# Haskalc
A text calculator (that also supports trigonometric functions) written in Haskell.

## Usage
From any terminal that you fancy:
```calc "*expression*"```

The result will be printed on screen.

For now, not enclosing the input expression between quotes may cause errors.

## Installation
At the moment, this program must be built from source, but it has only one source file.

Steps:
1. Clone repository;
2. Inside the repo directory, type:
```mkdir bin```
and:
```ghc src/Main.hs -o bin/calc```
