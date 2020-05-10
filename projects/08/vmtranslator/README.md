# VM translator

## Requirements

[Babashka](https://github.com/borkdude/babashka)

## How to use

`$ bb --classpath src --main translator.main {file-path}`


## Steps to create an executable jar file

1. `mkdir classes`
2. `clj -e "(compile 'translator.main)"`
3. `clojure -A:uberjar --main-class translator.main`

> TODO: 
> 1. Folders translation
> 4. Init
