# VM translator

## Requirements

[Babashka](https://github.com/borkdude/babashka)

## How to use

`$ bb --classpath src --main compiler.main {file-path|folder}`


## Steps to create an executable jar file

1. `mkdir classes`
2. `clj -e "(compile 'compiler.main)"`
3. `clojure -A:uberjar --main-class translator.main`

