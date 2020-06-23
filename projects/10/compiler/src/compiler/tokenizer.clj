(ns compiler.tokenizer
  (:require [clojure.string :as str]))

(defn remove-comments [content]
  (str/replace content #"(?m)(\/\/.+)|(\/\*.*)|(^\s*\*.*)|(^\s*\*.+)" ""))

(defn pad-symbols [content]
  (str/replace content #"([{}()\[\].,;+\-*\/&|<>=~])" " $1 "))

(defn split-exluding-strings [content]
  (str/split content #"\s(?=([^\"]|\"[^\"]*\")*$)"))

(defn remove-empty [content]
  (remove str/blank? content))

(def keywords 
  ["class"  "constructor" "function" "method" 
   "field"  "static"  "var"  "int"  "char"  "boolean" 
   "void"  "true"  "false"  "null"  "this"  "let"  "do" 
   "if"  "else"  "while"  "return"])

(def symbols
  ["{" "}" "(" ")" "[" "]" "." "," ";" 
   "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"])

(defn get-type [token]
  (cond 
    (contains? (set keywords) token) :keyword
    (contains? (set symbols) token) :symbol
    (re-matches #"\d+" token) :integerConstant
    (re-matches #"\".*\"" token) :stringConstant
    ; :else (throw (Exception. (str token " is not valid token")))
    :else :identifier))

(defn handle-types [tokens] 
  (map (fn [token] {:type (get-type token) :value token}) tokens))

(defn tokenize [content]
  (->> content
       remove-comments
       pad-symbols
       str/split-lines
       (map split-exluding-strings)
       flatten
       remove-empty
       handle-types))

; (def ttt (make-tag {:type :symbol :value "\"helllo\""}))

