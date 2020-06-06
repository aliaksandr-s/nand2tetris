(ns compiler.tokenizer
  (:require [clojure.string :as str]))

(defn remove-comments [content]
  (str/replace content #"(?m)(\/\/.+)|(\/\*.*)|(^\s*\*.*)|(^\s*\*.+)" ""))

(defn pad-symbols [content]
  (str/replace content #"([{}()\[\].,;+\-*\/&|<>=~])" " $1 "))

(defn split-exluding-strins [content]
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


; <, >, ", and & are outputted as
; &lt;, &gt;, &quot;, and &amp;

(defn transform-symbol [symbol]
  (case symbol
    "<" "&lt;"
    ">" "&gt;"
    "&" "&amp;"
    symbol))

; (transform-symbol ">")

(defn transform-value [token-obj]
  (cond (= (:type token-obj) :stringConstant) 
           (update token-obj :value str/replace #"\"" "")
        (= (:type token-obj) :symbol) 
           (update token-obj :value transform-symbol)
        :else token-obj))

; (transform-value {:type :stringConstant :value "\"Hello\""})
; (transform-value {:type :symbol :value ">"})


(defn make-tag [token-obj]
  (let [tag (-> :type token-obj name)
        value (:value (transform-value token-obj))]
    (str "<" tag ">" 
         " " value " "
         "</" tag ">")))

(defn handle-types [tokens] 
  (map (fn [token] {:type (get-type token) :value token}) tokens))

(defn wrap-global [xml-content]
  (flatten [(str "<tokens>") xml-content (str "</tokens>")]))

(defn tokenize [content]
  (->> content
       remove-comments
       pad-symbols
       split-exluding-strins
       remove-empty
       handle-types
       (map make-tag)
       wrap-global))


; (def ttt (make-tag {:type :symbol :value "\"helllo\""}))

