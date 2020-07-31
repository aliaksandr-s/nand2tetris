(ns compiler.utils
  (:require [clojure.string :as str]))

(defn empty-or-comment?
  [str]
  (or (str/blank? str)
      (str/starts-with? str "//")))

(defn split-by-line [str]
  (str/split-lines str))

(defn split-by-whitespace [str]
 (str/split str #"\s+"))

(defn join-by-line [lst]
  (str/join "\r\n" lst))

(defn build-file-path [path]
  (if (str/includes? path ".jack")
    (str/replace path #".jack" ".vm")
    (str path "/" path ".vm")))

(defn create-local-filename [file]
  (str/replace file #"/" "."))

(defn vm-file? [file]
  (str/includes? file ".vm"))

(defn jack-file? [file]
  (str/includes? file ".jack"))

(defn get-file-name [path]
  (-> path
      (str/split #"/")
      last
      (str/split #"\.")
      first))



;;;;;;;;;; xml stuff ;;;;;;;;;;;;

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

(defn make-open-tag [tag]
  (str "<" tag ">"))

(defn make-closing-tag [tag]
  (str "</" tag ">"))

(defn wrap-global [xml-content]
  (flatten [(str "<tokens>") xml-content (str "</tokens>")]))

(defn tokens->xml [tokens]
  (->> tokens (map make-tag) wrap-global))
;;;;;;;;;;;;

(defn write-line [w text]
 (.write w (str text "\n")))

(defn print-table [w table]
  (doseq [smbl table]
    (write-line w smbl)))


