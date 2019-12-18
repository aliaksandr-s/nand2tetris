(ns transpiler.c-inst
  (:require [clojure.string :as str]))

(def comp-table 
  {"0"    "0101010"
   "1"    "0111111"
   "-1"   "0111010"
   "D"    "0001100"
   "A"    "0110000"
   "M"    "1110000"
   "!D"   "0001101"
   "!A"   "0110001"
   "!M"   "1110001"
   "-D"   "0001111"
   "-A"   "0110011"
   "-M"   "1110011"
   "D+1"  "0011111"
   "A+1"  "0110111"
   "M+1"  "1110111"
   "D-1"  "0001110"
   "A-1"  "0110010"
   "M-1"  "1110010"
   "D+A"  "0000010"
   "D+M"  "1000010"
   "D-A"  "0010011"
   "D-M"  "1010011"
   "A-D"  "0000111"
   "M-D"  "1000111"
   "D&A"  "0000000"
   "D&M"  "1000000"
   "D|A"  "0010101"
   "D|M"  "1010101"
  })

(defn comp->bin
  [comp]
  (get comp-table comp))

(= (comp->bin "0")   "0101010")
(= (comp->bin "A-1") "0110010")
(= (comp->bin "!M")  "1110001")

(def dest-table 
  {nil   "000"
   "M"   "001"
   "D"   "010"
   "MD"  "011"
   "A"   "100"
   "AM"  "101"
   "AD"  "110"
   "AMD" "111"
   })

(defn dest->bin
  [dest]
  (get dest-table dest))

(= (dest->bin nil)   "000")
(= (dest->bin "M")   "001")
(= (dest->bin "AMD") "111")


(def jump-table 
  {nil    "000"
   "JGT"  "001"
   "JEQ"  "010"
   "JGE"  "011"
   "JLT"  "100"
   "JNE"  "101"
   "JLE"  "110"
   "JMP"  "111"})

(defn jump->bin
  [jump]
  (get jump-table jump))

(= (jump->bin nil)   "000")
(= (jump->bin "JGT") "001")
(= (jump->bin "JMP") "111")

(def pattern #"^(?:(.*?)=)?(.+?)(?:$|;(.*?))$")

(defn C-inst->bin
  [inst]
  (let [symbols (vec (rest (re-matches pattern inst)))
        dest (get symbols 0)
        comp (get symbols 1)
        jump (get symbols 2)]
    (->
     ["111"
      (comp->bin comp)
      (dest->bin dest)
      (jump->bin jump)]
     clojure.string/join)))

(defn C-inst?
  [inst]
  (or
   (str/includes? inst "=")
   (str/includes? inst ";")))

(= (C-inst->bin "M=1")       "1110111111001000")
(= (C-inst->bin "D=D-M")     "1111010011010000")
(= (C-inst->bin "D=D-M;JGT") "1111010011010001")
(= (C-inst->bin "D;JGT")     "1110001100000001")

(= (C-inst? "M=1")   true)
(= (C-inst? "D;JGT") true)
(= (C-inst? "@3")    false)