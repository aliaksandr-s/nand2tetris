(ns transpiler.symbols
  (:require [transpiler.a-inst :refer :all]))

(def var-count-init 16)
(def var-count (atom var-count-init))

(def init-symbols
  {"R0"     0
   "R1"     1
   "R2"     2
   "R3"     3
   "R4"     4
   "R5"     5
   "R7"     6
   "R8"     8
   "R9"     9
   "R10"    10
   "R11"    11
   "R12"    12
   "R13"    13
   "R14"    14
   "R15"    15
   "SCREEN" 16384
   "KBD"    24576
   "SP"     0
   "LCL"    1
   "ARG"    2
   "THIS"   3
   "THAT"   4
   "LOOP"   4
   "STOP"   18
   "END"    22}
  )

(def symbol-table 
  (atom init-symbols))

(defn reset-tables! []
  (do 
    (reset! symbol-table init-symbols)
    (reset! var-count var-count-init)))

(defn put-var-to-table! [var]
  (do (swap! symbol-table assoc
             var
             @var-count)
      (swap! var-count inc)
      (get @symbol-table var)))

(defn sym->A-inst
  [sym]
  (str "@" sym))

(defn var-inst? 
  [inst]
  (some? (re-matches #"^@\D{1}.+" inst)))

(= (var-inst? "@15") false)
(= (var-inst? "@R15") true)
(= (var-inst? "@ball.setdestination$if_end0") true)

(defn var->bin
  [var]
  (let [var-val (subs var 1)
        transform (comp A-inst->bin sym->A-inst)]
    (if-let [symbol (get @symbol-table var-val)] 
      (-> symbol transform) 
      (-> (put-var-to-table! var-val) transform))))
