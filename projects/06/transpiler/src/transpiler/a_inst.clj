(ns transpiler.a-inst
  (:require [clojure.string :as str]))

(defn add-zeros [inst]
  (let [res-len 16
        cur-len (count inst)
        fin-len (- res-len cur-len)]
    (as-> (repeat fin-len 0) s
          (apply str s)
          (str s inst))))

(defn A-inst->bin
  [inst]
  (-> inst
      (subs 1)
      (read-string)
      (Integer/toString 2)
      (add-zeros)))

(defn A-inst?
  [inst]
  (some? (re-matches #"^@\d+" inst)))

(= (A-inst->bin "@2") 
   "0000000000000010")

(= (A-inst->bin "@0") 
   "0000000000000000")

(= (A-inst? "@0") true)
(= (A-inst? "M=1") false)