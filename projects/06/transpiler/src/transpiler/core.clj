(ns transpiler.core
  (:require [clojure.string :as str]
            [transpiler.a-inst :refer :all]
            [transpiler.c-inst :refer :all]
            [transpiler.symbols :refer :all]))

(defn empty-or-comment?
  [str]
  (or (str/blank? str)
      (str/starts-with? str "//")))

(def paths [
            ["../add/Add.asm"    "../add/Add.hack"]
            ["../max/MaxL.asm"   "../max/MaxL.hack"]
            ["../max/Max.asm"    "../max/Max.hack"]
            ["../pong/PongL.asm" "../pong/PongL.hack"]
            ["../pong/Pong.asm" "../pong/Pong.hack"]
            ["../rect/RectL.asm" "../rect/RectL.hack"]
            ["../rect/Rect.asm" "../rect/Rect.hack"]
            ])

(defn transform-to-bin
  [inst]
  (cond
    (C-inst? inst) (C-inst->bin inst)
    (A-inst? inst) (A-inst->bin inst)
    (var-inst? inst) (var->bin inst)
    :else inst))

(defn handle-labels
  [coll]
  (reduce (fn [new-coll unit]
            (if (str/starts-with? unit "(")
              (do
                (swap! symbol-table assoc
                       (-> unit read-string first str)
                       (count new-coll))
                (into new-coll []))
              (into new-coll [unit])))
          []
          coll))

(defn trim-line 
  [line]
  (-> line
      (str/split #"//")
      first
      str/trim))

(for [in-out paths]
  (do
    (reset-tables!)
    (as->
     (slurp (first in-out)) v
      (str/split v #"\r\n")
      (remove empty-or-comment? v)
      (map trim-line v)
      (handle-labels v)
      (map transform-to-bin v)
      (str/join "\r\n" v)
      (spit (last in-out) v))))
