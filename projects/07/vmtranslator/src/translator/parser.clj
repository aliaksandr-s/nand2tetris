(ns translator.parser
  (:require [translator.operations :as op]))

(defmulti parse (fn [cmd] (first cmd)))

(defmethod parse "push"
  [[cmd type val]] (op/push type val))

(defmethod parse "pop"
  [[cmd type val]] (op/pop-op type val))

(defmethod parse "add"
  [cmd] (op/two-sign-op :add))

(defmethod parse "sub"
  [cmd] (op/two-sign-op :sub))

(defmethod parse "and"
  [cmd] (op/two-sign-op :and))

(defmethod parse "or"
  [cmd] (op/two-sign-op :or))

(defmethod parse "neg"
  [cmd] (op/one-sign-op :neg))

(defmethod parse "not"
  [cmd] (op/one-sign-op :not))

(defmethod parse "eq"
  [cmd] (op/eq-op :eq))

(defmethod parse "gt"
  [cmd] (op/eq-op :gt))

(defmethod parse "lt"
  [cmd] (op/eq-op :lt))

(defmethod parse :default
  [cmd] 
  (throw (Exception. (str "No handler for command: " cmd)))
  ; "// to be defined"
  )


; (parse ["push" "constant" "10"])
