(ns translator.parser
  (:require [translator.operations :as op]))

(defmulti parse (fn [cmd] (first cmd)))

(defmethod parse "push"
  [[cmd type val]] (op/push type val))

(defmethod parse "pop"
  [[cmd type val]] (op/pop-op type val))

(defmethod parse "add"
  [_] (op/two-sign-op :add))

(defmethod parse "sub"
  [_] (op/two-sign-op :sub))

(defmethod parse "and"
  [_] (op/two-sign-op :and))

(defmethod parse "or"
  [_] (op/two-sign-op :or))

(defmethod parse "neg"
  [_] (op/one-sign-op :neg))

(defmethod parse "not"
  [_] (op/one-sign-op :not))

(defmethod parse "eq"
  [_] (op/eq-op :eq))

(defmethod parse "gt"
  [_] (op/eq-op :gt))

(defmethod parse "lt"
  [_] (op/eq-op :lt))

(defmethod parse "label"
  [[cmd id]] (op/label-handle id))

(defmethod parse "if-goto"
  [[cmd id]] (op/if-goto id))

(defmethod parse "goto"
  [[cmd id]] (op/goto id))

(defmethod parse "function"
  [[cmd name n]] (op/function name n))

(defmethod parse "return"
  [_] (op/return))

(defmethod parse "call"
  [[cmd name n]] (op/call name n))

(defmethod parse :default
  [cmd] 
  ; (throw (Exception. (str "No handler for command: " cmd)))
  "// to be defined"
  )

; (parse ["function" "SimpleFunction.test" "2"])
; (parse ["push" "constant" "10"])
; (parse ["label" "LOOP"])
