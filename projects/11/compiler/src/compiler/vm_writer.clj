(ns compiler.vm-writer
  (:require [compiler.utils :as utl]
            [clojure.string :as str]))

(defn write-push [w segment index]
  (utl/write-line w (str/join " " ["push" segment index])))

(defn write-pop [w segment index]
  (utl/write-line w (str/join " " ["pop" segment index])))

(defn write-arithmetic [w command]
  (utl/write-line w command))

(defn write-label [w label]
  (utl/write-line w (str "label " label)))

(defn write-go-to [w label]
  (utl/write-line w (str "goto " label)))

(defn write-if [w label]
  (utl/write-line w (str "if-goto " label)))

(defn write-call [w name n-args]
  (utl/write-line w (str/join " " ["call" name n-args])))

(defn write-function [w name n-locals]
  (utl/write-line w (str/join " " ["function" name n-locals])))

(defn write-return [w]
  (utl/write-line w "return"))
