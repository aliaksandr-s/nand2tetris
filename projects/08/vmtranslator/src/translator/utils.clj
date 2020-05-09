(ns translator.utils
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

(defn build-file-path [file]
  (str/replace file #".vm" ".asm"))

(defn get-file-name [path]
  (-> path
      (str/split #"/")
      last
      (str/split #"\.")
      first))

; (get-file-name "../../../MemoryAccess/BasicTest/BasicTest.vm")
