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

(defn build-file-path [path]
  (if (str/includes? path ".vm")
    (str/replace path #".vm" ".asm")
    (str path "/" path ".asm")))

(defn create-local-filename [file]
  (str/replace file #"/" "."))

(defn vm-file? [file]
  (str/includes? file ".vm"))

(defn get-file-name [path]
  (-> path
      (str/split #"/")
      last
      (str/split #"\.")
      first))

; (get-file-name "../../../MemoryAccess/BasicTest/BasicTest.vm")
