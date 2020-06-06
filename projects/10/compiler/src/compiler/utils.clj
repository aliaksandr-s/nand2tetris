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
    (str/replace path #".jack" ".xmlt")
    (str path "/" path ".xmlt")))

(build-file-path "Game/Game.jack")

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

; (get-file-name "../../../MemoryAccess/BasicTest/BasicTest.vm")
