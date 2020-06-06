(ns compiler.main
  (:require [compiler.utils :as utl]
            [clojure.java.io :as io]
            [compiler.tokenizer :refer [tokenize]]
            )
  (:gen-class))

(def folder? (atom false))

(defn proccess-file-content [content]
  (->> content tokenize))

(defn proccess-file [file]
  (with-open [w (clojure.java.io/writer (utl/build-file-path file) :append false)]
    (let [xml-content (->> file slurp proccess-file-content)]
      xml-content
      (doseq [tag xml-content]
        (.write w (str tag "\n"))))))

(defn handle-folder [folder]
  (let [files (.listFiles (io/file folder))]
    (->> files
         (filter utl/jack-file?)
         (map #(proccess-file %)))))

(defn -main [& _args]
  (let [[path] _args
        isFolder? (.isDirectory (io/file path))]
    (if isFolder?
      (do 
        (reset! folder? true)
        (handle-folder path))
      (proccess-file path))))


; (def test-file "../Test/Main.jack")
(def test-array  "../ArrayTest/Main.jack")
(def test-square-main "../Square/Main.jack")
(def test-square-square "../Square/Square.jack")
(def test-square-game "../Square/SquareGame.jack")

; (proccess-file test-array)

; (let [[file] *command-line-args*]
;   (println "File:" (slurp file)))

