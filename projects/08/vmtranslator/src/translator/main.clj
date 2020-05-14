(ns translator.main
  (:require [translator.utils :as utl]
            [translator.parser :refer [parse]]
            [translator.operations :refer [init file-name]]
            [clojure.java.io :as io])
  (:gen-class))

(def folder? (atom false))

(defn proccess-file-content [content]
  (->> 
    content 
    utl/split-by-line
    (remove utl/empty-or-comment?)
    (map utl/split-by-whitespace)
    (map parse)
    (#(if @folder? (conj (init) %) %))
    ; (fn [ls]
    ;   (if folder? (conj (init) ls) ls))
    ; (conj (init))
    flatten
    utl/join-by-line))

(defn proccess-file [file]
  (reset! file-name (utl/create-local-filename file))
  (->> file 
       slurp
       proccess-file-content))

(defn handle-file [file]
  (spit (utl/build-file-path file) 
        (proccess-file file)))

(defn handle-folder [folder]
  (let [files (.listFiles (io/file folder))]
    (->> files
         (filter utl/vm-file?)
         (map #(proccess-file %))
         utl/join-by-line
         (spit (utl/build-file-path folder)))))

(defn -main [& _args]
  (let [[path] _args
        isFolder? (.isDirectory (io/file path))]
    (if isFolder?
      (do 
        (reset! folder? true)
        (handle-folder path))
      (handle-file path))))


; (def test-file "../../../MemoryAccess/BasicTest/BasicTest.vm")
; (def test-file-1 "../../../StackArithmetic/SimpleAdd/SimpleAdd.vm")

; (def test-file "../../target/StaticTest.vm")

; (-main test-file-1)

; (->> test-file
;      slurp
;      proccess-file-content
;      (spit "test.asm"))

; (let [[file] *command-line-args*]
;   (println "File:" (slurp file)))


