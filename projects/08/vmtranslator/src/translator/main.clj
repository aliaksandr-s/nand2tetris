(ns translator.main
  (:require [translator.utils :as utl]
            [translator.parser :refer [parse]]
            [translator.operations :refer [file-name]])
  (:gen-class))

(defn proccess-file-content [content]
  (->> 
    content 
    utl/split-by-line
    (remove utl/empty-or-comment?)
    (map utl/split-by-whitespace)
    (map parse)
    flatten
    utl/join-by-line))

(defn -main [& _args]
; - (let [[file] *command-line-args*]
  (let [[file] _args]
    (reset! file-name file)
    (->> file 
         slurp
         proccess-file-content
         (spit (utl/build-file-path file)))))


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


