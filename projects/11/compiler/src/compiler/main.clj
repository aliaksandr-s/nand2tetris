(ns compiler.main
  (:require [compiler.utils :as utl]
            [clojure.java.io :as io]
            [compiler.tokenizer :refer [tokenize]]
            [compiler.parser :refer [parse]]
            )
  (:gen-class))


(defn proccess-file-test [file]
  (spit (utl/build-file-path file) 
        (->> file slurp tokenize pr-str)))

(defn proccess-file [file]
  (println "proccess-file")
  (with-open [w (clojure.java.io/writer (utl/build-file-path file) :append false)]
    (let [tokens (->> file slurp tokenize)]
      (parse w tokens))))

(defn handle-folder [folder]
  (println "handle-folder")
  (let [files (.listFiles (io/file folder))]
    (doseq [file (filter utl/jack-file? files)]
      (proccess-file file))))

(defn -main [& _args]
  (let [[path] _args
        isFolder? (.isDirectory (io/file path))]
    (if isFolder?
      (handle-folder path)
      (proccess-file path))))

