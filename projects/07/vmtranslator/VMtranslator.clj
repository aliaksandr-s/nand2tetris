(ns translator.utils
  (:require [clojure.string :as str]))

(defn empty-or-comment?
  [str]
  (or (str/blank? str)
      (str/starts-with? str "//")))

(defn split-by-line [str]
  (str/split str #"\r\n"))

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

(ns translator.operations
  (:require [translator.utils :as utl]))

; (def *command-line-args* ["../../../MemoryAccess/BasicTest/BasicTest.vm"])

(def label-id (atom 0))
(def temp-base-addr 5)
(def file-name (-> (first *command-line-args*)
                   utl/get-file-name))

(defn generate-label-id [name]
  (swap! label-id inc)
  (str name "." @label-id))

(defn label [id]
  (str "(" id ")"))

(defn jump-to [label]
  [(str "@" label)
   "0;JMP"])

(defn SP++ []
  ["@SP"
   "M=M+1"])

(defn SP-- []
  ["@SP"
   "M=M-1"])

(defn setSP [val neg?]
  [(str "@" val)
   (if neg? "D=-A" "D=A")
   "@SP"
   "A=M"
   "M=D"])

(defn SP->D []
  ["A=M"
   "D=M"])

(defn get-pointer-addr [val]
  (cond (= val "0") "THIS"
        (= val "1") "THAT"))

(defn get-addr [type val]
  (let [addresses {:static (str "@" file-name "." val)
                   :constant (str "@" val)}]
    ((keyword type) addresses)))

(defn calc-local-addr [reg val]
  (if (= val "0")
    [reg
     "A=M"]
    [reg
     "A=M"
     (repeat (read-string val) "A=A+1")]))

(defn pop [type val]
  (let [types {:static   (str "@" file-name "." val)
               :local    (calc-local-addr "@LCL" val)
               :argument (calc-local-addr "@ARG" val)
               :this     (calc-local-addr "@THIS" val)
               :that     (calc-local-addr "@THAT" val)
               :temp     (str "@" (+ temp-base-addr (read-string val)))
               :pointer  (str "@" (get-pointer-addr val))
               }]
    [(SP--)
     (SP->D)
     ((keyword type) types)
     "M=D"]))

(defn push [type val]
  (let [types {:static   [(str "@" file-name "." val)
                          "A=M"
                          "D=A"]
               :constant [(str "@" val) 
                          "D=A"]
               :local    [(calc-local-addr "@LCL" val)
                          "D=M"]
               :argument [(calc-local-addr "@ARG" val)
                          "D=M"]
               :this     [(calc-local-addr "@THIS" val)
                          "D=M"]
               :that     [(calc-local-addr "@THAT" val)
                          "D=M"]
               :temp     [(str "@" (+ temp-base-addr (read-string val)))
                          "D=M"]
               :pointer  [(str "@" (get-pointer-addr val))
                          "D=M"] 
               }]
    [((keyword type) types)
     "@SP"
     "A=M"
     "M=D"
     (SP++)]))

(defn one-sign-op [type]
  (let [operations {:neg "M=-M"
                    :not "M=!M"}]
    [(SP--)
     (SP->D)
     (type operations)
     (SP++)]))

(defn two-sign-op [type & save-result?]
  (let [operations {:sub "M=M-D"
                    :add "M=D+M"
                    :and "M=M&D"
                    :or  "M=M|D"}]
    [(SP--)
     (SP->D)
     (SP--)
     "A=M"
     (type operations)
     (if save-result? "D=M" [])
     (SP++)]))

(defn eq-op [type]
  (let [true-id (generate-label-id "TRUE")
        end-id (generate-label-id "END")
        operations {:eq "D;JEQ"
                    :gt "D;JGT"
                    :lt "D;JLT"}]
    [(two-sign-op :sub true) ; compute the difference and save result to D
     (SP--)         
     (str "@" true-id)  ; load TRUE address
     (type operations) ; jump to label by condition
     (setSP "0" false)  ; write false (0)
     (jump-to end-id)
     (label true-id)
     (setSP "1" true)   ; write true (-1)
     (label end-id)
     (SP++)]))

(ns translator.parser
  (:require [translator.operations :as op]))

(defmulti parse (fn [cmd] (first cmd)))

(defmethod parse "push"
  [[cmd type val]] (op/push type val))

(defmethod parse "pop"
  [[cmd type val]] (op/pop type val))

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

; (get-file-name "../../../MemoryAccess/BasicTest/BasicTest.vm")
(ns translator.main
  (:require [translator.utils :as utl]
            [translator.parser :refer [parse]]))

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
  (let [[file] *command-line-args*]
    (->> file 
         slurp
         proccess-file-content
         (spit (utl/build-file-path file)))))


; (def test-file "../../../MemoryAccess/BasicTest/BasicTest.vm")
; (def test-file-1 "../../../StackArithmetic/SimpleAdd/SimpleAdd.vm")

; (-main test-file-1)

; (->> test-file-1
;      slurp
;      proccess-file-content
;      (spit "test.asm"))

; (let [[file] *command-line-args*]
;   (println "File:" (slurp file)))


(ns user (:require [translator.main])) (apply translator.main/-main *command-line-args*)
