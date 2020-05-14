(ns translator.operations
  (:require [translator.utils :as utl]))

; (def *command-line-args* ["../../../MemoryAccess/BasicTest/BasicTest.vm"])

(def file-name (atom ""))
(def func-name (atom ""))
(def label-id (atom 0))
(def temp-base-addr 5)

; (def file-name (-> (first *command-line-args*)
;                    utl/get-file-name))

; (def file-name "StaticTest") ; quick workaround to make compilation work

(defn generate-label-id [name]
  (swap! label-id inc)
  (str name "." @label-id))

(defn label-handle [id]
  (str "(" @func-name "$" id ")" ))

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

(defn setTempR 
  "Set a D value to one of temporary registers (R13-R15)"
  [n]
  [(str "@R" n)
   "M=D"])

(defn goToTempR 
  "Get a value from temp registers (R13-R15)"
  [n]
  [(str "@R" n)])

(defn SP->D []
  ["A=M"
   "D=M"])

(defn goto [id]
  [(str "@" @func-name "$" id)
   "0;JMP"])

(defn if-goto [id]
  [(SP--)
   (SP->D)
   (str "@" @func-name "$" id)
   "D;JGT"])

(defn get-pointer-addr [val]
  (cond (= val "0") "THIS"
        (= val "1") "THAT"))

(defn get-addr [type val]
  (let [addresses {:static (str "@" @file-name "." val)
                   :constant (str "@" val)}]
    ((keyword type) addresses)))

(defn calc-local-addr [reg val]
  (if (= val "0")
    [reg
     "A=M"]
    [reg
     "A=M"
     (repeat (read-string val) "A=A+1")]))

(defn pop-op [type val]
  (let [types {:static   (str "@" @file-name "." val)
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
  (let [types {:static   [(str "@" @file-name "." val)
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

(defn function [name n]
  (reset! func-name name)
  [(label name)
   (repeat (read-string n) (push :constant "0"))])

(defn restoreCaller 
  "// THAT = *(endFrame - 1)
   // THIS = *(endFrame - 2)
   // ARG  = *(endFrame - 3)
   // LCL  = *(endFrame - 4)"
  [label]
  ["@LCL"
   "AM=M-1"
   "D=M"
   label
   "M=D"])

(defn return []
  ["// endFrame = LCL"
   "@LCL"
   "D=M"

   "// retAddr = *(endFrame - 5)"
   "@5"
   "A=D-A"
   "D=M"
   (setTempR 13)

   "// *ARG = pop()"
   (SP--)
   (SP->D)
   "@ARG"
   "A=M"
   "M=D"

   "// SP   = ARG + 1"
   "D=A"
   "@SP"
   "M=D"
   (SP++)

   "// restore caller"
   (restoreCaller "@THAT")
   (restoreCaller "@THIS")
   (restoreCaller "@ARG")
   (restoreCaller "@LCL")

   "// goto retAddr"
   (goToTempR 13)
   "A=M"
   "0;JMP"
   ])


(defn push-reg [reg]
  [reg
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   (SP++)])

(defn call [f-name arg-n]
  (let [ret-id (generate-label-id "RET")]
    ["// SP -> R13"
     "@SP"
     "D=M"
     (setTempR 13)

     "// push @RET"
     (str "@" ret-id)
     "D=A"
     "@SP"
     "A=M"
     "M=D"
     (SP++)

     "// push LCL, ARG, THIS, THAT"
     (push-reg "@LCL")
     (push-reg "@ARG")
     (push-reg "@THIS")
     (push-reg "@THAT")

     "// ARG = SP - 5 - arg-n"
     (goToTempR 13)
     "D=M"
     (str "@" arg-n)
     "D=D-A"
     "@ARG"
     "M=D"

     "// LCL = SP"
     "@SP"
     "D=M"
     "@LCL"
     "M=D"

     "// goto f-name"
     (str "@" f-name)
     "0;JMP"
     (label ret-id)
     ]))


(defn init []
  ["@256"
   "D=A"
   "@SP"
   "M=D"
   (call "Sys.init" "0")
   "0;JMP"])
