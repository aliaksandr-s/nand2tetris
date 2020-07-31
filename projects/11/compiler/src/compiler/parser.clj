(ns compiler.parser
  (:require [compiler.utils :as utl]
            [compiler.symbol-table :as s-tbl]
            [compiler.vm-writer :as vm-w]
            [clojure.string :as str]))


; cursor helpers

(defn advance! [tokens]
  (let [cursor (first @tokens)]
   (reset! tokens (rest @tokens))
   cursor))

(defn move-cursor! [tokens] 
  (reset! tokens (rest @tokens)))

(defn get-current [tokens] 
  (first @tokens))

(defn peek-next [tokens]
  (second @tokens))

; ************



; predicates

(defn subroutine-dec? [token]
  (some #(= % (:value token)) ["function" "method" "constructor"]))

(defn op? [token]
  (contains? #{"+" "-" "*" "/" "&" "|" "<" ">" "="} (:value token)))

(defn unary-op? [tokens-atom]
  (contains? #{"~" "-"} (:value (get-current tokens-atom))))

(defn subroutine-call? [tokens-atom]
  (let [next-value (:value (peek-next tokens-atom))] 
    (if (or (= next-value "(")
            (= next-value ".")) 
      true 
      false)))

(defn var-and-expression? [tokens-atom]
  (let [next-value (:value (peek-next tokens-atom))]
    (= next-value "[")))

(defn expression? [tokens-atom]
  (= (:value (get-current tokens-atom)) "("))

(defn class-var-dec? [token]
  (contains? #{"static" "field"} (:value token)))

; *******


(declare compile-subroutine-dec)
(declare compile-subroutine-body)
(declare compile-subroutine-call)
(declare compile-statements)
(declare compile-expression)


; global 

(def class-name (atom ""))
(def var-type (atom ""))
(def while-counter (atom 0))
(def if-counter (atom 0))


(def operations-map 
  {
   "+" "add"
   "-" "sub"
   "&" "and"
   "|" "or"
   "<" "lt"
   ">" "gt"
   "=" "eq"
   })



(defn compile-var [w tokens-atom]
  (println "compile var")

  (move-cursor! tokens-atom)                                         ; var
  (reset! var-type (:value (get-current tokens-atom)))               ; type
  (move-cursor! tokens-atom)

  (let [cur-token (atom (get-current tokens-atom))]
    (while (not= (:value @cur-token) ";")
      (s-tbl/add (:value @cur-token) @var-type "local")               ; varName
      (move-cursor! tokens-atom)

      (when (= (:value (get-current tokens-atom)) ",")
        (move-cursor! tokens-atom))                                 ; ,
      (reset! cur-token (get-current tokens-atom)))

    (move-cursor! tokens-atom) ; ;

    (when (= (:value (get-current tokens-atom)) "var") 
      (compile-var w tokens-atom))))

(defn compile-term [w tokens-atom]
  (println "compile term")
  (println (get-current tokens-atom))

  (cond 
    (= (:type (get-current tokens-atom)) :integerConstant)
    (do (vm-w/write-push w "constant" (:value (get-current tokens-atom)))
        (move-cursor! tokens-atom))


    (= (:type (get-current tokens-atom)) :stringConstant)
    (let [str-val-wrapped (:value (get-current tokens-atom))
          str-val (str/replace (subs str-val-wrapped 1 (- (count str-val-wrapped) 1)) #"(\s;\s)" ";")] 
      (vm-w/write-push w "constant" (count str-val))
      (vm-w/write-call w "String.new" 1)
      (doseq [str-el str-val]
        (vm-w/write-push w "constant" (int str-el))
        (vm-w/write-call w "String.appendChar" 2))
      (move-cursor! tokens-atom))


    (unary-op? tokens-atom)
    (let [op (:value (get-current tokens-atom))           ; ~ | -
          op-map {"~" "not"
                  "-" "neg"}] 
      (move-cursor! tokens-atom)                               
      (compile-term w tokens-atom)
      (vm-w/write-arithmetic w (get op-map op)))


    (contains? #{"false" "null"} (:value (get-current tokens-atom)))
    (do (vm-w/write-push w "constant" 0)
        (move-cursor! tokens-atom))


    (= "true" (:value (get-current tokens-atom)))
    (do 
      (vm-w/write-push w "constant" 0)
      (vm-w/write-arithmetic w "not")

      ; (vm-w/write-push w "constant" 1)
      ; (vm-w/write-arithmetic w "neg")

      (move-cursor! tokens-atom))


    (= "this" (:value (get-current tokens-atom)))
    (do (vm-w/write-push w "pointer" 0)
        (move-cursor! tokens-atom))


    (expression? tokens-atom)
    (do 
      (move-cursor! tokens-atom)                               ; (
      (compile-expression w tokens-atom)
      (move-cursor! tokens-atom))                               ; )

    (subroutine-call? tokens-atom) 
    (compile-subroutine-call w tokens-atom)

    (var-and-expression? tokens-atom) 
    (do 
      (let [value (:value (get-current tokens-atom))]
        (move-cursor! tokens-atom)                                 ; varName
        (move-cursor! tokens-atom)                                 ; [

        (compile-expression w tokens-atom)

        (vm-w/write-push w (s-tbl/kind-of value) (s-tbl/index-of value))
        (vm-w/write-arithmetic w "add")
        (vm-w/write-pop w "pointer" 1)
        (vm-w/write-push w "that" 0))

      (move-cursor! tokens-atom))                                 ; ]

    :else 
    (let [smbl (:value (get-current tokens-atom))
          kind (s-tbl/kind-of smbl)
          index (s-tbl/index-of smbl)]
      (vm-w/write-push w kind index)
      (move-cursor! tokens-atom))))


(defn compile-expression [w tokens-atom]
  (println "compile expression")
  (compile-term w tokens-atom) ; term

  (when (op? (get-current tokens-atom))
    (println "op term")

    (let [op (:value (get-current tokens-atom))]
      (move-cursor! tokens-atom)
      (compile-term w tokens-atom)

      (cond
        (= op "*") (vm-w/write-call w "Math.multiply" 2)
        (= op "/") (vm-w/write-call w "Math.divide" 2)
        :else (vm-w/write-arithmetic w (get operations-map op))))
    ))


(defn compile-expression-list [w tokens-atom extra-arg?]
  (println "compile expression list")

  (let [cur-token  (atom (get-current tokens-atom))
        args-count (atom (if extra-arg? 1 0))]
    (while (not= (:value @cur-token) ")")
      (println "expresion list - expression")
      ; (println (get-current tokens-atom))

      (when (= (:value @cur-token) ",")
        (move-cursor! tokens-atom)) ; ,

      (compile-expression w tokens-atom)
      (swap! args-count inc)
      (reset! cur-token (get-current tokens-atom)))
    
    @args-count 
    ))



(defn compile-subroutine-call [w tokens-atom]
  (println "compile subroutine call")
  (if (= (:value (peek-next tokens-atom)) ".")
    (let [subroutine-name (str (:value (advance! tokens-atom))     ; className | varName
                               (:value (advance! tokens-atom))     ; .                   
                               (:value (advance! tokens-atom)))]   ; subroutineName      

      (move-cursor! tokens-atom)                                      ; (
      (let [args-count (compile-expression-list w tokens-atom false)] ; expressions
        (vm-w/write-call w subroutine-name args-count))    
      (move-cursor! tokens-atom))                                     ; )

    (let [subroutine-name (:value (advance! tokens-atom))] ; subroutineName

      (move-cursor! tokens-atom)                                     ; (
      (let [args-count (compile-expression-list w tokens-atom true)] ; expressions
        (vm-w/write-push w "pointer" 0)
        (vm-w/write-call w (str @class-name "." subroutine-name) args-count))    
      (move-cursor! tokens-atom))))                                  ; )
    

; STATEMENTS

(defn compile-let [w tokens-atom]
  (println "compile let")
  (println (get-current tokens-atom))

  (move-cursor! tokens-atom)               ; let

  (let [value (:value (get-current tokens-atom))] 

    (move-cursor! tokens-atom)               ; val

    ;[ expression ]
    (if (not= (:value (get-current tokens-atom)) "=")
      (do
       (move-cursor! tokens-atom) ;[

       (compile-expression w tokens-atom)
       (vm-w/write-push w (s-tbl/kind-of value) (s-tbl/index-of value))
       (move-cursor! tokens-atom) ;]

       (vm-w/write-arithmetic w "add")
       (println (str "->" (get-current tokens-atom)))
       (move-cursor! tokens-atom) ; = 

       (compile-expression w tokens-atom)
       (vm-w/write-pop w "temp" 0)
       (vm-w/write-pop w "pointer" 1)
       (vm-w/write-push w "temp" 0)
       (vm-w/write-pop w "that" 0))
      
      (do
        ; = 
        (move-cursor! tokens-atom)          
        (compile-expression w tokens-atom)
        (vm-w/write-pop w (s-tbl/kind-of value) (s-tbl/index-of value)))) )


  (move-cursor! tokens-atom)) ; ;


(defn compile-do [w tokens-atom]
  (println "compile do")
  (move-cursor! tokens-atom) ; do

  (compile-subroutine-call w tokens-atom) 
  (vm-w/write-pop w "temp" 0) ; ignore returning result

  (move-cursor! tokens-atom) ; ;
  )


(defn compile-while [w tokens-atom]
  (println "compile while")

  (let [l1 (str "WHILE_EXP" @while-counter)
        l2 (str "WHILE_END" @while-counter)]

    (swap! while-counter inc)

    (vm-w/write-label w l1)            ; l1
    (move-cursor! tokens-atom)         ; while

    ; ( expression )
    (move-cursor! tokens-atom)
    (compile-expression w tokens-atom)
    (vm-w/write-arithmetic w "not")
    (vm-w/write-if w l2)               ; if go-to l2
    (move-cursor! tokens-atom)

    ; { statements }
    (move-cursor! tokens-atom)
    (compile-statements w tokens-atom)
    (vm-w/write-go-to w l1)           ; go-to l1
    (vm-w/write-label w l2)           ; l2

    (move-cursor! tokens-atom))) ;


(defn compile-if [w tokens-atom]
  (println "compile if")

  (let [l-true  (str "IF_TRUE"   @if-counter)
        l-false (str "IF_FALSE"  @if-counter)
        l-end   (str "IF_END"    @if-counter)]

    (swap! if-counter inc)

    (move-cursor! tokens-atom) ; if

    ; ( expression )
    (move-cursor! tokens-atom)
    
    (compile-expression w tokens-atom)
    (vm-w/write-if w l-true)
    (vm-w/write-go-to w l-false)
    (vm-w/write-label w l-true)

    (move-cursor! tokens-atom)

    ; { statements }
    (move-cursor! tokens-atom)
    (compile-statements w tokens-atom)
    (vm-w/write-go-to w l-end)
    (move-cursor! tokens-atom)

    ; else { statements }
    (when (= (:value (get-current tokens-atom)) "else") 
      (println "compile else")

      (vm-w/write-label w l-false)

      (move-cursor! tokens-atom) ; else
      (move-cursor! tokens-atom) ; {
      (compile-statements w tokens-atom)
      (move-cursor! tokens-atom)) ; }

    (vm-w/write-label w l-end)))


(defn compile-return [w tokens-atom]
  (println "compile return")
  (move-cursor! tokens-atom)                                     ; return

  (let [cur-token (atom (get-current tokens-atom))]
    (if (not= (:value @cur-token) ";")
      (do
        (compile-expression w tokens-atom)                       ; expression?
        (reset! cur-token (get-current tokens-atom)))
      (vm-w/write-push w "constant" 0)))

  (move-cursor! tokens-atom)                                     ; ;
  (vm-w/write-return w))


(defn compile-statements [w tokens-atom]
  (println "compile statements")

  (let [cur-token (atom (get-current tokens-atom))]
    (while (not= (:value @cur-token) "}") 
      (cond 
        (= (:value @cur-token) "let") (compile-let w tokens-atom)
        (= (:value @cur-token) "do") (compile-do w tokens-atom)
        (= (:value @cur-token) "while") (compile-while w tokens-atom)
        (= (:value @cur-token) "if") (compile-if w tokens-atom)
        (= (:value @cur-token) "return") (compile-return w tokens-atom))
      
      (reset! cur-token (get-current tokens-atom)))
    ))

; ********


(defn compile-parameter-list [w tokens-atom method?]
  (println "compile parameter list")

  (when method?
    (s-tbl/add "this" @class-name "argument"))

  (let [cur-token (atom (get-current tokens-atom))] ; type
    (while (not= (:value @cur-token) ")")

      (move-cursor! tokens-atom)
      (s-tbl/add (:value (get-current tokens-atom)) (:value @cur-token) "argument") ;varName
      (move-cursor! tokens-atom)

      (when (= (:value (get-current tokens-atom)) ",") ; ,
        (move-cursor! tokens-atom))

      (reset! cur-token (get-current tokens-atom)))))


(defn compile-subroutine-body [w tokens-atom func-name kind]
  (println "compile subroutine body")
  (move-cursor! tokens-atom)                               ;{

  (let [cur-token (atom (get-current tokens-atom))]
    (when (= (:value @cur-token) "var") 
      (compile-var w tokens-atom))

    ; write function name after adding all locals to the table
    (vm-w/write-function w (str @class-name "." @func-name) (s-tbl/var-count "local"))

    (when (= kind "constructor")
      (let [field-count (s-tbl/var-count "field")]
        (when (> field-count 0)
          (vm-w/write-push w "constant" field-count)))
      (vm-w/write-call w "Memory.alloc" 1)
      (vm-w/write-pop w "pointer" 0))

    (when (= kind "method")
      (vm-w/write-push w "argument" 0)
      (vm-w/write-pop w "pointer" 0))

    (compile-statements w tokens-atom)
    
    (move-cursor! tokens-atom)                               ;}
    ))


; (def method? (atom false))
; (def func-name (atom ""))

(defn compile-subroutine-dec [w tokens-atom]
  (println "compile subroutine declaration")
  (reset! s-tbl/subroutine-table [])

  (let [method? (atom false)
        func-name (atom "")
        kind (:value (get-current tokens-atom))]

    (if (= kind "method") 
      (reset! method? true) 
      (reset! method? false))

    (move-cursor! tokens-atom)                               ; function | method | constructor
    (move-cursor! tokens-atom)                               ; void | type
    (reset! func-name (:value (get-current tokens-atom)))
    (move-cursor! tokens-atom)                               ; functionName

    (move-cursor! tokens-atom)                               ;(
    (compile-parameter-list w tokens-atom @method?)          ; parameter List
    (move-cursor! tokens-atom)                               ;)

    (compile-subroutine-body w tokens-atom func-name kind)        ; subroutine Body

    ; print table for debug
    ; (utl/write-line w "Subroutine Table")
    ; (utl/print-table w @s-tbl/subroutine-table)
    ; (utl/write-line w "---------")

    (when (subroutine-dec? (get-current tokens-atom))
      (compile-subroutine-dec w tokens-atom))))



(def class-type (atom ""))
(def class-kind (atom ""))

(defn compile-class-var [w tokens-atom]
  (println "compile class var")

  (reset! class-kind (:value (get-current tokens-atom)))
  (move-cursor! tokens-atom) ; kind: static | field

  (reset! class-type (:value (get-current tokens-atom)))
  (move-cursor! tokens-atom) ; type: int, ..

  (s-tbl/add (:value (get-current tokens-atom)) @class-type @class-kind)
  (move-cursor! tokens-atom) ; varName

  (let [cur-token (atom (get-current tokens-atom))]
    (while (= (:value @cur-token) ",")
      (move-cursor! tokens-atom) ; ,

      (s-tbl/add (:value (get-current tokens-atom)) @class-type @class-kind)
      (move-cursor! tokens-atom) ; varName

      (reset! cur-token (get-current tokens-atom))))

    (move-cursor! tokens-atom)) ; ;

(defn compile-class [w tokens-atom]
  (println "compile class")
  (reset! s-tbl/class-table [])

  (move-cursor! tokens-atom)                               ; class
  (reset! class-name (:value (get-current tokens-atom)))
  (move-cursor! tokens-atom)                               ; className
  (move-cursor! tokens-atom)                               ; {

  (let [cur-token (atom (get-current tokens-atom))]
    (while (class-var-dec? @cur-token)
      (compile-class-var w tokens-atom)      
      (reset! cur-token (get-current tokens-atom)))) 

  (compile-subroutine-dec w tokens-atom)                   ; subroutineDec
  (move-cursor! tokens-atom)                               ; }

  ; test symbol table
  ; (utl/write-line w "Class Table")
  ; (utl/print-table w @s-tbl/class-table)
  ; (utl/write-line w "---------")
  )




(defn parse [w tokens]
  (let [tokens-atom (atom tokens)]
    (compile-class w tokens-atom)))


