(ns compiler.parser
  (:require [compiler.utils :as utl]))


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


(defn compile-var [w tokens-atom]
  (println "compile var")
  (utl/write-line w (utl/make-open-tag "varDec"))

  (let [cur-token (atom (get-current tokens-atom))]
    (while (not= (:value @cur-token) ";")
      (utl/write-line w (utl/make-tag @cur-token))
      (move-cursor! tokens-atom)
      (reset! cur-token (get-current tokens-atom)))

    (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; ;
    (utl/write-line w (utl/make-closing-tag "varDec"))

    (when (= (:value (get-current tokens-atom)) "var") 
      (compile-var w tokens-atom))))


(defn compile-term [w tokens-atom]
  (println "compile term")
  (println (get-current tokens-atom))
  (utl/write-line w (utl/make-open-tag "term"))

  (cond 
    (unary-op? tokens-atom)
      (do 
        (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; ~ | -
        (compile-term w tokens-atom))

    (expression? tokens-atom)
      (do 
        (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; (
        (compile-expression w tokens-atom)
        (utl/write-line w (utl/make-tag (advance! tokens-atom)))) ; (

    (subroutine-call? tokens-atom) 
      (compile-subroutine-call w tokens-atom)

    (var-and-expression? tokens-atom) 
      (do 
        (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; varName
        (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; [
        (compile-expression w tokens-atom)
        (utl/write-line w (utl/make-tag (advance! tokens-atom)))) ; ]

    :else 
      (utl/write-line w (utl/make-tag (advance! tokens-atom))))

  (utl/write-line w (utl/make-closing-tag "term")))


(defn compile-expression [w tokens-atom]
  (println "compile expression")
  (utl/write-line w (utl/make-open-tag "expression"))
  (compile-term w tokens-atom) ; term

  (when (op? (get-current tokens-atom))
    (println "op term")
    (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; op
    (compile-term w tokens-atom))

  (utl/write-line w (utl/make-closing-tag "expression")))


(defn compile-expression-list [w tokens-atom]
  (println "compile expression list")
  (utl/write-line w (utl/make-open-tag "expressionList"))

  (let [cur-token (atom (get-current tokens-atom))]
    (while (not= (:value @cur-token) ")")
      (println "expresion list - expression")
      (println (get-current tokens-atom))

      (when (= (:value @cur-token) ",")
        (utl/write-line w (utl/make-tag (advance! tokens-atom)))) ; ,

      (compile-expression w tokens-atom)
      (reset! cur-token (get-current tokens-atom)))
    
    (utl/write-line w (utl/make-closing-tag "expressionList"))))



(defn compile-subroutine-call [w tokens-atom]
  (println "compile subroutine call")
  (if (= (:value (peek-next tokens-atom)) ".")
    (do
      (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; className | varName
      (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; . 
      (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; subroutineName

      ; ( expressions )
      (utl/write-line w (utl/make-tag (advance! tokens-atom)))
      (compile-expression-list w tokens-atom)
      (utl/write-line w (utl/make-tag (advance! tokens-atom))))
    (do
      (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; subroutineName

      ; ( expressions )
      (utl/write-line w (utl/make-tag (advance! tokens-atom)))
      (compile-expression-list w tokens-atom)
      (utl/write-line w (utl/make-tag (advance! tokens-atom))))
    ))



; STATEMENTS

(defn compile-let [w tokens-atom]
  (println "compile let")
  (utl/write-line w (utl/make-open-tag "letStatement"))

  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; let
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; varName

  ;[ expression ]
  (when (not= (:value (get-current tokens-atom)) "=")
    (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; [
    (compile-expression w tokens-atom)
    (utl/write-line w (utl/make-tag (advance! tokens-atom)))) ; ]

  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; =

  (compile-expression w tokens-atom)

  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; ;
  (utl/write-line w (utl/make-closing-tag "letStatement")))


(defn compile-do [w tokens-atom]
  (println "compile do")
  (utl/write-line w (utl/make-open-tag "doStatement"))
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; do

  (compile-subroutine-call w tokens-atom) 

  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; ;
  (utl/write-line w (utl/make-closing-tag "doStatement")))


(defn compile-while [w tokens-atom]
  (println "compile while")
  (utl/write-line w (utl/make-open-tag "whileStatement"))
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; while

  ; ( expression )
  (utl/write-line w (utl/make-tag (advance! tokens-atom)))
  (compile-expression w tokens-atom)
  (utl/write-line w (utl/make-tag (advance! tokens-atom)))

  ; { statements }
  (utl/write-line w (utl/make-tag (advance! tokens-atom)))
  (compile-statements w tokens-atom)
  (utl/write-line w (utl/make-tag (advance! tokens-atom)))

  (utl/write-line w (utl/make-closing-tag "whileStatement")));


(defn compile-if [w tokens-atom]
  (println "compile if")
  (utl/write-line w (utl/make-open-tag "ifStatement"))
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; if

  ; ( expression )
  (utl/write-line w (utl/make-tag (advance! tokens-atom)))
  (compile-expression w tokens-atom)
  (utl/write-line w (utl/make-tag (advance! tokens-atom)))

  ; { statements }
  (utl/write-line w (utl/make-tag (advance! tokens-atom)))
  (compile-statements w tokens-atom)
  (utl/write-line w (utl/make-tag (advance! tokens-atom)))

  ; else { statements }
  (when (= (:value (get-current tokens-atom)) "else") 
    (println "compile else")
    (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; else
    (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; {
    (compile-statements w tokens-atom)
    (utl/write-line w (utl/make-tag (advance! tokens-atom)))) ; }

  (utl/write-line w (utl/make-closing-tag "ifStatement")))


(defn compile-return [w tokens-atom]
  (println "compile return")
  (utl/write-line w (utl/make-open-tag "returnStatement"))
  (utl/write-line w (utl/make-tag (advance! tokens-atom)))     ; return

  (let [cur-token (atom (get-current tokens-atom))]
    (when (not= (:value @cur-token) ";")
      (compile-expression w tokens-atom)                       ; expression?
      (reset! cur-token (get-current tokens-atom))))

  (utl/write-line w (utl/make-tag (advance! tokens-atom)))     ; ;
  (utl/write-line w (utl/make-closing-tag "returnStatement")))


(defn compile-statements [w tokens-atom]
  (println "compile statements")
  (utl/write-line w (utl/make-open-tag "statements"))

  (let [cur-token (atom (get-current tokens-atom))]
    (while (not= (:value @cur-token) "}") 
      (cond 
        (= (:value @cur-token) "let") (compile-let w tokens-atom)
        (= (:value @cur-token) "do") (compile-do w tokens-atom)
        (= (:value @cur-token) "while") (compile-while w tokens-atom)
        (= (:value @cur-token) "if") (compile-if w tokens-atom)
        (= (:value @cur-token) "return") (compile-return w tokens-atom))
      
      (reset! cur-token (get-current tokens-atom)))
    
    (utl/write-line w (utl/make-closing-tag "statements"))))

; ********


(defn compile-parameter-list [w tokens-atom]
  (println "compile parameter list")
  (utl/write-line w (utl/make-open-tag "parameterList"))

  (let [cur-token (atom (get-current tokens-atom))]
    (while (not= (:value @cur-token) ")")
      (utl/write-line w (utl/make-tag @cur-token))
      (move-cursor! tokens-atom)
      (reset! cur-token (get-current tokens-atom)))
    
    (utl/write-line w (utl/make-closing-tag "parameterList"))))


(defn compile-subroutine-body [w tokens-atom]
  (println "compile subroutine body")
  (utl/write-line w (utl/make-open-tag "subroutineBody"))
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ;{

  (let [cur-token (atom (get-current tokens-atom))]
    (when (= (:value @cur-token) "var") 
      (compile-var w tokens-atom))

    (compile-statements w tokens-atom)
    
    (utl/write-line w (utl/make-tag (advance! tokens-atom))) ;}
    (utl/write-line w (utl/make-closing-tag "subroutineBody"))))



(defn compile-subroutine-dec [w tokens-atom]
  (println "compile subroutine declaration")
  (utl/write-line w (utl/make-open-tag "subroutineDec"))

  (utl/write-line w (utl/make-tag (advance! tokens-atom) )) ; function | method | constructor
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; void | type
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; functionName

  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ;(
  (compile-parameter-list w tokens-atom)                   ; parameter List
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ;)

  (compile-subroutine-body w tokens-atom)                  ; subroutine Body

  (utl/write-line w (utl/make-closing-tag "subroutineDec"))

  (when (subroutine-dec? (get-current tokens-atom))
    (compile-subroutine-dec w tokens-atom)))



(defn compile-class-var [w tokens-atom]
  (println "compile class var")
  (utl/write-line w (utl/make-open-tag "classVarDec"))

  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; static | field
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; type
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; varName

  (let [cur-token (atom (get-current tokens-atom))]
    (while (= (:value @cur-token) ",")
      (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; ,
      (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; varName
      (reset! cur-token (get-current tokens-atom))))

    (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; ;
    (utl/write-line w (utl/make-closing-tag "classVarDec")))


(defn compile-class [w tokens-atom]
  (println "compile class")
  (utl/write-line w (utl/make-open-tag "class"))

  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; class
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; className
  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; {

  (let [cur-token (atom (get-current tokens-atom))]
    (while (class-var-dec? @cur-token)
      (compile-class-var w tokens-atom)      
      (reset! cur-token (get-current tokens-atom)))) 

  (compile-subroutine-dec w tokens-atom)                   ; subroutineDec

  (utl/write-line w (utl/make-tag (advance! tokens-atom))) ; }
  (utl/write-line w (utl/make-closing-tag "class")))



(defn parse [w tokens]
  (let [tokens-atom (atom tokens)]
    (compile-class w tokens-atom)))


