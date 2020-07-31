(ns compiler.symbol-table)


(def ttt-1 {:name "x" :type "int" :kind "field" :index 0})
(def ttt-2 {:name "y" :type "int" :kind "field" :index 1})
(def ttt-3 {:name "pointCount" :type "int" :kind "static" :index 0})

(def ppp-1 {:name "dx" :type "int" :kind "local" :index 0})
(def ppp-2 {:name "dy" :type "int" :kind "local" :index 1})
(def lll [ttt-1 ttt-2 ttt-3])


(def class-table (atom [ttt-1 ttt-2 ttt-3]))
(def subroutine-table (atom [ppp-1 ppp-2]))

; (def class-table (atom []))
; (def subroutine-table (atom []))

@class-table
@subroutine-table

; filed, static => class
; arg, var      => subroutine

(defn class-scope? [kind]
  (contains? #{"static" "field"} kind))

(defn var-count [kind]
  (let [target-table (if (class-scope? kind) class-table subroutine-table)] 
    (-> (filter #(= (:kind %) kind) @target-table)
        count)))

(defn get-fn [table name] 
  (-> (filter #(= (:name %) name) @table)
      first))

(defn get-from-table 
  ([name key] 
   (if (get-fn subroutine-table name)
     (key (get-fn subroutine-table name)) 
     (key (get-fn class-table name))))
  ([name]
   (if (get-fn subroutine-table name)
     (get-fn subroutine-table name) 
     (get-fn class-table name))))

(defn get-symbol [name] (get-from-table name))
(defn kind-of [name] (get-from-table name :kind))
(defn type-of [name] (get-from-table name :type))
(defn index-of [name] (get-from-table name :index))

; (get-symbol "dx")


(defn add [name type kind]
  (let [target-table (if (class-scope? kind) class-table subroutine-table)]
    (swap! target-table 
           conj 
           {:name name :type type :kind kind :index (var-count kind)})))

; (add "this" "Point" "argument")



; (def var-input
;   '(
;     {:value "{", :type :symbol}
;     {:value "var", :type :keyword}
;     {:value "Array", :type :identifier}
;     {:value "a", :type :identifier}
;     {:value ";", :type :symbol}
;     {:value "var", :type :keyword}
;     {:value "int", :type :keyword}
;     {:value "length", :type :identifier}
;     {:value ";", :type :symbol}
;     {:value "var", :type :keyword}
;     {:value "int", :type :keyword}
;     {:value "i", :type :identifier}
;     {:value ",", :type :symbol}
;     {:value "sum", :type :identifier}
;     {:value ";", :type :symbol}
;     {:value "let", :type :keyword}
;   ))
