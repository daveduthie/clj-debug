(ns clj-debug.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [doc source apropos]]))

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defn lazy? [form]
  (contains? #{#'for} (resolve (first form))))

(defn elide-dbg-syms [exp]
  ;; (prn :type exp (type exp))
  (cond
    (not (seqable? exp))          exp
    (and (seqable? exp)
         (= (first exp)
            'clj-debug.core/dbg)) (map elide-dbg-syms (second exp))
    (vector? exp)                 (mapv elide-dbg-syms exp)
    (string? exp)                 exp
    :else                         (map elide-dbg-syms exp)))

;; TODO: replace nested expresssions with their values when printing?
(defmacro dbg
  [form]
  (let [ctx (let [;; symbols (keys &env)]
                  symbols (filter #(not (re-find #"_" (str %))) (keys &env))] ; An ugly hack
              (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols))

        elided (elide-dbg-syms form)]

    (if-not (lazy? form)
      `(let [ret# (contextual-eval ~ctx '~form)] ; nested exps eval'ed here
         (println ">")                           ; how to capture?
         (read-line)
         (prn :ctx ~ctx)
         (prn '~elided :=> ret#)
         ret#)
      `(let [ret# (contextual-eval ~ctx '~form)]
         (println ">")
         (read-line)
         (prn :ctx ~ctx)
         (prn '~elided :=> "Lazy Seq")
         ret#))))

(declare attach-dbg)

;; TODO: complain on malformed bindings
(defn debug-bindings [bindings]
  (->> bindings
       (partition-all 2)
       (mapcat (fn [[lvar expr]] [lvar (attach-dbg expr)]))
       (into [])))

;; TODO: add more special forms
(defn attach-dbg [form]
  (if-not (seq? form)
    form
    (condp #(contains? %1 %2) (resolve (first form))
      #{#'let #'doseq #'for} (let [[sym bind & body] form]
                               `(dbg (~sym ~(debug-bindings bind)
                                      ~@(map attach-dbg body))))
      `(dbg ~(map attach-dbg form)))))

(defmacro instrument
  [& body]
  `(do ~@(map attach-dbg body)))

(defn adder
  [a b]
  (instrument
   (str :black
        "_"
        (+ a b)
        "-_-"
        (reduce *' (repeat a b))
        " "
        (let [fruit :banana!]
          (str "I'd like a " (name fruit)))
        " "
        (apply str
               (for [x (range 10)]
                 (str "I'm holding " x " bananas! "))))))

(defn -main [& args]
  (adder 3 4))
