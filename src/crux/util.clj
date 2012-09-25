(ns crux.util
  (:require [clojure.set :as set]
            [clojure.java.io :refer [reader]])

  (:require [slingshot.slingshot :refer [throw+]])

  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io File]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Structure utils

(defn submap? [m superm]
  (set/subset? (set (seq m)) (set (seq superm))))

(defn map-over-values [modifier-fn m]
  (into {} (for [[k v] m]
             [k (modifier-fn v)])))

(defn map-over-keys [modifier-fn m]
  (into {} (for [[k v] m]
             [(modifier-fn k) v])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exception utils

(defn throw-already-declared-error [declaration-type sym owner-type owner]
  (throw+
   (java.lang.IllegalArgumentException.
    (format "Crux: %s '%s' already declared for %s '%s'"
            declaration-type sym owner-type owner))))

(defn throw-reduce-forms-required-error [event-symbol]
  (throw+
   (java.lang.IllegalArgumentException.
    (format "Crux: %s '%s' requires a reduce form"
            event-symbol))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; multimethod utils

(defn create-multi [multifn-name dispatch-fn]
  (new clojure.lang.MultiFn (name multifn-name) dispatch-fn
       :default #'clojure.core/global-hierarchy))

(defn add-method-to-multi
  [multifn dispatch-val fn]
  (. multifn addMethod dispatch-val fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defrecord extensions

(defmacro defrecord-keep-meta [name [& fields-with-meta] & body]
  `(do
     (def ~(symbol (format "-%s-fields-with-meta" name)) '~fields-with-meta)
     (defrecord ~name [~@fields-with-meta]
       ~@body)))

(defn defrecord-dynamically [record-symbol fields]
  (let [cls (eval `(defrecord-keep-meta ~record-symbol ~fields))
        ctor-symbol (symbol (str "map->" record-symbol))]
    {:record-class cls
     :record-symbol record-symbol
     :record-ctor (eval `~ctor-symbol)
     :record-ctor-symbol ctor-symbol}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Form reader

;;; stolen from:
;;; https://github.com/jonase/kibit/blob/master/src/kibit/check.clj

(defn -read-forms-from-reader
  "Gen a lazy sequence of top level forms from a LineNumberingPushbackReader"
  [^LineNumberingPushbackReader r]
  (lazy-seq
    (let [form (try
                 (read r false ::eof)
                 (catch Exception e
                   (throw (Exception.
                           (str "reader crashed"
                                (.getMessage e)) e))))]
      (when-not (= form ::eof)
        (cons form (-read-forms-from-reader r))))))

(defn read-forms-from-file [file-or-path]
  (-read-forms-from-reader
   (LineNumberingPushbackReader. (reader file-or-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros utils

;;; some of them stolen from clojure.contrib.with-ns


(defn type-symbol [instance]
  (symbol (.getSimpleName (type instance))))

(defn unquoted?
  "Check if the form is syntax-unquoted: like ~form"
  [form]
  (and (seq? form)
       (= (first form) 'clojure.core/unquote)))

(defn -quote-or-unquote-fields-form
  "Allow fields form to be either a vector of forms, or a var that
has a reference to a vector of forms."
  [fields owner]
  (cond
    (vector? fields) `'[~@fields]
    (unquoted? fields) (second fields)
    (= fields 'entity-fields) 'entity-fields
    :else (throw+
           (java.lang.IllegalArgumentException.
            (format "Crux: Illegal fields definition '%s' for %s"
                    fields owner)))))

(defn eval-with-meta [form meta-info]
  (with-meta (eval form)
    (merge {:crux/generated-code form}
           meta-info)))

(defmacro with-ns
  "Evaluates body in another namespace. ns is either a namespace
object or a symbol. This makes it possible to define functions in
namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     ~@(map (fn [form] `(eval '~form)) body)))


(defmacro with-test-ns
  "Evaluates body in an anonymous namespace, which is then immediately
removed. The temporary namespace will 'refer' clojure.core."
  [name & body]
  `(do
     (when-let [test-ns# (find-ns '~name)]
       (remove-ns 'test-ns#))

     (create-ns '~name)
     (let [test-ns# (find-ns '~name)]
       (let [result# (with-ns '~name
                       (clojure.core/refer-clojure)
                       ~@body)]
         result#))))
