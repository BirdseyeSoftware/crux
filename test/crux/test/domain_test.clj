(ns crux.test.domain-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [crux.example.tickets :as tickets]
            [crux.domain :as domain]
            [crux.internal.keys :refer :all]
            [crux.util :refer [map-over-keys submap?]])
  (:require [clojure.java.io :refer [input-stream reader]])
  (:import [crux.domain DomainSpec])
  (:import [clojure.lang LineNumberingPushbackReader])
  (:import [java.io File]))


;; Stolen from clojure.contrib.with-ns
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


(deftest test-domain-is-built-correctly
  (let [tickets-domain (tickets/build-test-domain-spec)]
    (is (instance? DomainSpec tickets-domain))))


(deftest test-reifier-is-building-domain-correctly
  (let [ticket-domain (tickets/build-test-domain-spec)]
    (with-test-ns tickets-test
      (require '[crux.reify :refer (reify-domain-spec!)])
      (require '[crux.example.tickets :as tickets])
      (-> (tickets/build-test-domain-spec)
          reify-domain-spec!))))

(defn -read-file
  ;; see https://github.com/jonase/kibit/blob/master/src/kibit/check.clj
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
        (cons form (-read-file r))))))

(defn read-file [file-or-path]
  (-read-file
   (LineNumberingPushbackReader. (reader file-or-path))))

(defn get-domain-data-readers [domain-spec prefix]
  (map-over-keys
   #(symbol (format "%s/%s" prefix %))
   (:crux.reify/constructors domain-spec)))

(defn assert-reduction
  ([reducer-fn init-st events expected-final-st]
     (let [final (reduce reducer-fn init-st events)]
       (is (submap? expected-final-st final)))))

(deftest read-events-from-file
  (let [fpath "test/crux/example/tickets_sample_events1.clj"
        domain-spec (tickets/build-reified-test-domain-spec)
        data-readers (get-domain-data-readers domain-spec 'tickets)
        ]
    (binding [*data-readers* data-readers]
      (doseq [test-map (read-file fpath)]
        (let [{entity-symbol :entity
               :keys [name initial events expected]} test-map  
              entity-ctor (get-in domain-spec [:crux.reify/constructors entity-symbol])
              entity-reducer (get-in domain-spec [:crux.reify/reducers entity-symbol])
              entity0 (or initial (entity-ctor {}))]
          (is (contains? (meta entity0) :crux/properties), (meta entity0))
          (is (nil? (:crux/properties entity0)))
          (assert-reduction entity-reducer entity0 events expected))))))