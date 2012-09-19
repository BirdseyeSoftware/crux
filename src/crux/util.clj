(ns crux.util
  )

;; (defn -mkmethod
;;   [multifn dispatch-val method]
;;   (. multifn ;; (with-meta multifn {:tag 'clojure.lang.MultiFn})
;;      addMethod dispatch-val method))
;; (def ticket-evs (get-in reified-tickets-domain [:entities 'Ticket
;;                                                 :events]))
;; (defmulti ticket-red (fn [ent ev] (type ev)))
;; (doall (doseq [ev-spec (vals ticket-evs)]
;;          (pprint (:record-class ev-spec))

         ;; (eval `(defmethod ticket-red ~(symbol (:record-symbol ev-spec))
         ;;          [ent# ev#] (~(REDUCERs ev-spec) ent# ev#)))

         ;; (-mkmethod ticket-red (:record-class ev-spec) (REDUCERs ev-spec))
         ;; ))


(defn eval-with-meta [form meta-info]
  (with-meta (eval form)
    (merge {:crux/generated-code form}
           meta-info)))

(defn unquoted?
  "Check if the form is syntax-unquoted: like ~form"
  [form]
  (and (seq? form)
       (= (first form) 'clojure.core/unquote)))

(defn defrecord-dynamically [record-symbol fields]
  (let [cls (eval `(defrecord ~record-symbol ~fields))
        ctor-symbol (symbol (str "map->" record-symbol))]
    {:record-class cls
     :record-symbol record-symbol
     :record-ctor (eval `~ctor-symbol)
     :record-ctor-symbol ctor-symbol}))

(defmacro defrecord-keep-meta [name [& fields-with-meta] & body]
  `(do
     (def ~(symbol (format "-%s-fields-with-meta" name)) '~fields-with-meta)
     (defrecord ~name [~@fields-with-meta]
       ~@body)))
