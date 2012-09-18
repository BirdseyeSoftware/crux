(ns crux.reify
  (:require [crux.util :refer [defrecord-dynamically]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Crux Reification code:
;;; abstract-spec -> real records, fns, protos/interfaces, etc.

;;TODO: make this a configurable callback in the entity-map
(defn- -entity-subrecord-symbol [entity-symbol record-type record-symbol]
  (symbol (case record-type
            :events (format "%s%s" entity-symbol record-symbol)
            :commands (format "%s%s" record-symbol entity-symbol))))

(defn reify-event-or-command-record!
  [domain-spec [entity-symbol record-type record-symbol]]
  (let [sub-rec-symbol (-entity-subrecord-symbol
                        entity-symbol record-type record-symbol)
        fields (get-in domain-spec [:entities entity-symbol
                                    record-type record-symbol
                                    :fields])]
    (update-in domain-spec [:entities entity-symbol record-type record-symbol]
               merge
               (defrecord-dynamically sub-rec-symbol fields))))

(defn reify-all-event-or-command-records!
  [domain-spec entity-symbol record-type-key]
  (let [entity-spec (get-in domain-spec [:entities entity-symbol])
        record-symbols (keys (record-type-key entity-spec))
        reduce-inputs (map #(do [entity-symbol record-type-key %])
                           record-symbols)]
    (reduce reify-event-or-command-record! domain-spec reduce-inputs)))

(defn reify-entity-event+command-records! [domain-spec entity-symbol]
  (-> domain-spec
      (reify-all-event-or-command-records! entity-symbol :events)
      (reify-all-event-or-command-records! entity-symbol :commands)))

(defn reify-entity-record! [domain-spec entity-symbol]
  (let [entity-spec (get-in domain-spec [:entities entity-symbol])
        fields (:fields entity-spec)
        properties (:properties entity-spec)
        defrecord-map (defrecord-dynamically entity-symbol fields)
        orig-ctor (:record-ctor defrecord-map)
        crux-meta-fields {:crux/properties properties}
        defrecord-map (assoc defrecord-map
                        :record-ctor
                        (fn [m] (orig-ctor
                                 (merge crux-meta-fields m))))]
    (update-in domain-spec [:entities entity-symbol]
               merge defrecord-map)))

(defn reify-all-entity-records! [domain-spec]
  (reduce reify-entity-record! domain-spec (keys (:entities domain-spec))))

(defn reify-all-entity-event+command-records! [domain-spec]
  (reduce reify-entity-event+command-records! domain-spec
          (keys (:entities domain-spec))))

(defn reify-domain-records! [domain-spec]
  (-> domain-spec
      reify-all-entity-records!
      reify-all-entity-event+command-records!))

;; (defn defmulti-event-reducer [domain-map multi-name entity-symbol]
;;   (let [events (get-in domain-map
;;                        [:entities entity-symbol :events])]
;;     (eval `(defmulti ~multi-name (fn [entity# event#] (type event#))))
;;     (doseq [[event-symbol event-map] events]
;;       (let [event-symbol (-entity-event-symbol entity-symbol
;;                                                event-symbol)
;;             reducer-fn (:reducer event-map)]
;;         (when-not reducer-fn
;;           (throw+ {:type :library/specify-reducer-error
;;                    :msg ""}))
;;         (eval `(defmethod ~multi-name ~event-symbol [state0# evt#]
;;                  (~reducer-fn state0# evt#)))))
;;     domain-map))
