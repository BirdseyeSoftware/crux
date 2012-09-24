(ns crux.command-handling)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command constraint and validation checking

(defn first-unmet-constraint [entity constraint-forms+preds]
  (some (fn -or* [[form pred]] (if-not (pred entity) form))
        constraint-forms+preds))

(defn unmet-constraints [entity constraint-forms+preds]
  (for [[form pred] constraint-forms+preds
        :when (not (pred entity))]
    form))

(defn unmet-validations [validation-args validation-forms+vfns]
  (for [[form vfn] validation-forms+vfns
        [error-message] [[(vfn validation-args)]]
        :when (not (nil? error-message))]
    {:error-message error-message
     :validation-form form}))

(defn first-unmet-validation [validation-args validation-forms+vfns]
  (first (unmet-validations validation-args validation-forms+vfns)))
