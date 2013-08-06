(ns compliment.context)

(def ^:private previous-context (atom nil))

(def prefix-placeholder '__prefix__)

(defn parse-context [context]
  (let [parse (fn parse [ctx]
                (cond
                 (sequential? ctx)
                 (when-let [res (first (keep-indexed (fn [idx el]
                                                       (when-let [p (parse el)]
                                                         [idx p]))
                                                     ctx))]
                   (cons {:idx (first res) :form ctx} (second res)))

                 (map? ctx)
                 (when-let [res (first (keep (fn [[k v]]
                                               (if-let [p (parse v)]
                                                 [k :value p]
                                                 (when-let [p (parse k)]
                                                   [v :key p])))
                                             ctx))]
                   (cons {:idx (first res) :map-role (second res) :form ctx}
                         (nth res 2)))

                 (= ctx prefix-placeholder) ()))
        parsed (parse context)]
    (when parsed
      (reverse parsed))))

(defn cache-context [context]
  (when-not (= context :same)
    (reset! previous-context (parse-context context)))
  @previous-context)
