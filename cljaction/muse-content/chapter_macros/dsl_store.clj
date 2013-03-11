(ns chapter-macros.dsl-store)

(def RULES (ref {}))

(defn register-segment [segment-name segment-fn]
  (dosync
   (alter RULES assoc-in [:segments segment-name] segment-fn)))

(defn segment-named [segment-name]
  (get-in @RULES [:segments segment-name]))

(defn all-segments []
  (:segments @RULES))

(defn clear-all-rules []
  (dosync
   (ref-set RULES {})))