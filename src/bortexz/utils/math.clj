(ns bortexz.utils.math
  (:require [clojure.math.numeric-tower :as math-nt])
  (:import (java.math RoundingMode)))

(defn round-step
  "Given BigDecimal `value`, BigDecimal `step` and RoundingMode `rm`: 
   Rounds the given value to the closer step, setting the scale of the resulting decimal
   to same scale as step, using rm (by default RoundingMode/HALF_DOWN). RoundingMode/HALF_EVEN is not supported 
   as both edges of the step could be even (e.g. using 0.2M as step)."
  (^BigDecimal [^BigDecimal value ^BigDecimal step]
   (round-step value step RoundingMode/HALF_DOWN))
  (^BigDecimal [^BigDecimal value ^BigDecimal step ^RoundingMode rm]
   (assert (pos? step) "Step must be an absolute value")
   (let [^BigDecimal r (.remainder value step)
         rv (if (zero? r)
              value
              (case (.name rm)
                "UP"
                (.add (.subtract value r)
                      (.multiply step (BigDecimal/valueOf ^int (.signum value))))

                "DOWN"
                (.subtract value r)

                "HALF_UP"
                (let [absr (.abs r)
                      diff (.subtract step absr)]
                  (case (compare absr diff)
                    (0 1) (.add value (.multiply diff (BigDecimal/valueOf ^int (.signum value))))
                    -1 (.subtract value r)))

                "HALF_DOWN"
                (let [absr (.abs r)
                      diff (.subtract step absr)]
                  (case (compare absr diff)
                    1 (.add value (.multiply diff (BigDecimal/valueOf ^int (.signum value))))
                    (0 -1) (.subtract value r)))

                "CEILING"
                (case (.signum value)
                  1  (.add (.subtract value r) step)
                  -1 (.subtract value r))

                "FLOOR"
                (case (.signum value)
                  1 (.subtract value r)
                  -1 (.add (.subtract value r) (.negate step)))))]
     (.setScale rv (.scale step)))))

(defn mean
  "Calculates the mean of given collection of numbers `xs`"
  [xs]
  (/ (reduce + 0 xs)
     (count xs)))

(defn variance
  "Calculates the variance of values `xs`. If optional arg `bessel?` is true, applies bessel's correction and uses n-1 
   instead of n as divisor."
  ([xs] (variance xs false))
  ([xs bessel?]
   (let [avg (mean xs)
         total (reduce (fn [acc x]
                         (+ acc (math-nt/expt (- x avg) 2)))
                       0
                       xs)]
     (/ total (cond-> (count xs)
                bessel? (dec))))))

(defn standard-deviation
  "Calculates the standard deviation of values `xs`. If optional arg `bessel?` is true, applies bessel's correction 
   and uses n-1 instead of n as divisor."
  ([xs] (standard-deviation xs false))
  ([xs bessel?] (math-nt/sqrt (variance xs bessel?))))
