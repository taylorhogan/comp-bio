; My  "Apologies"To"The clojure elite. First Attempts...

(ns comp-bio.cyclopeptide
  (:require [clojure.math.combinatorics]
            [comp-bio.primitives :refer :all ]
            [comp-bio.tables :refer :all ]))

; return the atomic weight of a set of codons
(defn weight [codons]
  (let [l (map (fn [c] (to-weight c)) codons)
        s (reduce + l)]
    s
    )
  )

; given a codon find all contiguous (consider wrapping) codons of length k
(defn k-clumps [codon k]
  (let [
         f (take (dec k) codon)
         w (concat codon f)]
    (loop
      [in-set w
       out-set ()
       k k]

      (if (empty? in-set) out-set
        (if (< (count in-set) k) out-set
          (recur (rest in-set) (concat out-set (list (take k in-set))) k))
        )
      )
    )
  )

; add in the empty set and the full set to a list of codons
(defn add-book-ends [codons full-set]
  (concat (list ()) (concat full-set (list codons)))
  )

; find all groups of contiguous codons in a codon
(defn all-k-clumps [codons]
  (add-book-ends codons
    (loop [in-set codons
           out-set ()
           this-k (dec (count codons))]

      (if (<= this-k 0) out-set
        (recur in-set (concat out-set (k-clumps in-set this-k)) (dec this-k))
        )
      )
    )
  )

; find the spectrum of weights for a codon
(defn spectrum [codons]
  (let [in-set (all-k-clumps codons)]
    (sort
      (loop
        [in-set in-set
         out-set ()]

        (if (empty? in-set) out-set
          (recur (rest in-set) (conj out-set (weight (first in-set)))))
        )
      )
    )
  )


; given one codon's weight add all 18 potential codon weights to it, creating 18 lists
(defn branch [weight]
  (map (fn [x] (concat (list x) weight)) (vals to-weight))
  )



(defn branch-all [weights]
  (loop [iset weights
         out1-set ()]

    (if (empty? iset) out1-set
      (recur (rest iset) (concat out1-set (branch (first iset)))))
    )
  )



; does this list of weights produce (sum of all in list) a weight that exist in the original
(defn match? [ws original]
  (let [sum (reduce + ws)]
    (some #(= sum %) original)
    )
  )

(defn afilter [candidates original]
  (loop [cs candidates
         o original
         out-list ()]


    (if (empty? cs) out-list
      (recur (rest cs) o (if (match? (first cs) o) (conj out-list (first cs)) out-list)))

    )
  )

(defn cyclopeptide [weights]

  )


(def zeroorder
  (map (fn [x] (concat (list x) ())) (vals to-weight))
  )

(println "zero" zeroorder)
(def atest (branch (first zeroorder)))
(println "a test" atest)
(def btest (branch (first (rest zeroorder))))
(count (concat atest btest))
(def firstorder (branch-all zeroorder))
(def secondorder (branch-all firstorder))
(count firstorder)
(count secondorder)
;(println "first order" (count firstorder) firstorder)
;(def secondorder (branch firstorder))
;(println (count secondorder) secondorder)

(first zeroorder)

