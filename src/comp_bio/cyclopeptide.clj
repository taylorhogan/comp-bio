; My  "Apologies"To"The clojure elite. First Attempts...

(ns comp-bio.cyclopeptide
  (:require [clojure.math.combinatorics]
            [comp-bio.primitives :refer :all ]
            [comp-bio.tables :refer :all ]))



; just in case your universe does not have 20 codons
(defn primitive-peptides [codon-map]
  (map (fn [x] (concat (list x) ())) (vals codon-map))
  )

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

; given a codon find all contiguous (consider wrapping) codons of length k
(defn k-clumps-linear [codon k]

  (loop
    [in-set codon
     out-set ()
     k k]

    (if (empty? in-set) out-set
      (if (< (count in-set) k) out-set
        (recur (rest in-set) (concat out-set (list (take k in-set))) k))
      )
    )

  )

; add in the empty set and the full set to a list of codons
(defn add-book-ends [codons full-set]
  (concat (list ()) (list codons) full-set)
  )

; find all groups of contiguous codons in a codon
(defn all-k-clumps [codons]

  (add-book-ends codons (loop [in-set codons
                               out-set ()
                               this-k (dec (count codons))]

                          (if (<= this-k 0) out-set

                            (recur in-set (concat out-set (k-clumps-linear in-set this-k)) (dec this-k))
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









