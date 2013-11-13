; My  "Apologies"To"The clojure elite. First Attempts...

(ns comp-bio.translation
  (:require [clojure.math.combinatorics]
            [comp-bio.primitives :refer :all ]
            [comp-bio.tables :refer :all ]))



; change Ts->Us (transcription)
(defn to-rna [dna]
  (map (fn [c] (if (= c \T) \U c)) dna)
  )




; peform translation from rna to protein
(defn transcribe [rna]
  (reverse
    (loop
      [inset (partition 3 rna)
       outset ()]

      (if (empty? inset) outset
        (let [codon (to-protein (apply str (first inset)))]

          (if (= codon "STOP") outset
            (recur (rest inset) (conj outset codon)))
          )
        )
      )
    )
  )

; determine if this strand (or compliment) of dna creates the amino acid
(defn peptides-match? [dna amino-acid]
  (let [rna (to-rna dna)
        rna-c (to-rna (compliment dna))
        protein (apply str (transcribe rna))
        protein-c (apply str (transcribe rna-c))
        acid (apply str amino-acid)
        ]
    (or
      (= acid protein)
      (= acid protein-c))

    )
  )

; determine all strands of the dna that would produce the amino-acids
(defn peptides-of [dna amino-acid]
  (let [len (* 3 (count amino-acid))]
    (loop [dna dna
           len len
           amino amino-acid
           results ()]

      (if (empty? dna) results
        (do
          (let [strand (take len dna)]
            (recur (rest dna) len amino (if (peptides-match? strand amino) (conj results strand) results)))))
      ))
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

      ))
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


