; My  "Apologies"To"The clojure elite. First Attempts...

(ns comp-bio.translation
  (:require [clojure.math.combinatorics]
            [comp-bio.primitives :refer :all ]
            [comp-bio.tables :refer :all ]))



; change Ts->Us (transcription)
(defn to-rna [dna]
  (map (fn [c] (if (= c \T) \U c)) dna)
  )




; perform translation from rna to protein
(defn transcribe [rna]
  (reverse
    (loop
      [inset (partition 3 rna)
       outset ()]

      (if (empty? inset) outset
        (let [codon (get (to-protein) (apply str (first inset)))]

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




