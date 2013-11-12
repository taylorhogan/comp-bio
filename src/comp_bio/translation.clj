; My  "Apologies"To"The clojure elite. First Attempts...

(ns primitives)


; change Ts->Us (transcription)
(defn to-rna [dna]
  (map (fn [c] (if (= c \T) \U c)) dna)
  )

; define a map of codon->weight
(def to-weight
  {
    \G 57
    \A 71
    \S 87
    \P 97
    \V 99
    \T 101
    \C 103
    \I 113
    \L 113
    \N 114
    \D 115
    \K 128
    \Q 128
    \E 129
    \M 131
    \H 137
    \F 147
    \R 156
    \Y 163
    \W 186
    }
  )
; define a map of 3 nucleotides (codon) -> protein
(def to-protein
  {
    "UUU" "F"
    "CUU" "L"
    "AUU" "I"
    "GUU" "V"
    "UUC" "F"
    "CUC" "L"
    "AUC" "I"
    "GUC" "V"
    "UUA" "L"
    "CUA" "L"
    "AUA" "I"
    "GUA" "V"
    "UUG" "L"
    "CUG" "L"
    "AUG" "M"
    "GUG" "V"
    "UCU" "S"
    "CCU" "P"
    "ACU" "T"
    "GCU" "A"
    "UCC" "S"
    "CCC" "P"
    "ACC" "T"
    "GCC" "A"
    "UCA" "S"
    "CCA" "P"
    "ACA" "T"
    "GCA" "A"
    "UCG" "S"
    "CCG" "P"
    "ACG" "T"
    "GCG" "A"
    "UAU" "Y"
    "CAU" "H"
    "AAU" "N"
    "GAU" "D"
    "UAC" "Y"
    "CAC" "H"
    "AAC" "N"
    "GAC" "D"
    "UAA" "STOP"
    "CAA" "Q"
    "AAA" "K"
    "GAA" "E"
    "UAG" "STOP"
    "CAG" "Q"
    "AAG" "K"
    "GAG" "E"
    "UGU" "C"
    "CGU" "R"
    "AGU" "S"
    "GGU" "G"
    "UGC" "C"
    "CGC" "R"
    "AGC" "S"
    "GGC" "G"
    "UGA" "STOP"
    "CGA" "R"
    "AGA" "R"
    "GGA" "G"
    "UGG" "W"
    "CGG" "R"
    "AGG" "R"
    "GGG" "G"}
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


(defn weight [codons]
  (let [l (map (fn [c] (to-weight c)) codons)
        s (reduce + l)]
    s
    )
  )


(defn spectrum [codons]

)

(clojure.math.combinatorics/combinations [1 2 3] 2)
;(println (weight (seq "TGT")))




