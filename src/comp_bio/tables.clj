(ns comp-bio.tables)

; define a map of codon->weight
(defn to-weight []
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
(defn to-protein []
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

(defn testme []
  )