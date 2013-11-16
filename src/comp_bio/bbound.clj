; My  "Apologies"To"The clojure elite. First Attempts...

(ns comp-bio.bbound
  (:require [comp-bio.primitives :refer :all ]
            [comp-bio.tables :refer :all ]
            [comp-bio.cyclopeptide :refer :all ]))


; does this list of weights produce (sum of all in list) a weight that exist in the original
(defn match? [ws original]
  (let [sum (reduce + ws)]
    (some #(= sum %) original)
    )
  )


; given one codon's weight add all 18 potential codon weights to it, creating 18 lists
(defn branch [use-codons weight]
  (map (fn [x] (concat x weight)) use-codons)
  )




(defn branch-all [use-codons weights]
  (loop [iset weights
         out1-set #{}]

    (if (empty? iset) out1-set
      (recur (rest iset) (into out1-set (branch use-codons (first iset)))))
    )
  )





; given one candidate is it spectrum consistant, all k-mers must match?
(defn spectrum-consistant [candidate original]
  (let [all (all-k-clumps candidate)]
    (every? #(match? % original) all)
    )
  )



; incoming is a list of candidates to add to the tree, outgoing is the ones that are spectrum consistant
(defn cfilter [candidates original]
  (loop [cs candidates
         o original
         out-list ()]


    (if (empty? cs) out-list
      (recur (rest cs) o (if (spectrum-consistant (first cs) o) (conj out-list (first cs)) out-list)))

    )
  )


; remove weights not represented in the original
(defn efilter [candidates original]
  (loop [cs candidates
         o original
         out-list ()]


    (if (empty? cs) out-list
      (recur (rest cs) o (if (match? (first cs) o) (conj out-list (first cs)) out-list)))

    )
  )

; one step of branch/bound
(defn branch-bound [use-codons original horizon]
  (cfilter (branch-all use-codons horizon) original)
  )


(defn pretty-print-weights [ws]
  (loop [ws ws]
    (if-not (empty? ws)
      (do (println (apply str (interpose "-" (first ws)))) (recur (rest ws))))))



(defn available-peptides [codon-map original]
  (loop [candidate original
         out-set ()]

    (if (empty? candidate) out-set

      (recur
        (rest candidate)
        (if (in? codon-map (first candidate))
          (conj out-set (list (first candidate)))
          out-set)
        )
      )
    )
  )


; perform a branch and bound search for potential peptide strings
(defn search-for-cyclopeptides [weights codons20]

  (let [use-codons (available-peptides codons20 weights)]

    (loop
      [best ()
       last use-codons
       ]
      (if (empty? last) best

        (recur last (branch-bound use-codons weights last))
        )

      )
    )

  )



(def orig (list 0 87 87 87 99 101 103 115 131 137 147 147 174 188 190 202 202 230 234 238 262 275 278 284 289 305 321 325 333 349 349 377 385 404 412 415 420 422 436 436 452 472 480 514 516 523 535 537 539 551 559 559 567 603 615 617 624 626 638 674 682 682 690 702 704 706 718 725 727 761 769 789 805 805 819 821 826 829 837 856 864 892 892 908 916 920 936 952 957 963 966 979 1003 1007 1011 1039 1039 1051 1053 1067 1094 1094 1104 1110 1126 1138 1140 1142 1154 1154 1154 1241))
(def shortl (list 0 113 128 186 241 299 314 427))
(def answer (search-for-cyclopeptides orig (vals (to-weight))))
(pretty-print-weights answer)


