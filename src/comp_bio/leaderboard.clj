; My  "Apologies"To"The clojure elite. First Attempts...

(ns comp-bio.leaderboard
  (:require [comp-bio.primitives :refer :all ]
            [comp-bio.tables :refer :all ]
            [comp-bio.spectrum :refer :all ]
            ))




; how many of the ws exist exist in the original spectrum
(defn peptide-in-spectrum [peptide spectrum]
  (let [sum (reduce + peptide)]
    (some #(= sum %) spectrum)
    )
  )




; given one candidate is it spectrum consistant, all k-mers must match?
(defn score [candidate spectrum]
  (let [all (all-k-clumps candidate)]

    (loop
      [candidates all
       score 0]

      (if (empty? candidates) score
        (recur (rest candidates) (if (peptide-in-spectrum (first candidates) spectrum) (inc score) score)

          )
        )
      )
    )
  )


; take top N of a map keeping ties
(defn take-top [cutoff leader-board-map]
  (let [sorted-map (sort-by val < leader-board-map)
        top-score (val (last (take cutoff sorted-map)))]

    (loop [sorted sorted-map
           top top-score
           out-set ()]
      (do

        (if (empty? sorted) out-set
          (let [x (first sorted)
                xs (rest sorted)
                this-score (val x)
                this-candidate (key x)]

            (recur xs top (if (>= this-score top) (concat out-set this-candidate) out-set)

              )
            )
          )
        )
      )
    )
  )

; score each candidate and take top N
(defn leader-filter [candidates original NUM]
  (take-top NUM
    (loop [cs candidates
           o original
           leader-board-map {}]


      (if (empty? cs) leader-board-map
        (recur (rest cs) o (assoc leader-board-map (first cs) (score (first cs) original)))
        )
      )
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

; one step of branch/bound
(defn branch-bound [use-codons original horizon NUM]
  (leader-filter (branch-all use-codons horizon) original NUM)
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
; perform a branch and bound search for potential peptide strings
(defn leader-board [weights codons20 NUM]

  (let [use-codons (available-peptides codons20 weights)]

    (loop
      [best ()
       last use-codons
       ]
      (if (empty? last) best
        (do
          (println weights)
          (recur last (branch-bound use-codons weights last NUM))
          )
        )
      )
    )

  )




(def s (list 0 71 113 129 147 200 218 260 313 331 347 389 460))
(def NUM 10)

(def answer (leader-board s (vals (to-weight)) NUM))
;(pretty-print-weights answer)

(def ms {"a" 1 "b" 2 "c" 3 "d" 4})
(val (last (take 10 (sort-by val < ms))))
