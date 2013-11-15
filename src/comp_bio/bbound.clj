; My  "Apologies"To"The clojure elite. First Attempts...

(ns comp-bio.bbound
  (:require [comp-bio.primitives :refer :all ]
            [comp-bio.tables :refer :all ]))


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



(defn sfilter [candidates original]
  (loop [cs candidates
         o original
         s (reduce + original)
         out-list ()]


    (if (empty? cs) out-list
      (recur (rest cs) o s (if (<= (reduce + (first cs)) s) (conj out-list (first cs)) out-list)))

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
  (efilter (branch-all use-codons horizon) original)
  )


(defn pretty-print-weights [ws]
  (loop [ws ws]
    (if-not (empty? ws)
      (do (println (reduce + (first ws)) "at" (apply str (interpose "-" (first ws)))) (recur (rest ws))))))



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


(def orig (list 113 128 186 241 299 314 427))
(println orig)
(def peptides (vals (to-weight)))
(println peptides)



(def myanswer (search-for-cyclopeptides orig peptides))
(println myanswer)
