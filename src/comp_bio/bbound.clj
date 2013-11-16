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




(defn sfilter [candidates original]
  (loop [cs candidates
         o original
         s (reduce + original)
         out-list ()]


    (if (empty? cs) out-list
      (recur (rest cs) o s (if (<= (reduce + (first cs)) s) (conj out-list (first cs)) out-list)))

    )
  )


(defn cfilter [candidates original]
  (loop [cs candidates
         o original
         out-list ()]


    (if (empty? cs) out-list
      (recur (rest cs) o (if (match? (first cs) o) (conj out-list (first cs)) out-list)))

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

(def shortl (list 0 1))
(def orig (list 0 71 97 99 103 113 113 114 115 131 137 196 200 202 208 214 226 227 228 240 245 299 311 311 316 327 337 339 340 341 358 408 414 424 429 436 440 442 453 455 471 507 527 537 539 542 551 554 556 566 586 622 638 640 651 653 657 664 669 679 685 735 752 753 754 756 766 777 782 782 794 848 853 865 866 867 879 885 891 893 897 956 962 978 979 980 980 990 994 996 1022 1093))

