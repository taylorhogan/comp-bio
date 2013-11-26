(ns comp-bio.motifenumeration
  (:require [comp-bio.primitives :refer :all ]
            [comp-bio.tables :refer :all ]
            [comp-bio.spectrum :refer :all ]
            [clojure.java.io :as io]
            [clojure.string :as STR :only (join split)]

            )
  )


; read an input file and create a map of the parameters
(defn read-input-file [file-name]
  (let [rdr (io/reader file-name)
        line (line-seq rdr)]

    (loop
      [flines line
       lc 0
       my-map {}]


      (if (empty? flines) (assoc my-map :max (dec lc))
        (recur (rest flines) (inc lc)
          (if (= lc 0)
            (let
              [args (STR/split (first flines) #"\s")]
              (assoc my-map :k (read-string (first args)) :d (read-string (second args))))
            (assoc my-map :dnas (conj (my-map :dnas ) (list (first flines))))
            )
          )
        )
      )

    )
  )


(defn ks-of-dna [dna k]
  (k-match (to-genome dna) k)
  )

(defn motif-enumeration [dna-strings k d]

  (loop
    [in-set dna-strings
     out-set ()]

    (if (empty? in-set) out-set
      (recur (rest in-set) (conj out-set (ks-of-dna (first (first in-set)) k))
        )
      )
    )
  )



(def args (read-input-file "/Users/taylor/Documents/leinprojects/comp-bio/src/comp_bio/input.txt"))

(motif-enumeration (args :dnas ) (args :k ) (args :d ))




