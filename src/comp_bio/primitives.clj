; My apologies to the clojure elite. First attempts...

(ns
  ^{:author "Taylor Hogan",
    :doc "primitive genome transformations"}
  comp-bio.primitives
  )

; Turn a String into a List of chars
(defn to-genome [char-string] (seq char-string))

; Pretty print a list of chars into a genome looking sequence  (turn a list of chars into a string)
(defn pretty-print [genome] (apply str genome))

; Pretty print a seq of genomes
(defn pretty-print-seq [list-of-genomes] (map ((fn [x] (apply str x)))))

; Return the opposite nucleotide
(defn opposite [arg] (cond
                       (= arg \A) \T
                       (= arg \T) \A
                       (= arg \C) \G
                       (= arg \G) \C
                       ))



; Return the compliment of a genome
(defn compliment [genome] (reverse (map opposite genome)))

; from a k-mere, count list, extract just the k-mers
(defn filter-k-mers [k-mers k]
  (reduce
    (fn [m s] (if (= (count (first s)) k) (assoc m (first s) (second s)) m))
    {}
    k-mers
    )
  )

; find k-mers of a genome
(defn k-match [genome k]
  (loop [mini-strand genome
         this-k k
         accum {}]

    (if-not (empty? mini-strand)
      (let [k-key (apply str (take this-k mini-strand))]
        (recur (rest mini-strand) this-k (assoc accum k-key (inc (get accum k-key 0)))))
      (sort-by second > accum)
      )
    )
  )

; find indices of a pattern in a genome
(defn p-match [genome pattern]
  (loop [mini-strand genome
         this-pattern pattern
         accum ()
         index 0]

    (let [size (.length this-pattern)]
      (if (empty? mini-strand) (reverse accum)
        (let [this-strand-piece (apply str (take size mini-strand))
              updated (if (= this-pattern this-strand-piece) (cons index accum) accum)]
          (recur (rest mini-strand) this-pattern updated (inc index))))
      )
    )
  )

; find the number of differences in 2 patterns, maximizing at d
(defn d-apart? [pattern p2 d]
  (loop
    [p1 pattern
     p2 p2
     total 0]
    (if (empty? p1) (<= total d)
      (if (> total d) (<= total d)
        (recur (rest p1) (rest p2) (if (= (first p1) (first p2)) total (inc total))))))
  )

; find the number of differences in 2 patterns, maximizing at d (alternate slower solution)
(defn d? [pattern p2 d]
  (let [diffs (map (fn [a b] (if (= a b) 0 1)) pattern p2)]
    (<= (reduce + diffs) d)
    )
  )



; find all locations of a pattern in a genome whose differences <= d
(defn approx-pattern-match [pattern genome d]
  (loop
    [genome genome
     count 0
     d d
     collection #{}]
    (if (empty? genome) collection
      (do
        (recur (rest genome) (inc count) d (if (d-apart? pattern genome d) (conj collection count) collection)))
      )
    )
  )

; some convenience functions for tuple (key occurance)
(defn occurance-key [o] (first o))
(defn occurance-val [o] (second o))


; return the frequency of a pattern in a genome
(defn frequency-of-pattern [genome pattern]
  (count (p-match genome pattern))
  )

; return the frequency of a fuzzy pattern in a genome
(defn frequency-of-fuzzy-pattern [pattern genome d]
  (count (approx-pattern-match pattern genome d))
  )

; return an occurrence set of for a set of fuzzy patterns
(defn fuzzy-occurences-set [genome pattern-set d]
  (reduce (fn [col i] (assoc col i (frequency-of-fuzzy-pattern i genome d))) {} pattern-set)
  )

; return an occurrence set of for a set of fuzzy patterns
(defn fuzzy-occurences-set-with-compliment [genome genome-c pattern-set d]

  (reduce (fn [col i]
            (assoc col i (+ (frequency-of-fuzzy-pattern i genome d) (frequency-of-fuzzy-pattern i genome-c d))))
    {} pattern-set)
  )


; return a list of k-mers that have at-least some occurrence
(defn k-mers-at-least [genome k at-least]
  (let [k-list (k-match genome k)]
    (vals (reduce
            (fn [m s]
              (if (>= (occurance-val s) at-least) (assoc m (count m) (occurance-key s)) m))
            {}
            k-list
            )
      )
    )
  )

; determine if a k-mer is at least L apart
(defn L-apart? [genome L t k-mer]
  (let [locations (p-match genome k-mer)]
    (loop [this-list locations
           t t
           L L
           state 0]
      (if (= state 1) state
        (if (< (count this-list) t) state
          (do
            (def span (- (nth this-list (dec t)) (first this-list)))
            (recur (rest this-list) t L (if (>= L span) 1 0))
            )
          )

        )
      )
    )

  )

; Find All distinct k-mers forming (L, t)-clumps in Genome
(defn clump-match-new [genome k L t]
  (vals (let [good-k-mers (k-mers-at-least genome k t)]
          (reduce
            (fn [m this-k-mer]
              (if (= (L-apart? genome L t this-k-mer) 1) (assoc m (count m) this-k-mer) m))
            {}
            good-k-mers
            )
          ))
  )



; return the skew cost
(defn delta-c [c]
  (cond
    (= c \A) 0
    (= c \T) 0
    (= c \C) -1
    (= c \G) 1
    )
  )

; return the sweep of skews along the genome
(defn min-skew-data [genome]
  (loop
    [gs genome
     cur 0
     index 1
     data {}]
    (if (empty? gs) (sort-by second data)
      (let [this-cur (+ cur (delta-c (first gs)))]
        (recur (rest gs) this-cur (inc index) (assoc data index this-cur))
        )
      )
    )
  )

; given a full data set return the minimums
(defn min-skew [data]
  (loop
    [d data
     b (second (first data))
     list #{}
     ]
    (if (empty? d) (sort list)
      (if (> (second (first d)) b) (sort list)
        (recur (rest d) b (conj list (first (first d)))))
      )
    )
  )

; concat 3 strings
(defn concat3 [pre mid post]
  (apply str (concat pre (apply str (concat mid post))))
  )

; for string PRE POST, create 4 variants
(defn build-variants [pre post]
  (let [a (concat3 pre "A" post)
        b (concat3 pre "G" post)
        c (concat3 pre "C" post)
        d (concat3 pre "T" post)]
    [a b c d]
    )
  )



; generate all neighbors of a pattern varying at index
(defn neihbors-at-index [pattern index]
  (let [pre (apply str (take index pattern))
        post (apply str (drop (inc index) pattern))
        ]
    (build-variants pre post)
    )
  )


; for a give string create all neighbors 1 away
(defn neigbors-1 [pattern]
  (loop
    [p pattern
     index 0
     the-set #{}
     ]
    (if (>= index (count pattern)) the-set
      (recur p (inc index) (into the-set (neihbors-at-index p index)))
      )
    )
  )

; given a set of strings, create a new set of all the neighbors of each member of the incoming set
(defn neighbor-1-of-set [aset]
  (reduce (fn [coll i] (into coll (neigbors-1 i))) #{} (seq aset))
  )


; find all fuzzy k-mers of length k and fuzziness d
(defn total-fuzzy-kmers [genome k d]
  (let [k-mers (k-match genome k)
        good-k-mers (filter-k-mers k-mers k)
        k-mer-list (keys good-k-mers)]
    (loop [inset k-mer-list
           n d]
       (if (= n 1) inset
        (recur (neighbor-1-of-set inset) (dec n))
        )
      )
    )
  )

(defn extract-top [data]
  (def best (second (first data)))
  (loop
    [d data
     b best
     list #{}
     ]
    (if (empty? d) list
      (if (< (second (first d)) b) list
        (recur (rest d) b (conj list (first (first d)))))
      )
    )
  )

; create a list of all fuzzy patterns, sorted best first
(defn best-fuzzy-pattern [genome k d]
  (let [fuzzy-kmers (total-fuzzy-kmers genome k d)
        oc (fuzzy-occurences-set genome fuzzy-kmers d)]
    (reverse (sort-by second oc))
    )
  )

; create a list of all fuzzy patterns & consider compliments, sorted best first
(defn best-fuzzy-pattern-with-compliments [genome k d]
  (let [fuzzy-kmers-normal (total-fuzzy-kmers genome k d)
        oc (fuzzy-occurences-set-with-compliment genome (compliment genome) fuzzy-kmers-normal d)]
    (reverse (sort-by second oc))
    )
  )



