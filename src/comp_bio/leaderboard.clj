; My  "Apologies"To"The clojure elite. First Attempts...

(ns comp-bio.leaderboard
  (:require [comp-bio.primitives :refer :all ]
            [comp-bio.tables :refer :all ]
            [comp-bio.spectrum :refer :all ]
            ))



;LEADERBOARDCYCLOPEPTIDESEQUENCING(Spectrum, N)
;Leaderboard ← {0-peptide}
;LeaderPeptide ← 0-peptide
;while Leaderboard is non-empty
; Leaderboard ← Expand(Leaderboard)
; for each Peptide in Leaderboard
;   if Mass(Peptide) = ParentMass(Spectrum)
;     if Score(Peptide, Spectrum) > Score(LeaderPeptide, Spectrum)
;        LeaderPeptide ← Peptide
;     else if Mass(Peptide) > ParentMass(Spectrum)
;        remove Peptide from Leaderboard
; Leaderboard ← Cut(Leaderboard, Spectrum, N)
; output LeaderPeptide





; how many of the ws exist exist in the original spectrum
(defn peptide-in-spectrum [peptide spectrum]
  (let [sum (reduce + peptide)]
    (if (empty? peptide) 0
      (if (some #(= sum %) spectrum) 1 0)
      )
    )
  )


; determine if a peptide mass is > parent spectrum mass
(defn fatty? [peptide spectrum]
  (let [my-weight (reduce + peptide)
        parent-weight (last spectrum)]
    (> my-weight parent-weight))
  )

; determine if the peptide mass = parenet spectrum mass
(defn perfect? [peptide spectrum]
  (let [my-weight (reduce + peptide)
        parent-weight (last spectrum)]
    (= my-weight parent-weight))
  )

; given one candidate is it spectrum consistant, all k-mers must match?
(defn score [candidate spectrum]
  (let [all (all-k-clumps-linear candidate)]

    (loop
      [candidates all
       score 0]
      (do

        (if (empty? candidates) score
          (recur (rest candidates) (+ (peptide-in-spectrum (first candidates) spectrum) score))

          )
        )
      )
    )
  )



; take top N of a map keeping ties
(defn take-top [cutoff leader-board-map]
  (if (empty? leader-board-map) ()
    (let [sorted-map (sort-by val > leader-board-map)
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

              (recur xs top (if (>= this-score top) (concat out-set (list this-candidate)) out-set))

              )
            )
          )
        )
      )
    )

  )

; score each candidate and take top N
(defn leader-filter [candidates spectrum NUM]
  (take-top NUM
    (loop [cs candidates
           o spectrum
           leader-board-map {}]


      (if (empty? cs) leader-board-map
        (recur (rest cs) o
          (if (fatty? (first cs) o) leader-board-map
            (assoc leader-board-map (first cs) (score (first cs) o))
            )
          )
        )
      )
    )
  )


; given one codon's weight add all 18 potential codon weights to it, creating 18 lists
(defn branch [weight]
  (let [answer (map (fn [x] (concat weight (vector x))) (set (vals (to-weight))))]
    answer)
  )




(defn branch-all [horizon]
  (loop [in-set horizon
         out-set #{}]

    (if (empty? in-set) out-set
      (do

        (recur (rest in-set) (into out-set (branch (first in-set)))))
      )
    )
  )



; one step of branch/bound
(defn branch-bound [spectrum horizon NUM]

  (let [answer (leader-filter (branch-all horizon) spectrum NUM)]
    answer
    )
  )


(defn pretty-print-weights [ws]
  (loop [ws ws]
    (if-not (empty? ws)
      (do (println (apply str (interpose "-" (first ws)))) (recur (rest ws))))))



; perform a branch and bound search for potential peptide strings
(defn leader-board [spectrum NUM]
  (loop
    [best ()
     last (list ())
     index 0
     ]

    (if (and (> index 0) (empty? last)) best
      (do
        (recur last (branch-bound spectrum last NUM) (inc index))
        )
      )
    )

  )

(comment
  (def stest (list 0 71 113 129 147 200 218 260 313 331 347 389 460))
  (def sn 2)
  (def answers (leader-board stest sn))
  (pretty-print-weights answers)
  (if (some #(= '(113 147 71 129) %) answers) (println "success") (println "failure"))

  (def mtest (list 0 71 97 101 103 113 113 113 113 114 114 115 128 128 128 128 129 131 131 131 156 156 184 186 186 200 214 227 227 228 230 231 241 242 242 243 244 244 256 257 262 269 270 287 298 299 301 328 331 340 340 343 345 345 356 358 359 370 370 372 375 383 385 397 400 401 429 430 442 453 454 454 459 462 468 471 472 473 474 485 486 487 498 499 501 512 514 514 542 561 567 570 573 575 581 583 585 590 599 600 600 601 602 610 615 615 616 627 627 630 658 695 696 698 698 698 701 703 704 713 723 728 728 728 728 730 730 731 741 744 747 758 761 769 799 810 817 827 829 831 832 841 841 844 844 851 854 854 857 859 862 872 882 884 886 889 928 928 944 945 947 955 955 958 959 960 966 967 972 972 982 985 990 996 997 1000 1000 1003 1041 1056 1059 1062 1068 1068 1068 1073 1075 1075 1084 1087 1089 1095 1097 1103 1113 1114 1128 1128 1131 1152 1172 1172 1181 1182 1184 1189 1190 1190 1196 1197 1199 1200 1202 1210 1212 1227 1231 1242 1259 1259 1283 1295 1298 1303 1303 1303 1303 1304 1311 1312 1317 1318 1325 1325 1328 1330 1338 1340 1345 1355 1356 1388 1396 1416 1426 1426 1427 1431 1432 1432 1434 1440 1442 1443 1445 1451 1453 1453 1454 1458 1459 1459 1469 1489 1497 1529 1530 1540 1545 1547 1555 1557 1560 1560 1567 1568 1573 1574 1581 1582 1582 1582 1582 1587 1590 1602 1626 1626 1643 1654 1658 1673 1675 1683 1685 1686 1688 1689 1695 1695 1695 1696 1701 1703 1704 1713 1713 1733 1754 1757 1757 1771 1772 1782 1788 1790 1796 1798 1801 1810 1810 1812 1817 1817 1817 1823 1826 1829 1844 1882 1885 1885 1888 1889 1895 1900 1903 1913 1913 1918 1919 1925 1926 1927 1930 1930 1938 1940 1941 1957 1957 1996 1999 2001 2003 2013 2023 2026 2028 2031 2031 2034 2041 2041 2044 2044 2053 2054 2056 2058 2068 2075 2086 2116 2124 2127 2138 2141 2144 2154 2155 2155 2157 2157 2157 2157 2162 2172 2181 2182 2184 2187 2187 2187 2189 2190 2227 2255 2258 2258 2269 2270 2270 2275 2283 2284 2285 2285 2286 2295 2300 2302 2304 2310 2312 2315 2318 2324 2343 2371 2371 2373 2384 2386 2387 2398 2399 2400 2411 2412 2413 2414 2417 2423 2426 2431 2431 2432 2443 2455 2456 2484 2485 2488 2500 2502 2510 2513 2515 2515 2526 2527 2529 2540 2540 2542 2545 2545 2554 2557 2584 2586 2587 2598 2615 2616 2623 2628 2629 2641 2641 2642 2643 2643 2644 2654 2655 2657 2658 2658 2671 2685 2699 2699 2701 2729 2729 2754 2754 2754 2756 2757 2757 2757 2757 2770 2771 2771 2772 2772 2772 2772 2782 2784 2788 2814 2885))
  (def mn 26)
  (def answerm (leader-board mtest mn))
  (pretty-print-weights answerm)
  (if (some #(= '(156 71 113 114 131 156 113 101 129 128 128 114 128 103 97 131 131 113 131 113 128 115 128 113) %) answerm) (println "success") (println "failure"))

  )






