(load-file "BigCode.clj")

;; #1 percentage of sentences which contain the
(defn sentences-with-the [sentences]
    (pct-with-word "the" sentences)
)
; (println "percent of each of the authors sentences that contain 'the'" (map #(pct-with-word "the" %) (map2 towords authors)))

;; #2 percemtage of words which are punctuation
(defn pct-punc [sentences]
    (float(/
        (count(filter ispunc (flatten sentences)))
        (count (flatten sentences))
    ))
)

;; #3 percentage of words which are stop words
(defn pct-stop [sentences]
    (float(/
        (count(filter isstop (flatten sentences)))
        (count (flatten sentences))
    ))
)

;; #4 percentage of interesting words 
(defn words-pct [sentences]
    (- 1 (pct-punc sentences) (pct-stop sentences))
)

;; #4 the spacco way
(defn interesting-pct [sentences]
    (float (/(count (remove ispunc(remove isstop(flatten sentences))))
    (count (flatten sentences))))
)

;;#5 10 most common words that are not stopwords and are not punctuation
(defn most-common [sentences]
    (take 10 (sort-by second >(frequencies(remove ispunc (remove isstop(flatten sentences))))))
)
;;#6 n most common words that are not stopwords and are not punctuation
(defn n-most-common [sentences n]
    (take n (sort-by second >(frequencies(remove ispunc (remove isstop(flatten sentences))))))
)

(defn most-common-pairs [sentences]
    (take 10 (sort-by second >(frequencies (map #(str/join " " %) (mapcat #(partition 2 1 %) sentences)))))
)

(defn k-most-common-ngrams [sentences k n]
    (take k (sort-by second >(frequencies (map #(str/join " " %) (mapcat #(partition n 1 %) sentences)))))
)

(defn most-common-sentence-start [sentences]
    (take 10 (sort-by second >(frequencies (map #(first % ) sentences))))
)

(defn printin [arr]
    (println "Poe:" (first arr))
    (println "Wollstoncroft:" (second arr))
    (println "Lovecraft:" (nth arr 2))
)

(println "\n\npercent of each of the authors sentences that contain 'the'")
(printin (map sentences-with-the (map2 towords authors)))
(println "\n\npercent of each of the authors words which are punctuation") 
(printin (map pct-punc (map2 towords authors)))

(println "\n\npercent of each of the authors words which are stop words") 
(printin (map pct-stop (map2 towords authors))) 
(println "\n\n(faster method) percent of each of the authors words which are neither stop words nor punctuation") 
(printin (map interesting-pct (map2 towords authors)))
(println "\n\n10 most common words in each author") 
(printin (map most-common (map2 towords authors))) 
(println "\n\nn (in this case 5) most common words in each author")
(printin (map #(n-most-common % 15)(map2 towords authors)))
(println "\n\n10 most common word pairs for each author")
(printin (map most-common-pairs (map2 towords authors)))
(println "\n\nk (5) most common n(3)grams for each author")
(printin (map #(k-most-common-ngrams % 15 3)(map2 towords authors)))
(println "\n\n10 most common words to start a sentence\n")
(printin (map most-common-sentence-start (map2 towords authors)))
