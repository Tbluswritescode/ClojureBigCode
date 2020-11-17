;; get the Java string functions
;; now we can call string functions like this:
;; str/join
;; str/split
;; etc
(require '[clojure.string :as str])

;; read file of lines of text into a list of strings
(defn read-file [filename]
    ;; Read a file of lines of text into a vector of strings.
    ;; The resulting Clojure structure will look like this:
    ;; ["this is the first sentence ." "this is the second sentence ." "now the third sentence"]
    ;; Each sentence will have its punctuation split out by spaces
    (with-open [rdr (clojure.java.io/reader filename)]
        (reduce conj [] (line-seq rdr))
    )
)

(defn read-file-set [filename]
    ;; Read a file of lines of text into a set of strings
    ;; The resulting Clojure structure will look like this:
    ;; #{"dog" "cat" "bird"}
    (with-open [rdr (clojure.java.io/reader filename)]
        (reduce conj #{} (line-seq rdr))
    )
)

;; read stopwords into a set (constant time lookups)
(def stopset (read-file-set "stopwords.txt"))

;; Read EAP, MWS, HPL into global variables
;; This is a use of the def function in Clojure
;; In general avoid global variables, but for projects like this,
;; it's useful not to have to re-read the file every time
(def eap (read-file "eap.txt"))
(def mws (read-file "mws.txt"))
(def hpl (read-file "hpl.txt"))

;; create a variable containing a vector of all 3 authors
(def authors [eap mws hpl])

;; synthetic data that is better for testing
;; This is a vector containing 3 vectors of sentences.
;; You can pretend that shortauth is like authors when testing your code.
(def shortauth [
    ["the quick brown fox is awesome ." 
        "jumps over the lazy dogs ." 
        "evil dogs slobber on poe ."]
    ["cthulhu says this this this madness rules ." 
        "awesome possum and madness ." 
        "killer bunnies and lovecraft madness ."]
    ["shelley wrote frankenstein" 
        "that is all shelley needs to say"]
])

;; calculate the average of a list of numbers
;; note that this returns a fraction since that's what / does in Clojure
(defn avg
    "calcuate the average of a list of numbers"
    [numbers]
    (if (empty? numbers)
        0
        (/ (reduce + numbers) (count numbers))
    )
)

;; calculate the average of a list of numbers as a float
(defn favg [numbers]
    "average a list of numbers but return a float"
    (float (avg numbers))
)

;; count num sentences for each author using 3 function calls
(println "How many sentences do we have for each author?")
(println "Poe wrote" (count eap))
(println "Shelley wrote" (count mws))
(println "Lovecraft wrote" (count hpl))

;; count num sentences for each author using map and our vector 
;; containing the vector of sentences of each author
(map count authors)

;; average number of characters per sentence for EAP
;; note that (map count eap) returns a huge list of numbers
(avg (map count eap))

;; average number of chars per sentence for each author
(map #(favg (map count %)) authors)

;; convert a string into a list of words, splitting by spaces
;; #" +" is a regular expression, or regex, which is a type of pattern in CS.
;; #" +" that means "one or more spaces". The " +" is a space followed by plus
;; and with regexes, plus means "one or more". Hence, one or more spaces.
(defn towords [sentence] (str/split sentence #" +"))

;; convert a list of strings into a list of words, split by spaces, for one author
;; #" +" is a regular expression, or regex, which is like a pattern.
;; #" +" means "one or more spaces". 
;; Commented out because it prints too much stuff
;; 
;; (map #(str/split % #" +") eap)
;; (map towords eap)

;; convert a list of strings into the number of words in each string 
;; (splitting a string into words by spaces)
(count (map #(str/split % #" +") eap))
(count (map towords eap))

;; convert every sentence in a list of list of sentences (such as shortauth or authors)
;; into a list of the number of words (space-delimited) in each sentence
;; Next two calls commented out because the print too much!
;; 
;; (map #(map towords %) shortauth)
;; (map #(map towords %) authors)

;; now count the number of words in each sentence, given a list of lists of sentences
(map #(map count %) (map #(map towords %) shortauth))

;; ok, this is weird... Lots of mapping a lambda onto a collection of collections
;; let's turn that into a function we can re-use!
;; map a function onto every collection in a collection of collections!
(defn map2
    "Map a function onto every list of a list of lists. So:
    (map2 f [coll1 coll2 coll3])
    would return
    [(map f coll1) (map f coll2) (map f coll3)]
    "
    [f coll]
    (map #(map f %) coll)
)

;; same as above, but recursive, just to prove that I remember how to do recursion.
(defn map3
    "Recursive version of map2"
    [f coll]
    (cond
        (empty? coll) []
        :else (cons (map f (first coll)) (map3 f (rest coll)))
    )
)

;; new variables for shortening up code and for testing
;; sshortauth is shortauth but converted into vectors of words split by spaces
(def sshortauth (map2 towords shortauth))

;; These are sample sentences
(def sents (first sshortauth))
(def sent1 ["dog" "cat" "dog" "bird" "cat" "avocado" "dog" "goat" "alpaca" "cat" "cat" "cat"])


;; new variables! Using "s" suffix to mean "split by spaces"
(def eaps (map #(towords %) eap))
(def mwss (map #(towords %) mws))
(def hpls (map #(towords %) hpl))
(def sauthors [eaps mwss hpls])

;; count average number of words in each sentence
(avg (map #(count (towords %)) eap))

;; another way to do what we just did without using a lambda
(avg (map count (map towords eap)))

;; same as above (count num words on each line) using our new variables
(avg (map count eaps))

;; regexp to try to capture all punctuation. We don't use this but I wanted to try
;; to make a regular expression as a variable.
(def punc #"(\.|;|\?|!|\.|&|\(|\)|,|'|\"|`)+")
(defn ispunc
    "True if a string is made up of only punctuation chars; false otherwise.
This function may not be perfect! If you discover things that don't fit, add them here."
    [word]
    (not (nil? (re-matches #"(\.|;|\?|!|\.|&|\(|\)|,|'|\"|`)+" word)))
)

;; THIS FUNCTION DOES NOT WORK because contains? does not do what it seems like
;; it should do.
;; https://stackoverflow.com/questions/3249334/test-whether-a-list-contains-a-specific-value-in-clojure
;; http://insideclojure.org/2015/01/27/clojure-tips-contains/
(def stopwords (read-file "stopwords.txt"))
(defn bad-is-stop
    "Does not work. Also even if it did, it would be linear time."
    [word]
    (contains? stopwords word)
)

;; Check if word is a stopword
(defn isstop
    "True if a string is a stopword, false otherwise. Constant time!"
    [word]
    (contains? stopset (str/lower-case word))
)

;; filter out all punctuation from one sentence
(map #(remove ispunc %) (first sshortauth))

;; filter out punctuation from all sentences (constant time)
(map2 #(remove ispunc %) sshortauth)

;; filter out punctuation from all sentences (constant time)
(map2 #(remove isstop %) sshortauth)

;; sort the frequencies by the last part of each tuple
(sort-by last (frequencies (first eaps)))

;; we can reverse this
(reverse (sort-by last (frequencies (first eaps))))

;; or, same as above, but in reverse order
(sort-by last > (frequencies (first eaps)))

;; Code for the first one
(map #(apply hash-set %) (take 3 eaps))
 
;; OK, now count the number of sentences that contain the word 'the' at least once
;; by filtering the set using contains?
;; Remember that contains? does what we expect with sets, but does not do what we
;; expect with vectors of sequences
;; https://stackoverflow.com/questions/3249334/test-whether-a-list-contains-a-specific-value-in-clojure
;; http://insideclojure.org/2015/01/27/clojure-tips-contains/
;; https://lambdaisland.com/blog/2017-10-02-clojure-gotchas-associative-contains
(count (filter #(contains? % "the") (map #(apply hash-set %) eaps)))
 
;; Let's turn counting the number of sentences that contain a 
;; given word into a function
(defn num-with-word [word sentences]
   (count (filter #(contains? % word) (map #(apply hash-set %) sentences)))
)
 
;; now write a 2nd function that computes the percentage of sentences 
;; containing a given word
(defn pct-with-word [word sentences]
   (float (/ (num-with-word word sentences) (count sentences)))
)
 
; ;; test out on just sentences by Poe
; (println "percent of sentences in Poe that contain 'the'" (pct-with-word "the" eaps))
 
; ;; Now test out on the sentences of all of the authors
; (println "percent of each of the authors sentences that contain 'the'" (map #(pct-with-word "the" %) (map2 towords authors)))
