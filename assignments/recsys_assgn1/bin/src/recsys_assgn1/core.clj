(ns recsys-assgn1.core
  (:use clojure.test))

;clojure implementation of first programming assignment in the 
;recommender systems class.

(use '(incanter core stats io))

; This is a comma-separated values file, with the user number, movie ID, and rating, in that order.
(def fpath "/home/kiran/studies/recsys/p_assign1/data/recsys-data-ratings.csv")

(def moviespath "/home/kiran/studies/recsys/p_assign1/data/recsys-data-movie-titles.csv")
(def userspath "/home/kiran/studies/recsys/p_assign1/data/recsys-data-users.csv")

(def ratings-dset (read-dataset fpath :header false))
(def movies-dset (read-dataset moviespath :header false))
(def users-dset (read-dataset userspath :header false))

;definition:
;given 2 movies x and y, the score is number of users who rated
;movies x AND y, divided by number of users who rated movie X.
(with-test 
  (defn simscore [mymovie othermovie]
    (let [users-who-rated-my (:col0 (to-map ($where {:col1 mymovie} ratings-dset)))]
      (float (/ 
               (count (clojure.set/intersection 
                        (set users-who-rated-my)                  
                        (set (:col0 (to-map ($where {:col1 othermovie} ratings-dset))))))
               (count users-who-rated-my))))) 
  (is (> (abs (- (simscore 11 603) 0.96)) 0.000001)))

(defn output-score
  [mymovie scorefn]
  (str mymovie "," (clojure.string/join "," 
       (map #(str (first %) "," (format "%.2f" (second %))) 
            (take 5
                 (sort-by second #(> %1 %2)
                 (for [i ($ :col0 movies-dset) :when (not= i mymovie)]
                   [i (scorefn mymovie i)])))))))
(with-test  
  (defn simple-score2 
    [mymovie]
    (output-score mymovie simscore))
  (is (= (simple-score2 11)
         "11,603,0.96,1892,0.94,1891,0.94,120,0.93,1894,0.93")))

(def my-movieids [8358 629 120])
(defn write-output [filename scorefn]
  (spit filename (clojure.string/join "\n"
                                      (for [i my-movieids]
                                        (scorefn i)))))
  
;problem 1
;(write-output "/home/kiran/studies/recsys/p_assign1/ans1.csv" simple-score2)

;advanced formula:
;((x and y) / x) / ((!x and y) / !x)
;where 'x' means count of users who rated movie x
;(x and y): count of users who rated y and y
;(!x and y) count of users who rated y and didn't rate x
;!x count of users who didn't rate movie x. 
(with-test
  (defn complex-score1 
    [myx myy]
    (let [not-inx (clojure.set/difference 
                    (set ($ :col0 users-dset))
                    (set ($ :col0 ($where {:col1 myx} ratings-dset))))
          denomscore (float (/
                              (count 
                                (clojure.set/intersection 
                                  not-inx 
                                  (set ($ :col0 ($where {:col1 myy} ratings-dset)))))
                              (count not-inx)))]
    (/ (simscore myx myy) denomscore)))
  (is (= (- (abs (- (complex-score1 11 1891) 5.69)) 0.01))))

(with-test
  (defn advance-score1 
    [mymovie]
    (output-score mymovie complex-score1))
  (is (= (advance-score1 11)
         "11,1891,5.69,1892,5.65,243,5.00,1894,4.72,2164,4.11")))

;problem 2
;(write-output "/home/kiran/studies/recsys/p_assign1/ans2.csv" advance-score1)