(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (if(> 3 (count v))
    false
    (+ (get v 0) (get v 2))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if(> 3 (count v))
    false
    (+ (get v 0) (get v 2))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)))

(defn square? [rectangle]
  (if(= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [x y] point]
   (if(and (<= x1 x x2) (<= y1 y y2)) ;error on "and"
    true
    false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1o y1o] [x2o y2o]] outer
    [[x1i y1i] [x2i y2i]] inner]
   (if(and (<= x1o x1i x2i x2o) (<= y1o y1i y2i y2o))
    true
    false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if(< 1 (author-count book))
    true
    false))

(defn add-author [book new-author]
  (let [updated-authors (conj (:authors book) new-author)]
    (assoc book :authors updated-authors)))
    ;assoc? doesnt seem to need. may error.

(defn alive? [author]
  (if(contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection)) ;syntax/return type error?

(defn second-elements [collection]
  (map #(first(rest %)) collection))

(defn titles [books]
  (map (fn [book] (:title book)) books))

(defn monotonic? [a-seq]
  (if(apply <= a-seq)
    true
    (if(apply >= a-seq)
      true
      false)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if(= (count (set a-seq)) (count a-seq))
        false
        true))

(def a-china {:name "China Miéville", :birth-year 1972})
(def a-octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def a-friedman {:name "Daniel Friedman" :birth-year 1944})
(def a-felleisen {:name "Matthias Felleisen"})

(def b-cities {:title "The City and the City" :authors [a-china]})
(def b-wild-seed {:title "Wild Seed", :authors [a-octavia]})
(def b-embassytown {:title "Embassytown", :authors [a-china]})
(def b-little-schemer {:title "The Little Schemer"
                     :authors [a-friedman, a-felleisen]})

(def test-authors [a-china a-friedman a-felleisen a-octavia])

(def test-books [b-cities b-wild-seed b-embassytown b-little-schemer])

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))
(defn has-author? [book author]
  (if(contains? (:authors book) author)
  true
  false))

(defn authors [books]
  ;(println (str "authors: Here are the books:" books))
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  ;(println (str "author names are: " (set (mapv :name (authors books)))))
  (set (map :name (authors books))))

(defn alive-years [author]
  (cond 
    (not (contains? author :birth-year)) ""
    (not (contains? author :death-year)) (str " (" (:birth-year author) " - )")
    :else (str " (" (:birth-year author) " - " (:death-year author) ")")))

(defn create-author-name [author]
  (str (:name author)))

(defn author->string [author]
   (let [author-name (create-author-name author)
        years (alive-years author)]
    (str author-name years))) ;

(defn authors->string [authors]
;  (str (mapv #(interpose ", " (map )) authors)))
 (apply str (interpose ", " (mapv author->string authors))))
;"not yet")

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  ;(map #(str (book->string %) ", ") books))
  (cond
  (= (count books) 0) "No books."
  (= (count books) 1) (str "1 book. " (book->string (first books)) ".")
  (> (count books) 1) (str (count books) " books. " (apply str (interpose ". " (mapv book->string books)))  ".")
  ))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (let [found-authors (filter #(= (str (:name %)) name) authors)]
    (if (empty? found-authors)
      nil
      (nth found-authors 0)
    )))
  
;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})
;
;(def silmarillion {:title "Silmarillion"
;                   :authors #{jrrtolkien, christopher, kay}})
;
;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})
;
;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (if(empty? (living-authors (:authors book)))
    false
    true))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
