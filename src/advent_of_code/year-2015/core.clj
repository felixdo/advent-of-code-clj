(ns advent-of-code.year-2015.core
  (:require [clojure.java.io :as io]))

(comment "Day1 Part 1"
         (let [data (slurp (io/resource "2015/day1-part1.txt"))]
           (reduce
            (fn [r n]
              (condp = n
                \( (inc r)
                \) (dec r)))
            0
            data)))

(comment
  "Day1 Part 2"
  (let [data (slurp (io/resource "2015/day1-part1.txt"))]
    (loop [r {:floor 0 :pos 1}
           d data]
      (prn r)
      (if (= -1 (:floor r))
        (dec (:pos r))
        (recur
         (-> r
             (update :floor (condp = (first d)
                              \( inc
                              \) dec))
             (update :pos inc))
         (rest d))))))


(comment
  "Day 2 Part 1"
  (defn order-wrapping-paper [s]
    (let [[l w h] (map #(Integer/parseInt %) (.split s "x"))
          sideA (* l w)
          sideB (* w h)
          sideC (* h l)
          slack (min sideA sideB sideC)
          ]
      (+
       (* 2 sideA)
       (* 2 sideB)
       (* 2 sideC)
       slack)
      ))
  (order-wrapping-paper "3x11x24")
  (with-open [rdr (clojure.java.io/reader (io/resource "2015/day2.txt"))]
    (reduce + (map order-wrapping-paper (line-seq rdr))))
  )

(comment
  "Day 2 Part 2"
  (defn order-ribbon [s]
    (let [[l w h] (map #(Integer/parseInt %) (.split s "x"))
          p1 (+ l l w w)
          p2 (+ l l h h)
          p3 (+ w w h h)
          pm (min p1 p2 p3)
          vol (* l w h)
          ]
      (+ pm vol)
      ))
  (with-open [rdr (clojure.java.io/reader (io/resource "2015/day2.txt"))]
    (reduce + (map order-ribbon (line-seq rdr))))
  )


(comment
  "Day 3 Part 1"

  (defn right [pos]
    (update pos :x inc))

  (defn left [pos]
    (update pos :x dec))

  (defn up [pos]
    (update pos :y inc))

  (defn down [pos]
    (update pos :y dec))

  (count (loop [pos {:x 0 :y 0}
         visited #{pos}
         data (slurp (io/resource "2015/day3.txt"))]
    (if (seq data)
      (recur
       ((condp = (first data)
          \> right
          \< left
          \v down
          \^ up)
        pos)
       (conj visited pos)
       (rest data))
      (conj visited pos))))
  )

(comment
  "Day 3 Part 2"

  (defn right [pos]
    (update pos :x inc))

  (defn left [pos]
    (update pos :x dec))

  (defn up [pos]
    (update pos :y inc))

  (defn down [pos]
    (update pos :y dec))

  (count
   (loop [pos {:santa {:x 0 :y 0} 
               :robot {:x 0 :y 0}}
          turns (cycle [:santa :robot])
          visited #{}
          data (slurp (io/resource "2015/day3.txt"))]
     (if (seq data)
       (recur
        (update pos (first turns)
                (condp = (first data)
                  \> right
                  \< left
                  \v down
                  \^ up))
        (rest turns)
        (conj visited (:santa pos) (:robot pos))
        (rest data))
       visited
       )))


)