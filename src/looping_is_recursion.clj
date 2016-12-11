(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
    (if (= 0 exp)
      acc
      (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [elem tail]
    (if (empty? tail)
      elem
      (recur (first tail) (rest tail))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper
    (fn [s1 s2]
      (cond
        (and (empty? s1) (empty? s2)) true
        (not (= (count s1) (count s2))) false
        (= (first s1) (first s2)) (recur (rest s1) (rest s2))
        :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
      (empty? seq) nil
      (pred (first seq)) index
      :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [sum 0
         seq a-seq]
    (if (empty? seq)
      (/ sum (count a-seq))
      (recur
        (+ sum (first seq))
        (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq a-seq
         map #{}]
    (if (empty? seq)
      map
      (recur (rest seq) (toggle map (first seq))))))

(defn fast-fibo [n]
  (loop [prev 0
         cur 1
         count 1]
    (cond
      (< n 2) n
      (= count n) cur
      :else (recur cur (+ prev cur) (inc count)))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         items (empty #{})
         res []]
    (if (empty? seq)
      res
      (let [contains-el (contains? items (first seq))]
        (recur
          (rest seq)
          (if contains-el items (conj items (first seq)))
          (if contains-el res (conj res (first seq))))))))
