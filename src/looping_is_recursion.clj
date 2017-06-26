(ns looping-is-recursion)

(defn power [base exp]
  (if (= 0 exp)
    1
    (let [pow (fn [prod n]
                (if (zero? n)
                  prod
                  (recur (* base prod) (dec n))))]
      (pow base (- exp 1)))))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [last-elem (fn [elems]
                      (if (empty? (rest elems))
                        (first elems)
                        (recur (rest elems))))]
      (last-elem a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (not= (count seq1)(count seq2)) false 
    :else (let [compare (fn [s1 s2]
                          (cond
                            (and (empty? s1)(empty? s2)) true
                            (= (first s1)(first s2)) (recur (rest s1)(rest s2))
                            :else false))]
            (compare seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
      (empty? seq) nil
      (pred (first seq)) index 
      :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [sum 0
         counter 0
         seq a-seq]
    (cond
      (empty? seq) (/ sum counter)
      :else
      (recur (+ sum (first seq))
             (inc counter)
             (rest seq)))))

(defn parity [a-seq]
  (loop [seq a-seq
         parity-seq #{}]
    (let [element (first seq)]
      (cond
        (empty? seq) parity-seq
        (parity-seq element) (recur (rest seq)
                                    (disj parity-seq element))
        (not (parity-seq element)) (recur (rest seq)
                                          (conj parity-seq element))))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn fast-fibo [n]
  (loop [counter 1
         value-0 0
         value-1 1]
    (cond
      (= n 0) value-0
      (= n 1) value-1
      (= counter n) value-1
      :else (recur (inc counter) value-1 (+ value-0 value-1)))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         check-set []]
    (cond
      (empty? seq) check-set
      ((set check-set) (first seq)) check-set
      :else (recur (rest seq) (conj check-set (first seq))))))

