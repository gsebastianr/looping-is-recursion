(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [b e acc]
                 (if (zero? e)
                   acc
                   (recur b (- e 1) (* b acc))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (last-element (rest a-seq)))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1)
        (empty? seq2)) true
   (or  (empty? seq1)
        (empty? seq2)) false
   :else (if (= (first seq1) (first seq2))
           (seq= (rest seq1) (rest  seq2))
           false)))


(defn find-first-index [pred a-seq]
  (loop [p     pred
         asq   a-seq
         index 0]
    (if (empty? asq)
      nil
      (if (p (first asq))
        index
        (recur p (rest asq) (inc index))))))

(defn avg [a-seq]
  (loop [asq a-seq
         cnt 0
         sum 0]
    (if (empty? asq)
      (/ sum cnt)
      (recur (rest asq) (inc cnt) (+ sum (first asq))))))

(defn parity [a-seq]
  (loop [sett #{}
         asq  a-seq]
    (if (empty? asq)
      sett
      (if (contains? sett (first asq))
        (recur (disj sett (first asq)) (rest asq))
        (recur (conj sett (first asq)) (rest asq))))))

(defn fast-fibo [n]
  (loop [stl 0
         lst 1
         m   n]
    (if (zero? m)
      stl
      (recur lst (+ stl lst) (dec m)))))

(defn cut-at-repetition [a-seq]
  (loop [vect []
         ast  #{}
         asq  a-seq]
    (if (empty? asq)
      vect
      (if (contains? ast (first asq))
        vect
        (recur (conj vect (first asq)) (conj ast (first asq)) (rest asq))))))


