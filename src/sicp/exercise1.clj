(ns sicp.exercise1)


(defn A
  "Ackermann function"
  [x y]
  (cond
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (A (- x 1) (A x (- y 1)))))


;;;; 1.11
(defn f-recur
  [n]
  (if (< n 3)
    n
    (+ (f-recur (- n 1))
       (* 2 (f-recur (- n 2)))
       (* 3 (f-recur (- n 3))))))

(defn f-iter
  [n]
  (letfn [(f-iter [a b c n]
            (if (zero? n)
              c
              (recur b
                     c
                     (+ c (* 2 b) (* 3 a))
                     (dec n))))]
    (f-iter 0M 1M 2M (- n 2M))))

(defn exp
  [x n]
  (letfn [(exp-iter [x n acc]
            (if (zero? n)
              acc
              (recur x (dec n) (* x acc))))]
    (exp-iter x n 1)))

(defn square
  [x]
  (* x x))


;;;;; 1.16
(defn fast-exp-recur
  "x^n = (x ^ (n / 2)) ^ 2 if even
   x^n = x * x ^ (n - 1)   if odd"
  [x n]
  (cond
    (zero? n) 1
    (even? n) (square (fast-exp-recur x (/ n 2)))
    :else (* x (fast-exp-recur x (- n 1)))))

(defn fast-exp
  [x n]
  (letfn [(fast-exp-iter [x n acc]
            (cond
              (zero? n) acc
              (even? n) (recur (square x) (/ n 2) acc)
              :else     (recur x (- n 1) (* acc x))))]
    (fast-exp-iter x n 1)))


;;;;;; 1.17
(defn fast-mul
  [a b]
  (letfn [(double [x] (+ x x))
          (halve [x] (/ x 2))
          (fast-mul-iter [a b acc]
            (cond
              (zero? b) acc
              (even? b) (recur (double a) (halve b) acc)
              :else     (recur a (- b 1) (+ acc a))))]
    (fast-mul-iter a b 0)))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (rem a b))))

(defn smallest-divisor
  [n]
  (letfn [(find-divisor
            [n test-divisor]
            (cond
              (> (square test-divisor) n) n
              (divides? test-divisor n) test-divisor
              :else (recur n (next-divisor test-divisor))))
          (next-divisor
            [x]
            (if (= 2 x)
              3
              (+ x 2)))
          (divides?
            [a b]
            (zero? (rem b a)))]
    (find-divisor n 2)))

(defn prime?
  "Order of growth of O(sqrt(n))"
  [n]
  (if (< n 2)
    false
    (= n (smallest-divisor n))))

(defn fermat-test
  [n]
  (letfn [(try-it
            [a]
            (= (expmod a n n) a))
          (expmod
            [base exp m]
            (cond
              (zero? exp) 1
              (even? exp) (rem (square (expmod base (/ exp 2) m))
                               m)
              :else       (rem (* base (expmod base (- exp 1) m))
                               m)))]
    (try-it (rand-int n))))

(defn fast-prime?
  "Order of growth of O(log(n))"
  [n times]
  (cond
    (zero? times) true
    (fermat-test n) (recur n (dec times))
    :else false))


(defn sum-recur
  [term a get-next b]
  (if (> a b)
    0
    (+ (term a)
       (sum-recur term (get-next a) get-next b))))

;;;;;; 1.30
(defn sum
  [term a get-next b]
  (let [iter (fn [a acc]
               (if (> a b)
                 acc
                 (recur (get-next a) (+ acc (term a)))))]
    (iter a 0M)))

;;;;;; 1.31
(defn product
  [term a get-next b]
  (let [iter (fn [a acc]
               (if (> a b)
                 acc
                 (recur (get-next a) (* acc (term a)))))]
    (iter a 1)))

(defn factorial
  ([b]
   (factorial 1 b))
  ([a b]
   (product identity a inc b)))

;;;;;; 1.32
(defn accumulate-iter
  [combiner null-value term a get-next b]
  (let [iter (fn [a acc]
               (if (> a b)
                 acc
                 (recur (get-next a) (combiner acc (term a)))))]
    (iter a null-value)))

(defn accumulate-recur
  [combiner null-value term a get-next b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate-recur combiner null-value term (get-next a) get-next b))))

(defn sum-acc
  ([b]
   (sum-acc 1 b))
  ([a b]
   (accumulate-iter + 0 identity a inc b)))

(defn product-acc
  ([b]
   (product-acc 1 b))
  ([a b]
   (accumulate-iter * 1 identity a inc b)))

;;;;;; 1.33
(defn filtered-accumulate
  [combiner null-value term a get-next b pred]
  (let [iter (fn [a acc]
               (cond
                 (> a b) acc
                 (pred a) (recur (get-next a) (combiner acc (term a)))
                 :else (recur (get-next a) acc)))]
    (iter a null-value)))

(defn sum-sqr-prime
  ([b]
   (sum-sqr-prime 1 b))
  ([a b]
   (filtered-accumulate + 0 square a inc b prime?)))


(defn search
  [f neg-point pos-point]
  (let [midpoint (/ (+ neg-point pos-point) 2)
        close-enough? (fn [x y]
                        (< (- (Math/abs x) y) 0.001))]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond
          (pos? test-value) (recur f neg-point midpoint)
          (neg? test-value) (recur f midpoint pos-point)
          :else midpoint)))))

(defn half-interval-method
  [f a b]
  (let [a-val (f a)
        b-val (f b)]
    (cond
      (and (neg? a-val) (pos? b-val)) (search f a b)
      (and (neg? b-val) (pos? a-val)) (search f b a)
      :else (throw (Exception. (str "Values are not of opposite sign: " a ", " b))))))

(defn average
  [& x]
  (let [n (count x)]
    (if (zero? n)
      0
      (/ (reduce + x)
         n))))

;; Fixed points
(defn fixed-point
  [func first-guess]
  (let [tolerance 0.00001
        close-enough? (fn [val1 val2]
                        (< (Math/abs (- val1 val2))
                           tolerance))
        try-it (fn [guess]
                 (let [next-val (func guess)]
                   (println next-val)
                   (if (close-enough? guess next-val)
                     next-val
                     (recur next-val))))]
    (println "First Guess:")
    (try-it first-guess)))

(average 0)


;;;;;;; 1.35
(defn average-damp
  [f]
  (fn [x] (average x (f x))))

(defn golden-ratio
  "With average damping"
  []
  (fixed-point (average-damp (fn [x] (+ 1 (/ 1 x)))) 1.0))


(defn sqrt
  [x]
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))


(defn fibo
  [n]
  (->> [0 1]
       (iterate (fn [[a b]] [b (+ a b)]))
       (take n)
       (map first)))
