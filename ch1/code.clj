;; Code including snippets stolen from, exercise solutions for,
;; and random walks inspired by Chapter 1 of SICP
;; Author: Kevin A. Archie

(defn sum-of-squares [a b]
  (+ (* a a) (* b b)))

(defn ex1-3 [a b c]
  (cond (and (>= a c) (>= b c)) (sum-of-squares a b)
	(and (>= a b) (>= c b)) (sum-of-squares a c)
	:else (sum-of-squares b c)))

(defn square
  "Square the given number"
  [x]
  (* x x))

(defn mean
  "Compute the mean of the given arguments"
  [& args]
  (/ (apply + args) (count args)))

(defn improve
  "Use Newton's method to make a better guess at the square root"
  [guess x]
  (mean guess (/ x guess)))

(def *good-enough-threshold* 0.000001)

(defn abs [x] (if (< x 0) (- x) x))

(defn sqrt2
  "Compute the square root of x, using Newton's method and
  determining convergence by monitoring the difference between
  successive guesses."
  [x]

  (defn good-enough? [guess last-guess]
    (if (< (abs guess) *good-enough-threshold*)
      (< (abs last-guess) *good-enough-threshold*)
      (< (abs (/ (- guess last-guess) guess))
	 *good-enough-threshold*)))
  
  (defn sqrt-iter [guess last-guess]
    (if (good-enough? guess last-guess)
      guess
      (sqrt-iter (improve guess x) guess)))
  
  (sqrt-iter 1.0 0.0))


(defn sqrt
  "Compute the square root of x, using the Newton-Raphe method and
  determining convergence by monitoring the difference between x and
  the square of the current estimate."
  [x]

  (defn good-enough? [guess x]
    (< (abs (- (square guess) x)) 0.001))

  (defn sqrt-iter [guess]
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x))))

  (sqrt-iter 1.0))

(defn find-fixed-point
  "Find a fixed point of the given function, starting from x"
  [f x]

  (defn good-enough? [x last-x]
    (if (< (abs x) *good-enough-threshold*)
      (< (abs last-x) *good-enough-threshold*)
      (< (abs (/ (- x last-x) x))
	 *good-enough-threshold*)))

  (let [next-x (f x)]
    (if (good-enough? next-x x)
      next-x
      (find-fixed-point f next-x))))

(defn sqrt3 [x]
  (find-fixed-point #(mean %1 (/ x %1)) 1.0))
  
(defn cube-root [x]
  (find-fixed-point #(/ (+ %1 %1 (/ x %1 %1)) 3) 1.0))

(defn cube [x]
  (* x x x))


(defn dot-product [a b]
  (reduce + (map #(* %1 %2) a b)))

(defn norm [a]
  (dot-product a a))

(defn ex1-3-b [& args]
  (norm (take-last 2 (sort args))))

(defn ackermann [x y]
  (cond
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (recur (- x 1)
	     (ackermann x (- y 1)))))

;; ex 1.11

(defn f-ex11-r
  "Recursive solution for exercise 1.11"
  [n]
  (if (< n 3)
    n
    (+ (f-ex11-r (- n 1))
       (* 2 (f-ex11-r (- n 2)))
       (* 3 (f-ex11-r (- n 3))))))

(defn f-ex11-i
  "Iterative solution for exercise 1.11"
  [n]
  (defn recn [to-n f-1 f-2 f-3]
    (let [f (+ f-1 (* 2 f-2) (* 3 f-3))]
      (if (> to-n 0)
	(recn (- to-n 1) f f-1 f-2)
	f)))
  (if (< n 3)
    n
    (recn (- n 3) 2 1 0)))


(def *pascal-triangle-base* '(1))

(defn pascal-i
  "Build a Pascal's triangle in iterative (tail recursive) fashion."
  [n]

  (defn build-row [prev-row acc]
    (if (empty? (rest prev-row))
      (cons (first prev-row) acc)
      (build-row (rest prev-row)
		 (cons (+ (first prev-row) (second prev-row)) acc))))

  (defn build-all [n acc]
    (if (> n 1)
      (build-all (- n 1) (cons (build-row (first acc)
					  (list (ffirst acc)))
			       acc))
      acc))

  (build-all n (list *pascal-triangle-base*)))



(defn pascal
  "Build a Pascal's triangle recursively."
  [n]

  (defn build-row [prev]
    (if (empty? (rest prev))
      prev
      (cons (+ (first prev) (second prev))
	    (build-row (rest prev)))))
  
  (if (> n 1)
    (let [tree (pascal (- n 1))
	  last (first tree)]
      (cons (cons (first last) (build-row last)) tree))
    (list *pascal-triangle-base*)))

(defn pascal-2
  "Build a Pascal's triangle recursively (with different inner loop)."
  [n]

  (defn build-row [p ps]
    (cond
      (nil? p) (cons (first ps) (build-row (first ps) (rest ps)))
      (empty? ps) (list p)
      :else (cons (+ p (first ps))
		  (build-row (first ps) (rest ps)))))

  (if (> n 1)
    (let [tree (pascal-2 (- n 1))
	  last (first tree)]
      (cons (build-row nil last) tree))
    (list *pascal-triangle-base*)))
