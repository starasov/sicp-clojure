(ns clojure.com.intelliarts.starasov.sicp.symbolic-differentiation)

(defn variable? [e]
  (symbol? e))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn same-number? [n1 n2]
  (and (number? n1) (number? n2) (= n1 n2)))

(defn sum? [e]
  (and (list? e) (= (first e) '+)))

(defn addend [sum]
  (second sum))

(defn augend [sum]
  (last sum))

(defn make-sum [a1 a2]
  (cond
    (same-number? a1 0) a2
    (same-number? a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))

(defn product? [e]
  (and (list? e) (= (first e) '*)))

(defn multiplier [prod]
  (second prod))

(defn multiplicand [prod]
  (last prod))

(defn make-product [a1 a2]
  (cond
    (or (same-number? a1 0) (same-number? a2 0)) 0
    (same-number? a1 1) a2
    (same-number? a2 1) a1
    :else (list '* a1 a2)))

"Exercise 2.56.

Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the
differentiation rule by adding a new clause to the deriv program and defining appropriate procedures exponentiation?,
base, exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in the rules that
anything raised to the power 0 is 1 and anything raised to the power
1 is the thing itself."

(defn exponentiation? [e]
  (and (list? e) (= (first e) '**)))

(defn make-exponentiation [base exp]
  (cond
    (same-number? exp 0) 1
    (same-number? exp 1) base
    :else (list '** base exp)))

(defn base [e]
  (second e))

(defn exponent [e]
  (last e))

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp var) 1 0)
    (sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
    (product? exp) (make-sum
                     (make-product (multiplier exp) (deriv (multiplicand exp) var))
                     (make-product (multiplicand exp) (deriv (multiplier exp) var)))
    (exponentiation? exp) (make-product
                            (make-product (exponent exp) (make-exponentiation (base exp) (dec (exponent exp))))
                            (deriv (base exp) var))
    :else (throw (IllegalStateException. (str "Unsupported expression: " exp)))))

(println (deriv '(* x x x) 'x))

"Exercise 2.57.

Extend the differentiation program to handle sums and products of arbitrary numbers of (two or more) terms. Then the
last example above could be expressed as

(deriv '(* x y (+ x 3)) 'x)

Try to do this by changing only the representation for sums and products, without changing the deriv
procedure at all. For example, the addend of a sum would be the first term, and the augend would be the
sum of the rest of the terms."

(defn augend-ext [sum]
  (let [a (rest (rest sum)) a1 (first a) a2 (rest a)]
    (if (empty? a2) a1 (make-sum a1 (augend (rest sum))))))

(defn multiplicand-ext [prod]
  (let [m (rest (rest prod)) a1 (first m) a2 (rest m)]
    (if (empty? a2) a1 (make-product a1 (multiplicand-ext (rest prod))))))

"Exercise 2.58.

Suppose we want to modify the differentiation program so that it works with ordinary
mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation
program is defined in terms of abstract data, we can modify it to work with different representations of
expressions solely by changing the predicates, selectors, and constructors that define the representation of
the algebraic expressions on which the differentiator is to operate.

a. Show how to do this in order to differentiate algebraic expressions presented in infix form, such as (x +
(3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two arguments and
that expressions are fully parenthesized."

(defn sum?-ord [e]
  (and (list? e) (= (second e) '+)))

(defn addend-ord [sum]
  (first sum))

(defn augend-ord [sum]
  (last sum))

(defn make-sum-ord [a1 a2]
  (cond
    (same-number? a1 0) a2
    (same-number? a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list a1 '+ a2)))

(defn product?-ord [e]
  (and (list? e) (= (second e) '*)))

(defn multiplier-ord [prod]
  (first prod))

(defn multiplicand-ord [prod]
  (last prod))

(defn make-product-ord [a1 a2]
  (cond
    (or (same-number? a1 0) (same-number? a2 0)) 0
    (same-number? a1 1) a2
    (same-number? a2 1) a1
    :else (list a1 '* a2)))
