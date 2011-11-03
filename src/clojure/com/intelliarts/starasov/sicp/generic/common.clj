(ns clojure.com.intelliarts.starasov.sicp.generic.common)

(def -op-registry (ref {}))
(def -coertion-registry (ref {}))

(defn register-impl [registry op type item]
  (let [entry (@registry op)]
    (if (nil? entry)
      (dosync
        (alter registry assoc op (ref {}))
        (register-impl registry op type item))
      (dosync (alter entry assoc type item)))))

(defn lookup-impl [registry op type]
  (let [entry (@registry op)]
    (if (not (nil? entry))
      (let [op-fn (entry type)]
        (if (not (nil? op-fn))
          op-fn)))))

(defn attach-tag [tag contents]
  (cons tag contents))

(defn type-tag [data]
  (cond
    (number? data) :scheme-number
    (and (seq? data) (keyword? (first data))) (first data)
    :else (throw (IllegalStateException. (str "[type-tag] Data - " data " is not tagged.")))))

(defn contents [data]
  (cond
    (number? data) data
    (and (seq? data) (keyword? (first data))) (rest data)
    :else (throw (IllegalStateException. (str "[contents] Data - " data " is not tagged.")))))

(defn register [op type item]
  (register-impl -op-registry op type item))

(defn lookup [op type]
  (lookup-impl -op-registry op type))

(defn register-coertion [from to op]
  (register-impl -coertion-registry from to op))

(defn lookup-coertion [from to]
  (lookup-impl -coertion-registry from to))

(defn lookup-proc [op args]
  (let [type-tags (map type-tag args) proc (lookup op type-tags)]
    proc))

"Exercise 2.81.

Louis Reasoner has noticed that apply-generic may try to coerce the arguments to
each other's type even if they already have the same type. Therefore, he reasons, we need to put procedures
in the coercion table to `coerce` arguments of each type to their own type. For example, in addition to the
scheme-number->complex coercion shown above, he would do:

  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)

  (put-coercion 'scheme-number 'scheme-number
                scheme-number->scheme-number)

  (put-coercion 'complex 'complex complex->complex)

a. With Louis's coercion procedures installed, what happens if apply-generic is called with two
arguments of type scheme-number or two arguments of type complex for an operation that is not
found in the table for those types? For example, assume that we've defined a generic exponentiation
operation:

  (define (exp x y) (apply-generic 'exp x y))

and have put a procedure for exponentiation in the Scheme-number package but not in any other package:

  ;; following added to Scheme-number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt

What happens if we call exp with two complex numbers as arguments?

b. Is Louis correct that something had to be done about coercion with arguments of the same type, or does
apply-generic work correctly as is?

c. Modify apply-generic so that it doesn't try coercion if the two arguments have the same type."

(defn coerce-dyadic [arg1 arg2]
  (let [type1 (type-tag arg1)
        type2 (type-tag arg2)
        type1->type2 (lookup-coertion type2 type1)
        type2->type1 (lookup-coertion type1 type2)]
    (cond
      (= type1 type2) nil
      (not (nil? type1->type2)) [(type1->type2 (contents arg1)) arg2]
      (not (nil? type2->type1)) [arg1 (type2->type1 (contents arg2))])))

"Exercise 2.82.

Show how to generalize apply-generic to handle coercion in the general case of
multiple arguments. One strategy is to attempt to coerce all the arguments to the type of the first argument,
then to the type of the second argument, and so on. Give an example of a situation where this strategy (and
likewise the two-argument version given above) is not sufficiently general. (Hint: Consider the case where
there are some suitable mixed-type operations present in the table that will not be tried.)"

(defn coerce-to-first [first-arg args]
  (if (empty? args)
    '()
    (let [type1 (type-tag first-arg)
          type2 (type-tag (first args))
          type2->type1 (lookup-coertion type1 type2)]
      (cond
        (= type1 type2) (cons (first args) (coerce-to-first first-arg (rest args)))
        (not (nil? type2->type1)) (cons (type2->type1 (contents (first args))) (coerce-to-first first-arg (rest args)))
        :else nil))))

"Exercise 2.84.

Using the raise operation of exercise 2.83, modify the apply-generic procedure so
that it coerces its arguments to have the same type by the method of successive raising, as discussed in this
section. You will need to devise a way to test which of two types is higher in the tower. Do this in a
manner that is ``compatible'' with the rest of the system and will not lead to problems in adding new levels
to the tower."

(defn tower-level [arg]
  (defn- iter [arg level]
    (let [raise-op (lookup-proc :raise [arg])]
      (if (not (nil? raise-op))
        (iter (raise-op (contents arg)) (inc level))
        level)))
  (iter arg 0))

(defn map-tower-level [args]
  (map (fn [arg] [(tower-level arg) arg]) args))

(defn max-tower-level [max mapped-levels]
  (if (empty? mapped-levels)
    max
    (if (> (first max) (first (first mapped-levels)))
      (max-tower-level (first mapped-levels) (rest mapped-levels))
      (max-tower-level max (rest mapped-levels)))))

(defn coerce-levels [max mapped-levels]
  (defn- raise-n [arg n]
    (if (= n 0)
      arg
      (raise-n ((lookup-proc :raise [arg]) (contents arg)) (dec n))))
  (defn- coerce [m]
    (let [arg (second m)
          diff (- (first m) (first max))]
      (raise-n arg diff)))
  (map coerce mapped-levels))

(defn coerce-tower [args]
  (let [mapped-levels (map-tower-level args)
        max-level (max-tower-level (first mapped-levels) mapped-levels)
        coerced-levels (coerce-levels max-level mapped-levels)]
    coerced-levels))

(defn apply-generic [op & args]
  (let [proc (lookup-proc op args)]
    (cond
      (not (nil? proc)) (apply proc (map contents args))
      (and (nil? proc) (= 1 (count args))) nil
      :else
      (let [coerced-args (coerce-tower args)]
        (println coerced-args)
        (if (= (count coerced-args) (count args))
          (apply apply-generic op coerced-args))))))