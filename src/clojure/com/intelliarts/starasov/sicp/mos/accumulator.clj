(ns clojure.com.intelliarts.starasov.sicp.mos.accumulator)

"Exercise 3.1.

An accumulator is a procedure that is called repeatedly with a single numeric argument and
accumulates its arguments into a sum. Each time it is called, it returns the currently accumulated sum.
Write a procedure make-accumulator that generates accumulators, each maintaining an independent
sum. The input to make-accumulator should specify the initial value of the sum; for example:

  (define A (make-accumulator 5))
  (A 10)
  15
  (A 10)
  25
"

(defn make-accumulator [initial]
  (def acc (ref initial))
  (fn [x] (dosync (ref-set acc (+ @acc x)))))

"Exercise 3.2.

In software-testing applications, it is useful to be able to count the number of times a given
procedure is called during the course of a computation. Write a procedure make-monitored that takes
as input a procedure, f, that itself takes one input. The result returned by make-monitored is a third
procedure, say mf, that keeps track of the number of times it has been called by maintaining an internal
counter. If the input to mf is the special symbol how-many-calls?, then mf returns the value of the
counter. If the input is the special symbol reset-count, then mf resets the counter to zero. For any
other input, mf returns the result of calling f on that input and increments the counter. For instance, we
could make a monitored version of the sqrt procedure:

  (define s (make-monitored sqrt))
  (s 100)
  10
  (s 'how-many-calls?)
  1
"

(defn make-monitored [fun]
  (def cnt (ref 0))
  (defn dispatch [arg & args]
    (cond
      (= arg :reset-count ) (dosync (ref-set cnt 0))
      (= arg :how-many-times?) @cnt
      :else (dosync
              (ref-set cnt (inc @cnt))
              (apply fun (cons arg args))))))

"Exercise 3.3.

Modify the make-account procedure so that it creates password-protected accounts.
That is, make-account should take a symbol as an additional argument, as in(define acc (make-account 100 'secret-password))
The resulting account object should process a request only if it is accompanied by the password with
which the account was created, and should otherwise return a complaint:

  ((acc 'secret-password 'withdraw) 40)
  60
  ((acc 'some-other-password 'deposit) 50)
  'Incorrect password'
"

"Exercise 3.4.

Modify the make-account procedure of exercise 3.3 by adding another local state
variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it
invokes the procedure call-the-cops."

(defn make-account [initial-balance initial-password call-the-cops]
  (def balance (ref initial-balance))
  (def invalid-tries-count (ref 0))
  (def max-invalid-tries-count 0)

  (defn deposit [amount]
    (dosync (ref-set balance (+ @balance amount))))

  (defn withdraw [amount]
    (if (>= @balance amount)
      (dosync (ref-set balance (- @balance amount)))
      "Insufficient funds"))

  (defn handle-invalid-password []
    (if (> @invalid-tries-count max-invalid-tries-count)
      (call-the-cops)
      (dosync
        (ref-set invalid-tries-count (inc @invalid-tries-count))
        "Incorrect Password")))

  (defn dispatch [op password & args]
    (cond
      (not (= password initial-password)) (handle-invalid-password)
      (= op :balance) @balance
      (= op :deposit) (apply deposit args)
      (= op :withdraw) (apply withdraw args)
      :else (throw (UnsupportedOperationException. (str op))))))
