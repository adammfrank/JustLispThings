;; if number is greater than 1, loop over all numbers
;; from 2 to the natural square root of number.  If any of the loop
;; numbers are factors of number, number is not a prime and the loop
;; immediately exits
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number  when (primep n) return n))

;; structure of variable definition --> (var init-form step-form)
;; init-form only executes at beginning of loop and binds resutl to
;; var.
;; step-form executes before each subsequent iteration and binds result
;; to var.
;; loop ends if value of var is greater than value of end
;; (defmacro do-primes (var-and-range &rest body)
;;   (let ((var (first var-and-range))
;; 	(start (second var-and-range))
;; 	(end (third var-and-range)))
;;     `(do ((,var (next-prime ,start) (next-primt (1+ ,var))))
;; 	 ((> ,var ,end))
;;        ,@body)))

;; rewriting with destructuring parameter list
;; (defmacro do-primes ((var start end) &body body)
;;   `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;;        ((> ,var ,end))
;;      ,@body))

;; (defmacro do-primes ((var start end) &body body)
;;   `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
;; 	(ending-value ,end))
;;        ((> ,var ending-value))
;;      ,@body))

(defmacro do-primes ((var start end) &body body)
	(let ((ending-value-name (gensym)))
	  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
		(ending-value-name ,end))
	       ((> , ending-value-name))
	     ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
	(with-gensyms (ending-value-name)
	  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
		(ending-value-name ,end))
	       ((> ,var ,ending-value-name))
	     ,@body)))
