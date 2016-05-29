;;;; Solution for Problem 49 in Project Euler
;;;; Paulo Mendes, 28-MAY-2016


(defun is-prime? (n)
   (loop for i from 2 to (isqrt n)
         never (zerop (mod n i))))

(loop with primes = Nil                    ; Start with the 1st prime.
      for i from 3 by 2                    ; Iterate over odd numbers.
      if (is-prime? i) do (push i primes)  ; Save primes
      else do (and (loop for j in primes   ; Test 'i' vs. 'primes'; return if # is found.
                         for k = (/ (- i j) 2)
                         never (= (expt (isqrt k) 2) k)) (return i)))
