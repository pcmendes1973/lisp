;;;; Solution for Problem 77 in Project Euler
;;;; Paulo Mendes, 30-MAY-2017

(let ((primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109)))
   (defun count-sums (n &optional (remaining-primes primes))
     (let ((current-prime (car remaining-primes)))
      (if (< n current-prime) 0
        (if (= n current-prime) 1
          (+ (count-sums (- n current-prime) remaining-primes)
             (count-sums n (cdr remaining-primes)))))))
