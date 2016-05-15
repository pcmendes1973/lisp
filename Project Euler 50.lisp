;;;; Solution for Problem 50 in Project Euler
;;;; Paulo Mendes, 15-MAY-2016

(defun eratosthenes (n)
"Calculates all primes smaller than n using the sieve of Eratosthenes"
   (let ((sieve (make-array (1- n) :initial-contents (loop for i from 2 to n collect i))))
      (loop for i across sieve
            if i do (loop for j from (expt i 2) to n by i
                          do (setf (aref sieve (- j 2)) Nil)))
    (values (remove-if #'null sieve))))


(defparameter *max-primes* 1000000)

(let ((primes (eratosthenes *max-primes*))
      (largest-prime 2)
      (start 0) (end 0) (sum 2))
   (defun displace ()
   "Displaces the [start,end] interval one unit to the length
    and updates sum appropriately"
       (setf end (1+ end)
             start (1+ start)
             sum (+ sum (aref primes end)
                 (- (aref primes (1- start))))))

   (defun search-primes (n &optional (start 0)
                                     (end (1- (length primes))))
    "Looks for 'n' in 'primes' [start, end] using a binary search.
     Returns n's position in the primes array if it finds 'n', Nil otherwise"
    (let ((start-value (aref primes start))
          (end-value   (aref primes end))
          (interval    (- end start)))
      (cond
        ((= interval 0) 
            (and (= n start-value) start))
        ((= interval 1)
            (or (and (= n start-value) start)
                (and (= n end-value) end)))
        (t
           (let ((split (floor (+ start end) 2)))
              (cond
            ((< n (aref primes split))
              (search-primes n start (1- split)))
              (t
                (search-primes n split end))))))))

(loop for j = 2 then (displace) 
      while (< j *max-primes*)
      maximize (loop for i from (1+ end)
                     for s = (+ sum (aref primes i)) then (+ s (aref primes i))
                    until (> s *max-primes*)
                    when (and (oddp s)              ; If s isn't divisible by 2 ...
                  (> (floor s 3) 0)                 ; ... or by 3, ...
                  (search-primes s i))              ; ... check if it's prime.
                 do (setf largest-prime (max largest-prime s)
                                                     sum s
                                                     end i)
                    finally (return largest-prime))))
