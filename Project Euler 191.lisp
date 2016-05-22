;;;; Solution for Problem 191 in Project Euler
;;;; Paulo Mendes, 21-MAY-2016

(let ((memo (make-array '(2 2 2 2 30)
                        :initial-element Nil)))
   (defun update-list (new-element lst)
       (cons new-element (if (= (length lst) 3) (butlast lst) lst)))

   (defun count-permutations (n &key previous-days late-once)
     (let* ((1st-digit (if (eq 'A (car previous-days))   1 0)) ;; Calculates 1st
            (2nd-digit (if (eq 'A (cadr previous-days))  1 0)) ;; 4 digits for 
            (3rd-digit (if (eq 'A (caddr previous-days)) 1 0)) ;; reading/writing
            (4th-digit (if late-once 1 0))                     ;; into 5D memo array
            (cannot-miss-class? (and (eq 'A (car previous-days))
                                     (eq 'A (cadr previous-days)))))
     (cond
       ((aref memo 1st-digit 2nd-digit 3rd-digit 4th-digit (1- n)))
       ((= n 1)
          (+ (if late-once 0 1)
             (if cannot-miss-class? 0 1) 1))
       (t
         (setf (aref memo 1st-digit 2nd-digit 3rd-digit 4th-digit (1- n))
           (+ (count-permutations (1- n)
                                  :previous-days (update-list 'O previous-days)
                                  :late-once late-once)
           (if late-once 0
              (count-permutations (1- n)
                                  :previous-days (update-list 'L previous-days)
                                  :late-once T))
           (if cannot-miss-class? 0
              (count-permutations (1- n)
                                  :previous-days (update-list 'A previous-days)
                                  :late-once late-once))))))))
   (count-permutations 30))
