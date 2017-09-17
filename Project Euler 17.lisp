;;;; Solution for Problem 17 in Project Euler
;;;; Paulo Mendes, 16-SEP-2017

(+ (loop for i from 1 to 1000                             
         summing (loop for j across (format Nil "~R" i)   ; Generates all strings and
                       counting (alpha-char-p j)))        ; counts the alphabetic chars
   (* 9 99 3))                                            ; Sums the "ands" separately. Necessary for CCL Lisp
