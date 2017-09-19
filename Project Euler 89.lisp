;;;; Solution for Problem 89 in Project Euler
;;;; Paulo Mendes, 19-SEP-2017

(require 'cl-ppcre)

(defconstant *roman-numeral* "(C?M{1,4})|(C?D)|(X?C{1,4})|(X?L)|(I?X{1,4})|(I?V)|(I{1,4})"
"Regular expression to parse Roman numerals")

(defun parse-roman-numeral (value)
   (labels ((parse-letters (str)
      (let ((len (length str)) (first-char (char str 0)))
         (cond
           ((find #\M str) (- (* 1000 len) (if (eq first-char #\C) 1100 0)))
           ((find #\D str) (- 500 (* (1- len) 100)))
           ((find #\C str) (- (* 100 len) (if (eq first-char #\X) 110 0)))
           ((find #\L str) (- 50 (* (1- len) 10)))
           ((find #\X str) (- (* 10 len) (if (eq first-char #\I) 11 0)))
           ((find #\V str) (- 5 (1- len)))
           ((find #\I str) len)))))
     (reduce #'+ (mapcar #'parse-letters
                         (cl-ppcre:all-matches-as-strings *roman-numeral* value)))))


(defun roman-numeral-length (num)
"Returns the length of Roman numeral 'num'"
   (if (>= num 4000)
      (1+ (proper-length (- num 1000)))
     (length (format Nil "~@R" num))))

;;; Reads each numeral from the file, parses the numeral, finds how many redundant characters
;;; the stored number has and counts the excess characters while iterating.
(with-open-file (stream "\\Users\\Paulo Mendes\\Desktop\\Programming\\Lisp\\p089_roman.txt"
                        :direction :input)
   (loop for line = (read-line stream Nil) while line 
         summing (- (length line) (roman-numeral-length (parse-roman-numeral line)))))
