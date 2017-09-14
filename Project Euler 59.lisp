;;;; Solution for Problem 59 in Project Euler. 
;;;; Paulo Mendes, 14-SEP-2017

(require 'cl-ppcre)

(defconstant *cypher*
"Loads the cypher into memory and converts everything to integer"
(with-open-file (stream "\\Users\\Paulo Mendes\\Desktop\\Programming\\Lisp\\p059_cipher.txt"
                :direction :input)
    (mapcar 'parse-integer (cl-ppcre:split #\, (read-line stream Nil)))))

(defun decypher-all (passwd cypher)
"General purpose decyphering. Password must be a list of 3 char code values."
   (loop for i in (setf (cdr (last passwd)) passwd)  ; Circular list.
         for j in cypher
         collecting (code-char (logxor i j))))

;;; Iterates through all possible passwords and shortlists decodings
;;; that yield only numbers between 32 and 122. There are 6 in all.
(loop for i from (char-code #\a) to (char-code #\z)
      nconcing (loop for j from (char-code #\a) to (char-code #\z)
         nconcing  (loop for k from (char-code #\a) to (char-code #\z)
                           for msg = (decypher-all (list i j k) *cypher*)
                           when (every #'(lambda (x) (<= 32 (char-code x) 122)) msg)
                              collect (list (list i j k) msg))))

;;; After inspecting the above visually, find the password and sum the characters in the
;;; decyphered output to obtain the result.
(reduce #'+ (mapcar #'char-code (decypher-all (list 103 111 100) *cypher*)))
