;;;; Line minimization numerical methods
;;;;
;;;; Paulo Mendes, 04-OCT-2015

(defconstant phi 0.618033988749895D0 "Golden ratio")

(defun linemin (f x1 x2 &key (xi1 (- x2 (* (- x2 x1) phi)))
                             (xi2 (+ x1 (* (- x2 x1) phi)))
                             (tol 0.001))
"""Finds minimum of function f in [x1, x2] using the golden mean search.
   * xi1 and xi2 are the internal points inside the [x1, x2] interval
     and are calculated on demand by the recursive calls.
   * Returns the mean of x1 and x2 when x2 - x1 < tol"""
   (if (> tol (- xi2 xi1))
      (/ (+ xi1 xi2) 2)
      (if (> (funcall f x1) (funcall f x2))
         (linemin f xi1 x2 :tol tol :xi1 xi2)
         (linemin f x1 xi2 :tol tol :xi2 xi1))))     

;;; Test function - find the minimum of sin2 x in [-1, 1]
(linemin #'(lambda (x) (expt (sin x) 2)) -1 1)
