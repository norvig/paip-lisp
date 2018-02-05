;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File macsymar.lisp: The rewrite rules for MACSYMA in Chapter 8

(requires "macsyma")

(setf *simplification-rules* (mapcar #'simp-rule '(
  (x + 0  = x)
  (0 + x  = x)
  (x + x  = 2 * x)
  (x - 0  = x)
  (0 - x  = - x)
  (x - x  = 0)
  (- - x  = x)
  (x * 1  = x)
  (1 * x  = x)
  (x * 0  = 0)
  (0 * x  = 0)
  (x * x  = x ^ 2)
  (x / 0  = undefined)
  (0 / x  = 0)
  (x / 1  = x)
  (x / x  = 1)
  (0 ^ 0  = undefined)
  (x ^ 0  = 1)
  (0 ^ x  = 0)
  (1 ^ x  = 1)
  (x ^ 1  = x)
  (x ^ -1 = 1 / x)
  (x * (y / x) = y)
  ((y / x) * x = y)
  ((y * x) / x = y)
  ((x * y) / x = y)
  (x + - x = 0)
  ((- x) + x = 0)
  (x + y - x = y)
  )))

(setf *simplification-rules* 
 (append *simplification-rules* (mapcar #'simp-rule
  '((s * n = n * s)
    (n * (m * x) = (n * m) * x)
    (x * (n * y) = n * (x * y))
    ((n * x) * y = n * (x * y))
    (n + s = s + n)
    ((x + m) + n = x + n + m)
    (x + (y + n) = (x + y) + n)
    ((x + n) + y = (x + y) + n)))))

(setf *simplification-rules* 
 (append *simplification-rules* (mapcar #'simp-rule '(
  (log 1         = 0)
  (log 0         = undefined)
  (log e         = 1)
  (sin 0         = 0)
  (sin pi        = 0)
  (cos 0         = 1)
  (cos pi        = -1)
  (sin(pi / 2)   = 1)
  (cos(pi / 2)   = 0)
  (log (e ^ x)   = x)
  (e ^ (log x)   = x)
  ((x ^ y) * (x ^ z) = x ^ (y + z))
  ((x ^ y) / (x ^ z) = x ^ (y - z))
  (log x + log y = log(x * y))
  (log x - log y = log(x / y))
  ((sin x) ^ 2 + (cos x) ^ 2 = 1)
  ))))


(setf *simplification-rules* 
 (append *simplification-rules* (mapcar #'simp-rule '(
  (d x / d x       = 1)
  (d (u + v) / d x = (d u / d x) + (d v / d x))
  (d (u - v) / d x = (d u / d x) - (d v / d x))
  (d (- u) / d x   = - (d u / d x))
  (d (u * v) / d x = u * (d v / d x) + v * (d u / d x))
  (d (u / v) / d x = (v * (d u / d x) - u * (d v / d x)) 
                     / v ^ 2) ; [This corrects an error in the first printing]
  (d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))
  (d (u ^ v) / d x = v * u ^ (v - 1) * (d u / d x)
                   + u ^ v * (log u) * (d v / d x))
  (d (log u) / d x = (d u / d x) / u)
  (d (sin u) / d x = (cos u) * (d u / d x))
  (d (cos u) / d x = - (sin u) * (d u / d x))
  (d (e ^ u) / d x = (e ^ u) * (d u / d x))
  (d u / d x       = 0)))))

        
(integration-table
  '((Int log(x) d x = x * log(x) - x)
    (Int exp(x) d x = exp(x))
    (Int sin(x) d x = - cos(x))
    (Int cos(x) d x = sin(x))
    (Int tan(x) d x = - log(cos(x)))
    (Int sinh(x) d x = cosh(x))
    (Int cosh(x) d x = sinh(x))
    (Int tanh(x) d x = log(cosh(x)))
    ))

;;; Some examples to try (from an integration table):

; (simp '(int sin(x) / cos(x) ^ 2 d x))
; (simp '(int sin(x / a) d x))
; (simp '(int sin(a + b * x) d x))
; (simp '(int sin x * cos x d x))
; (simp '(Int log x / x d x))
; (simp '(Int 1 / (x * log x) d x))
; (simp '(Int (log x) ^ 3 / x d x))
; (simp '(Int exp(a * x) d x))
