;; expect: 1 1
;; Test that float literals are parsed with correct rounding.
;; 0.123456789 must equal 123456789/10^9 (single IEEE 754 division).
;; 0.6931471805599453 (ln 2) must equal 6931471805599453/10^16.
(display (if (= 0.123456789 (/ 123456789.0 1000000000.0)) 1 0))
(display " ")
(display (if (= 0.6931471805599453 (/ 6931471805599453.0 10000000000000000.0)) 1 0))
(newline)
