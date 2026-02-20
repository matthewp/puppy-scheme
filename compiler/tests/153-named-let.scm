;; expect: 120
(display
  (let fact ((n 5) (acc 1))
    (if (= n 0) acc
        (fact (- n 1) (* acc n)))))
