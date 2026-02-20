;; expect: 13
;; Import from multiple libraries
(import (puppy utils))
(import (puppy core))
(display (+ (add1 2) (double 5)))
