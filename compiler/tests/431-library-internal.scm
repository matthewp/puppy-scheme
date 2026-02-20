;; expect: 11
;; Internal helper is not visible, but exported add1 uses it
(import (puppy utils))
(display (add1 10))
