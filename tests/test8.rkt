(let ([f1 (future (lambda () (+ 1 2)))])
  (let ([f2 (future (lambda () (+ 3 4)))])
    (* (touch f1) (touch f2))))
