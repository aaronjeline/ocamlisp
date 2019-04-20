(func (fib a) 
      (if (< a 3) 1 (+ (fib (+ a -1)) (fib (+ a -2)))))

(func (main) (fib 5))
