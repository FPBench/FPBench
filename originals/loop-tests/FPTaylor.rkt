; FPTaylor
(lambda ([x (< 1.001 default 2.0)] [y (< 1.001 default 2.0)])
  #:name "FPTaylor"
  (let ((t (* x y))
        )
        #:target 
        (/ (- t 1.0)  (- (* t t) 1.0))
  )
)                     
  
  
