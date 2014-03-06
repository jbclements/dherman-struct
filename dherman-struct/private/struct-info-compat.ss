
;; Compatibility library for mzscheme 3xx and 4.x.
;; The syntax-local-value of a structure type changed in 3.99.


;; ... now, just trying to get it to run with racket 6+. Discarding 
;; compatibility checking.
(module struct-info-compat mzscheme

  (require scheme/struct-info)
  (define (get-struct-predicate type-stx)
    (list-ref (extract-struct-info
               (syntax-local-value type-stx))
              2))
  
  (provide get-struct-predicate))
