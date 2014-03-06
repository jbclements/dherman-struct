(module struct mzscheme
  (require rackunit)
  (require "../../struct.ss")

  (define-struct/opt normal (a b c))
  (define-struct/opt one-opt (a b [c "default c value"]))
  (define-struct/opt two-opt (a [b "default b value"] [c "default c value"]))
  (define-struct/opt three-opt ([a "default a value"] [b "default b value"] [c "default c value"]))

  (define test-struct
    (test-suite
     "all struct.ss tests"
     (test-case "normal arity" (check-true (and (not (procedure-arity-includes? make-normal 0))
                                                      (not (procedure-arity-includes? make-normal 1))
                                                      (not (procedure-arity-includes? make-normal 2))
                                                      (procedure-arity-includes? make-normal 3)
                                                      (not (procedure-arity-includes? make-normal 4)))))
     (test-case "normal (a)" (check-equal? (normal-a (make-normal 1 2 3)) 1))
     (test-case "normal (b)" (check-equal? (normal-b (make-normal 1 2 3)) 2))
     (test-case "normal (c)" (check-equal? (normal-c (make-normal 1 2 3)) 3))
     (test-case "one opt arity" (check-true (and (not (procedure-arity-includes? make-one-opt 0))
                                                       (not (procedure-arity-includes? make-one-opt 1))
                                                       (procedure-arity-includes? make-one-opt 2)
                                                       (procedure-arity-includes? make-one-opt 3)
                                                       (not (procedure-arity-includes? make-one-opt 4)))))
     (test-case "one opt, all explicit (a)" (check-equal? (one-opt-a (make-one-opt 1 2 3)) 1))
     (test-case "one opt, all explicit (b)" (check-equal? (one-opt-b (make-one-opt 1 2 3)) 2))
     (test-case "one opt, all explicit (c)" (check-equal? (one-opt-c (make-one-opt 1 2 3)) 3))
     (test-case "one opt, last implicit (a)" (check-equal? (one-opt-a (make-one-opt 1 2)) 1))
     (test-case "one opt, last implicit (b)" (check-equal? (one-opt-b (make-one-opt 1 2)) 2))
     (test-case "one opt, last implicit (c)" (check-equal? (one-opt-c (make-one-opt 1 2)) "default c value"))
     (test-case "two opt arity" (check-true (and (not (procedure-arity-includes? make-two-opt 0))
                                                       (procedure-arity-includes? make-two-opt 1)
                                                       (procedure-arity-includes? make-two-opt 2)
                                                       (procedure-arity-includes? make-two-opt 3)
                                                       (not (procedure-arity-includes? make-two-opt 4)))))
     (test-case "two opt, all explicit (a)" (check-equal? (two-opt-a (make-two-opt 1 2 3)) 1))
     (test-case "two opt, all explicit (b)" (check-equal? (two-opt-b (make-two-opt 1 2 3)) 2))
     (test-case "two opt, all explicit (c)" (check-equal? (two-opt-c (make-two-opt 1 2 3)) 3))
     (test-case "two opt, last implicit (a)" (check-equal? (two-opt-a (make-two-opt 1 2)) 1))
     (test-case "two opt, last implicit (b)" (check-equal? (two-opt-b (make-two-opt 1 2)) 2))
     (test-case "two opt, last implicit (c)" (check-equal? (two-opt-c (make-two-opt 1 2)) "default c value"))
     (test-case "two opt, both implicit (a)" (check-equal? (two-opt-a (make-two-opt 1)) 1))
     (test-case "two opt, both implicit (b)" (check-equal? (two-opt-b (make-two-opt 1)) "default b value"))
     (test-case "two opt, both implicit (c)" (check-equal? (two-opt-c (make-two-opt 1)) "default c value"))
     (test-case "three opt arity" (check-true (and (procedure-arity-includes? make-three-opt 0)
                                                         (procedure-arity-includes? make-three-opt 1)
                                                         (procedure-arity-includes? make-three-opt 2)
                                                         (procedure-arity-includes? make-three-opt 3)
                                                         (not (procedure-arity-includes? make-three-opt 4)))))
     (test-case "three opt, all explicit (a)" (check-equal? (three-opt-a (make-three-opt 1 2 3)) 1))
     (test-case "three opt, all explicit (b)" (check-equal? (three-opt-b (make-three-opt 1 2 3)) 2))
     (test-case "three opt, all explicit (c)" (check-equal? (three-opt-c (make-three-opt 1 2 3)) 3))
     (test-case "three opt, last implicit (a)" (check-equal? (three-opt-a (make-three-opt 1 2)) 1))
     (test-case "three opt, last implicit (b)" (check-equal? (three-opt-b (make-three-opt 1 2)) 2))
     (test-case "three opt, last implicit (c)" (check-equal? (three-opt-c (make-three-opt 1 2)) "default c value"))
     (test-case "three opt, two implicit (a)" (check-equal? (three-opt-a (make-three-opt 1)) 1))
     (test-case "three opt, two implicit (b)" (check-equal? (three-opt-b (make-three-opt 1)) "default b value"))
     (test-case "three opt, two implicit (c)" (check-equal? (three-opt-c (make-three-opt 1)) "default c value"))
     (test-case "three opt, all implicit (a)" (check-equal? (three-opt-a (make-three-opt)) "default a value"))
     (test-case "three opt, all implicit (b)" (check-equal? (three-opt-b (make-three-opt)) "default b value"))
     (test-case "three opt, all implicit (c)" (check-equal? (three-opt-c (make-three-opt)) "default c value"))
     ))

  (provide test-struct))