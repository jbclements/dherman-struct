(module datatype mzscheme
  (require rackunit)
  (require "../../datatype.ss")

  (define-datatype Term
    [Var (name)]
    [Abs (var body)]
    [App (rator rand)])

  ;(provide-datatype Term)
  (provide-datatype/contract Term
    [Var (symbol?)]
    [Abs (symbol? Term?)]
    [App (Term? Term?)])

  (define test-datatype
    (test-suite
     "all datatype.ss tests"
     ))

  (provide test-datatype))
