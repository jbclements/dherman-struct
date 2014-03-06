(module tests mzscheme
  (require rackunit
           rackunit/text-ui)
  (require "datatype.ss")
  (require "hierarchy.ss")
  (require "class.ss")
  (require "struct.ss")

  (define all-tests
    (test-suite
     "all struct.plt tests"
     test-hierarchy
     test-datatype
     test-class
     test-struct))

  (run-tests all-tests)

  (provide all-tests))
