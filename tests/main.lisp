(defpackage amop-impl/tests/main
  (:use :cl
        :amop-impl
        :rove))
(in-package :amop-impl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :amop-impl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
