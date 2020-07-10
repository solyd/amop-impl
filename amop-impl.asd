(defsystem "amop-impl"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "amop-impl/tests"))))

(defsystem "amop-impl/tests"
  :author ""
  :license ""
  :depends-on ("amop-impl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for amop-impl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
