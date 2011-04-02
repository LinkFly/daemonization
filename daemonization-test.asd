(defsystem :daemonization-test
  :depends-on (:daemonization)
  :components ((:module "tests"
			:components ((:file "tests")))))
