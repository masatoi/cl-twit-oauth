#|
  This file is a part of cl-twit-oauth project.
|#

(in-package :cl-user)
(defpackage cl-twit-oauth-test-asd
  (:use :cl :asdf))
(in-package :cl-twit-oauth-test-asd)

(defsystem cl-twit-oauth-test
  :author ""
  :license ""
  :depends-on (:cl-twit-oauth
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-twit-oauth"))))
  :description "Test system for cl-twit-oauth"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
