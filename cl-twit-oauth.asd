#|
  This file is a part of cl-twit-oauth project.
|#

(in-package :cl-user)
(defpackage cl-twit-oauth-asd
  (:use :cl :asdf))
(in-package :cl-twit-oauth-asd)

(defsystem cl-twit-oauth
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:cl-oauth
               :clack
               :ningle
               :cl-who
               :cl-ppcre
               :cl-json
               :local-time
               :cl-unicode
               :uiop
               :cl-store)
  :components ((:module "src"
                :components
                ((:file "cl-twit-oauth"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-twit-oauth-test))))
