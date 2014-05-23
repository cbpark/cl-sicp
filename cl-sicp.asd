(in-package :cl-user)
(defpackage cl-sicp-asd
  (:use :cl :asdf))
(in-package :cl-sicp-asd)

(defsystem cl-sicp
  :version "0.1.0.0"
  :author "Chan Beom Park"
  :license "BSD"
  :depends-on ()
  :components ((:module "src"
                        :components
                        ((:file "chapter1")
                         (:file "chapter1-exercise"
                                :depends-on ("chapter1"))
                         (:file "chapter2"
                                :depends-on ("chapter1"))
                         (:file "chapter2-exercise"
                                :depends-on ("chapter1"
                                             "chapter2"))
                         (:file "chapter3")
                         (:file "chapter3-exercise"
                                :depends-on ("chapter3")))))
  :description "Codes from Structure and Interpretation of Computer Programs in Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
