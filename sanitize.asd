;;;; sanitize.asd
;;;;
;;;; This file is part of the cl-sanitize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defsystem #:sanitize
  :depends-on (#:cl-libxml2)
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "mode" :depends-on ("package"))
                         (:file "clean" :depends-on ("mode"))
                         (:module "modes"
                                  :components
                                  ((:file "basic")
                                   (:file "relaxed")
                                   (:file "restricted"))
                                  :depends-on ("mode"))))))

(defsystem #:sanitize-test
  :depends-on (#:sanitize #:fiveam)
  :components ((:module "t"
                        :components
                        ((:file "suite")))))

(defmethod perform ((o test-op) (c (eql (find-system '#:sanitize))))
  (operate 'load-op '#:sanitize)
  (operate 'test-op '#:sanitize-test))

