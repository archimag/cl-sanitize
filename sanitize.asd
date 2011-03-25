;;;; sanitize.asd

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
  :depends-on (#:sanitize #:eos)
  :components ((:module "t"
                        :components
                        ((:file "suite")))))

(defmethod perform ((o test-op) (c (eql (find-system '#:sanitize))))
  (operate 'load-op '#:sanitize)
  (operate 'test-op '#:sanitize-test))

