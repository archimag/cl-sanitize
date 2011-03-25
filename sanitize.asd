;;;; sanitize.asd

(defsystem #:sanitize
  :depends-on (#:cl-libxml2)
  :components ((:module "src"
                        :components
                        ((:file "packages")
                         (:file "mode" :depends-on ("packages"))
                         (:file "clean" :depends-on ("mode"))))))

(defsystem #:sanitize-test
  :depends-on (#:sanitize #:eos)
  :components ((:module "t"
                        :components
                        ((:file "suite")))))

(defmethod perform ((o test-op) (c (eql (find-system '#:sanitize))))
  (operate 'load-op '#:sanitize)
  (operate 'test-op '#:sanitize-test))

