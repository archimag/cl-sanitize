;;;; sanitize.asd

(defsystem sanitize
  :depends-on (#:cl-libxml2)
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:file "mode" :depends-on ("packages"))
             (:file "clean" :depends-on ("mode"))))))