;;;; sanitize.asd

(defsystem sanitize
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:file "mode" :depends-on ("packages"))))))