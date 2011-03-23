;;;; packages.lisp

(defpackage #:sanitize
  (:use #:cl)
  (:export #:sanitize-mode
           #:define-sanitize-mode
           #:+default+
           #:+basic+
           #:+relaxed+
           #:+restricted+))

