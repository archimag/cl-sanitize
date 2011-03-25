;;;; restricted.lisp

(in-package #:sanitize)

(define-sanitize-mode +restricted+
    :elements ("b" "em" "i" "strong" "u"))
