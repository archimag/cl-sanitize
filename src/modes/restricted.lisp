;;;; restricted.lisp
;;;;
;;;; This file is part of the cl-sanitize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:sanitize)

(define-sanitize-mode +restricted+
    :elements ("b" "em" "i" "strong" "u"))
