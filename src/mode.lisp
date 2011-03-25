;;;; mode.lisp
;;;;
;;;; This file is part of the cl-sanitize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:sanitize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sanitize-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sanitize-mode ()
  ((allow-comments      :initform nil
                        :initarg :allow-comments
                        :reader mode-allow-comments)
   (add-attributes      :initform nil
                        :initarg :add-attributes
                        :reader mode-add-attributes)
   (attributes          :initform nil
                        :initarg :attributes
                        :reader mode-attributes)
   (elemetns            :initform nil
                        :initarg :elements
                        :reader mode-elements)
   (protocols           :initform nil
                        :initarg :protocols
                        :reader mode-protocols)
   (whitespace-elements :initform nil
                        :initarg :whitespace-elements
                        :reader mode-whitespace-elements)))

(defmethod shared-initialize :after ((mode sanitize-mode) slot-names &key &allow-other-keys)
  (unless (mode-whitespace-elements mode)
    (setf (slot-value mode 'whitespace-elements)
          (list "address" "article" "aside" "blockquote" "br" "dd" "div" "dl"
                "dt" "footer" "h1" "h2" "h3" "h4" "h5" "h6" "header" "hgroup"
                "hr" "li" "nav" "ol" "p" "pre" "section" "ul"))))

(defun element-allowed-p (mode tagname)
  (member tagname
          (mode-elements mode)
          :test #'string-equal))

(defun attribute-allowed-p (mode tagname attrname)
  (member attrname
          (concatenate 'list
                       (cdr (assoc tagname
                                   (mode-attributes mode)
                                   :test #'string-equal))
                       (cdr (assoc :all
                                   (mode-attributes mode))))
          :test #'string-equal))

(defun whitespace-element-p (mode tagname)
  (member tagname
          (mode-whitespace-elements mode)
          :test #'string-equal))

(defun element-additional-attributes (mode tagname)
  (cdr (assoc tagname
              (mode-add-attributes mode)
              :test #'string-equal)))

(defun element-protocols (mode tagname)
  (cdr (assoc tagname
              (mode-protocols mode)
              :test #'string-equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define-sanitize-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-sanitize-mode (name &key allow-comments add-attributes attributes elements protocols whitespace-elements)
  `(defparameter ,name
     (make-instance 'sanitize-mode
                    :allow-comments ',allow-comments
                    :add-attributes ',add-attributes
                    :attributes ',attributes
                    :elements ',elements
                    :protocols ',protocols
                    :whitespace-elements ',whitespace-elements)))

(define-sanitize-mode +default+)


 