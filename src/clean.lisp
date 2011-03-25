;;;; clean
;;;;
;;;; This file is part of the cl-sanitize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:sanitize)

(defun clean (html &optional (mode +default+))
  "Return sanitize copy of HTML"
  (html:with-parse-html (doc (format nil "<div>~A</div>" html))
    (let ((entry (xtree:first-child (xtree:first-child (xtree:root doc)))))
      (xtree:with-object (fragment (xtree:make-document-fragment doc))
        (dolist (item (xtree:all-childs entry))
          (xtree:append-child fragment
                              (xtree:detach item)))
        (clean-node fragment mode)
        (html:serialize-html fragment :to-string)))))

(defun clean-node (node mode)
  (case (xtree:node-type node)
    (:xml-document-fragment-node
     (dolist (item (xtree:all-childs node))
       (clean-node item mode)))
    (:xml-text-node node)
    (:xml-cdata-section-node
     (xtree:replace-child node
                          (xtree:make-text (xtree:text-content node))))
    (:xml-comment-node
     (cond
       ((mode-allow-comments mode) node)
       (t (xtree:remove-child node))))
    (:xml-element-node
     (clean-element node mode))
    (otherwise
     (print (xtree:node-type node))
     (print (xtree:text-content node))
     (xtree:remove-child node))))

(defun clean-element (element mode
                      &aux (tagname (xtree:local-name element)))
  (dolist (node (xtree:all-childs element))
    (clean-node node mode))

  (unless (element-allowed-p mode tagname)
    (let ((w (whitespace-element-p mode tagname))
          (child (xtree:all-childs element)))
      (when w
        (xtree:insert-child-before (xtree:make-text " ") element))

      (dolist (node child)
        (xtree:insert-child-before (xtree:detach node) element))
      
      (when (and w child)
        (xtree:insert-child-before (xtree:make-text " ") element))
      
      (xtree:remove-child element)
      (return-from clean-element)))
    
  (dolist (attr (xtree:all-attribute-nodes element))
    (unless (attribute-allowed-p mode tagname (xtree:local-name attr))
      (xtree:remove-child attr)))

  (let ((attr-protocols (element-protocols mode tagname)))
    (dolist (attr (xtree:all-attribute-nodes element))
      (let ((protocols (assoc (string-downcase (xtree:local-name attr))
                              attr-protocols
                              :test #'string-equal)))
        (when (and protocols
                   (not (ignore-errors (member (or (puri:uri-scheme (puri:parse-uri (xtree:text-content attr)))
                                                   :relative)
                                               protocols))))
          (xtree:remove-child attr)))))
          
  (dolist (attr/value (element-additional-attributes mode tagname))
    (setf (xtree:attribute-value element (car attr/value))
          (cdr attr/value))))
 