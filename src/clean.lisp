;;;; clean

(in-package #:sanitize)

(defgeneric clean (html mode)
  (:documentation "Return sanitize copy of HTML"))

(defmethod clean ((html string) mode)
  (html:with-parse-html-fragment (fragment html)
    (clean fragment mode)
    (html:serialize-html fragment :to-string)))

(defmethod clean ((node xtree:node) mode)
  (case (xtree:node-type node)
    (:xml-document-fragment-node
     (dolist (item (xtree:all-childs node))
       (clean item mode)))
    (:xml-text-node node)
    (:xml-comment-node
     (cond
       ((mode-allow-comments mode) node)
       (t (xtree:remove-child node))))
    (:xml-element-node
     (clean-element node mode))
    (otherwise
     (xtree:remove-child node))))

(defun clean-element (element mode
                      &aux (tagname (xtree:local-name element)))
  (dolist (node (xtree:all-childs element))
    (clean node mode))

  (unless (element-allowed-p mode tagname)
    (let ((fragment (xtree:make-document-fragment)))
      (dolist (node (xtree:all-childs element))
        (xtree:append-child fragment
                            (xtree:detach node)))
      
      (when (whitespace-element-p mode tagname)
        (xtree:prepend-child fragment
                             (xtree:make-text " "))
        (xtree:append-child fragment
                            (xtree:make-text " ")))
      
      (xtree:replace-child element fragment)
      (return-from clean-element fragment)))
    
  (dolist (attr (xtree:all-attribute-nodes element))
    (unless (attribute-allowed-p mode tagname (xtree:local-name attr))
      (xtree:remove-child attr)))

  (dolist (attr/value (element-additional-attributes mode tagname))
    (setf (xtree:attribute-value element (car attr/value))
          (cdr attr/value))))

      
      

    
     

  



    
    
  

