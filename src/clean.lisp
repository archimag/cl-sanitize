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
    (clean node mode))

  (unless (element-allowed-p mode tagname)
    (let ((w (whitespace-element-p mode tagname)))
      (when w
        (xtree:insert-child-before (xtree:make-text " ") element))

      (dolist (node (xtree:all-childs element))
        (xtree:insert-child-before (xtree:detach node) element))
      
      (when w
        (xtree:insert-child-before (xtree:make-text " ") element))
      
      (xtree:remove-child element)
      (return-from clean-element)))
    
  (dolist (attr (xtree:all-attribute-nodes element))
    (unless (attribute-allowed-p mode tagname (xtree:local-name attr))
      (xtree:remove-child attr)))

  (dolist (attr/value (element-additional-attributes mode tagname))
    (setf (xtree:attribute-value element (car attr/value))
          (cdr attr/value))))

      
      

    
     

  



    
    
  

