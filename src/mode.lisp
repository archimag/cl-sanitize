;;;; mode.lisp

(in-package #:sanitize)

(defclass sanitize-mode ()
  ((allow-comments :initarg :allow-comments :reader mode-allow-comments)
   (add-attributes :initarg :add-attributes :reader mode-add-attributes)
   (attributes :initarg :attributes :reader mode-attributes)
   (elemetns :initarg :elements :reader mode-elements)
   (output-format :initarg :output-format :reader mode-output-format)
   (protocols :initarg :protocols :reader mode-protocols)
   (whitespace-elements :initarg :whitespace-elements :reader mode-whitespace-elements)))

(defmacro define-sanitize-mode (name &key inherit
                                allow-comments
                                add-attributes
                                attributes
                                elements
                                output-format
                                protocols
                                whitespace-elements)
  (declare (ignore inherit))
  `(defparameter ,name
     (make-instance 'sanitize-mode
                    :allow-comments ',allow-comments
                    :add-attributes ',add-attributes
                    :attributes ',attributes
                    :elements ',elements
                    :output-format ',output-format
                    :protocols ',protocols
                    :whitespace-elements ',whitespace-elements)))

(define-sanitize-mode +default+
    :allow-comments nil
    :add-attributes nil
    :attributes nil
    :elements #()
    :output-format :html
    :protocols nil
    :whitespace-elements #(("address" "article" "aside" "blockquote" "br" "dd" "div" "dl" "dt" "footer"
                            "h1" "h2" "h3" "h4" "h5" "h6" "header" "hgroup" "hr" "li" "nav" "ol" "p" "pre"
                            "section" "ul")))

(define-sanitize-mode +basic+
    :inherit +default+
    
    :elements #("a" "abbr" "b" "blockquote" "br" "cite" "code" "dd" "dfn" "dl" "dt" "em" "i"
                "kbd" "li" "mark" "ol" "p" "pre" "q" "s" "samp" "small" "strike" "strong"
                "sub" "sup" "time" "u" "ul" "var")

    :attributes (("a"          . ("href"))
                 ("abbr"       . ("title"))
                 ("blockquote" . ("cite"))
                 ("dfn"        . ("title"))
                 ("q"          . ("cite"))
                 ("time"       . ("datetime" "pubdate")))

    :add-attributes (("a" . (("rel" . "nofollow"))))

    :protocols (("a"           . (("href" . ("ftp" "http" "https" "mailto" :relative))))
                ("blockquote"  . (("cite" . ("http" "https" :relative))))
                ("q"           . (("cite" . ("http" "https" :relative))))))


(define-sanitize-mode +relaxed+
    :inherit +default+
    
    :elements #("a" "abbr" "b" "bdo" "blockquote" "br" "caption" "cite" "code" "col"
                "colgroup" "dd" "del" "dfn" "dl" "dt" "em" "figcaption" "figure" "h1" "h2"
                "h3" "h4" "h5" "h6" "hgroup" "i" "img" "ins" "kbd" "li" "mark" "ol" "p" "pre"
                "q" "rp" "rt" "ruby" "s" "samp" "small" "strike" "strong" "sub" "sup" "table"
                "tbody" "td" "tfoot" "th" "thead" "time" "tr" "u" "ul" "var" "wbr")
    
    :attributes ((:all         . ("dir" "lang" "title"))
                 ("a"          . ("href"))
                 ("blockquote" . ("cite"))
                 ("col"        . ("span" "width"))
                 ("colgroup"   . ("span" "width"))
                 ("del"        . ("cite" "datetime"))
                 ("img"        . ("align" "alt" "height" "src" "width"))
                 ("ins"        . ("cite" "datetime"))
                 ("ol"         . ("start" "reversed" "type"))
                 ("q"          . ("cite"))
                 ("table"      . ("summary" "width"))
                 ("td"         . ("abbr" "axis" "colspan" "rowspan" "width"))
                 ("th"         . ("abbr" "axis" "colspan" "rowspan" "scope" "width"))
                 ("time"       . ("datetime" "pubdate"))
                 ("ul"         . ("type")))

    :protocols (("a"           . (("href" . ("ftp" "http" "https" "mailto" :relative))))
                ("blockquote"  . (("cite" . ("http" "https" :relative))))
                ("del"         . (("cite" . ("http" "https" :relative))))
                ("img"         . (("src"  . ("http" "https" :relative))))
                ("ins"         . (("cite" . ("http" "https" :relative))))
                ("q"           . (("cite" . ("http" "https" :relative))))))

(define-sanitize-mode +restricted+
    :inherit +default+
    :elements #("b" "em" "i" "strong" "u"))
 