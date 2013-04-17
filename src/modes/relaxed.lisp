;;;; relaxed.lisp
;;;;
;;;; This file is part of the cl-sanitize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:sanitize)

(define-sanitize-mode +relaxed+
    :elements ("a" "abbr" "b" "bdo" "blockquote" "br" "caption" "cite" "code" "col"
               "colgroup" "dd" "del" "dfn" "dl" "dt" "em" "figcaption" "figure" "h1" "h2"
               "h3" "h4" "h5" "h6" "hgroup" "i" "img" "ins" "kbd" "li" "mark" "ol" "p" "pre"
               "q" "rp" "rt" "ruby" "s" "samp" "small" "strike" "strong" "sub" "sup" "table"
               "tbody" "td" "tfoot" "th" "thead" "time" "tr" "u" "ul" "var" "wbr" "font")
    
    :attributes ((:all         . ("dir" "lang" "title" "class"))
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
                 ("ul"         . ("type"))
                 ("font"       . ("size")))

    :protocols (("a"           . (("href" . (:ftp :http :https :mailto :relative))))
                ("blockquote"  . (("cite" . (:http :https :relative))))
                ("del"         . (("cite" . (:http :https :relative))))
                ("img"         . (("src"  . (:http :https :relative))))
                ("ins"         . (("cite" . (:http :https :relative))))
                ("q"           . (("cite" . (:http :https :relative))))))
