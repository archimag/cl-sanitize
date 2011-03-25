.. -*- rst -*-

CL-SANITIZE
===========

cl-sanitize is a whitelist-based HTML sanitizer. Given a list of acceptable
elements and attributes, cl-sanitize will remove all unacceptable HTML from
a string.

cl-sanitize is a Common Lisp port of `Sanitize`_.

Requires
========

* `cl-libxml2`_

.. _Sanitize: https://github.com/rgrove/sanitize
.. _cl-libxml2: http://code.google.com/p/cl-libxml2/
