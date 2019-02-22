;;;; package.lisp

(defpackage :dc-ann-examples
  (:use :cl :dc-utilities :sb-thread :cl-ppcre :dc-ann)
  (:export
   xor-ann
   xor-training
   xor-ann-1hs
   xor-training-1hs
   circle-training
   circle-training-1hs
   count-ann
   count-training
   vector-with-n-items
   vector-with-item-n-lit
   count-trained
   variables-training
   variables-ann
   variables-trained))
