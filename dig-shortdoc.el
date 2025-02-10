;;; dig-shortdoc.el --- Shortdoc for dig.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2025  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shortdoc implementation for dig.

;;; Code:
(eval-when-compile
  (require 'dig)
  (require 'shortdoc))

(define-short-documentation-group dig
  "Dig into nested sutructures."
  (dig
   :eval (macroexpand
          '(dig subject :foo))
   :eval (macroexpand
          '(dig subject 'foo))
   :eval (macroexpand
          '(dig subject "foo"))
   :eval (macroexpand
          '(dig subject "foo" "bar"))
   :eval (macroexpand
          '(dig subject 1 2)))
  (dig-alist
   :eval (dig '((foo . 42)) 'foo)
   :eval (dig-alist '((foo . 42)) 'foo))
  (dig-plist
   :eval (dig '(:foo 42) :foo)
   :eval (dig-plist '(:foo 42) :foo))
  (dig-nth
   :eval (dig '(39 (40 41 42)) 1 2)
   :eval (dig-nth '(39 (40 41 42)) 1 2))
  (dig-hash
   :eval (macroexpand
          '(dig-hash subject "foo" "bar"))
   :eval (let* ((in (make-hash-table :test #'equal))
                (ht (make-hash-table :test #'equal)))
           (puthash "bar" 42 in)
           (puthash "foo" in ht)
           (dig-hash ht "foo" "bar"))))

(provide 'dig-shortdoc)
;;; dig-shortdoc.el ends here
