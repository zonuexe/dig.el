;;; digs-shortdoc.el --- Shortdoc for digs.el        -*- lexical-binding: t; -*-

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
  (require 'digs)
  (require 'shortdoc))

(define-short-documentation-group dig
  "Digs into nested sutructures."
  (digs
   :eval (macroexpand
          '(digs subject :foo))
   :eval (macroexpand
          '(digs subject 'foo))
   :eval (macroexpand
          '(digs subject "foo"))
   :eval (macroexpand
          '(digs subject "foo" "bar"))
   :eval (macroexpand
          '(digs subject 1 2)))
  (digs-alist
   :eval (digs '((foo . 42)) 'foo)
   :eval (digs-alist '((foo . 42)) 'foo))
  (digs-plist
   :eval (digs '(:foo 42) :foo)
   :eval (digs-plist '(:foo 42) :foo))
  (digs-nth
   :eval (digs '(39 (40 41 42)) 1 2)
   :eval (digs-nth '(39 (40 41 42)) 1 2))
  (digs-hash
   :eval (macroexpand
          '(digs-hash subject "foo" "bar"))
   :eval (let* ((in (make-hash-table :test #'equal))
                (ht (make-hash-table :test #'equal)))
           (puthash "bar" 42 in)
           (puthash "foo" in ht)
           (digs-hash ht "foo" "bar"))))

(provide 'digs-shortdoc)
;;; digs-shortdoc.el ends here
