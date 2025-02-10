;;; dig-test.el --- Test for dig.el                  -*- lexical-binding: t; -*-

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

;; Test code for dig.

;;; Code:
(require 'ert)
(require 'dig)

(ert-deftest dig-test ()
  (should (equal "value"
                 (let ((alist '((foo . ((bar . "value"))))))
                   (dig-alist alist 'foo 'bar))))
  (should (equal "value"
                 (let ((alist '(("foo" . (("bar". "value"))))))
                   (dig-alist alist "foo" "bar"))))
  (should (equal "value"
                 (let ((plist '("foo" ("bar" "value"))))
                   (dig-plist plist "foo" "bar"))))
  (should (equal "value"
                 (let ((lst '(nil (nil nil (nil nil nil"value")))))
                   (dig-nth lst 1 2 3))))
  (should (equal "value"
                 (let* ((inner (let ((ht (make-hash-table :test #'equal)))
                                 (puthash "bar" "value" ht)
                                 ht))
                        (hashtable (make-hash-table :test #'equal)))
                   (puthash "foo" inner hashtable)
                   (dig hashtable "foo" "bar")))))

(provide 'dig-test)
;;; dig-test.el ends here
