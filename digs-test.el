;;; digs-test.el --- Test for digs.el                -*- lexical-binding: t; -*-

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
(require 'digs)

(ert-deftest digs-test ()
  (should (equal "value"
                 (let ((alist '((foo . ((bar . "value"))))))
                   (digs-alist alist 'foo 'bar))))
  (should (equal "value"
                 (let ((alist '(("foo" . (("bar". "value"))))))
                   (digs-alist alist "foo" "bar"))))
  (should (equal "value"
                 (let ((plist '("foo" ("bar" "value"))))
                   (digs-plist plist "foo" "bar"))))
  (should (equal "value"
                 (let ((lst '(nil (nil nil (nil nil nil "value")))))
                   (digs-nth lst 1 2 3))))
  (should (equal "value"
                 (let* ((inner (let ((ht (make-hash-table :test #'equal)))
                                 (puthash "bar" "value" ht)
                                 ht))
                        (hashtable (make-hash-table :test #'equal)))
                   (puthash "foo" inner hashtable)
                   (digs hashtable "foo" "bar")))))

(ert-deftest digs-test-alist ()
  (should (equal '((foo . ((bar . 42))))
                 (let ((alist '((foo . ((bar . "value"))))))
                   (setf (digs-alist alist 'foo 'bar) 42)
                   alist)))
  (should (equal '(("foo" . (("bar". 42))))
                 (let ((alist '(("foo" . (("bar". "value"))))))
                   (setf (digs-alist alist "foo" "bar") 42)
                   alist)))
  (should (equal '("foo" ("bar" 42))
                 (let ((plist '("foo" ("bar" "value"))))
                   (setf (digs-plist plist "foo" "bar") 42)
                   plist)))
  (should (equal '(nil (nil nil (nil nil nil 42)))
                 (let ((lst '(nil (nil nil (nil nil nil "value")))))
                   (setf (digs-nth lst 1 2 3) 42)
                   lst)))
  (should (eq 42
              (let* ((inner (let ((ht (make-hash-table :test #'equal)))
                              (puthash "bar" "value" ht)
                              ht))
                     (hashtable (make-hash-table :test #'equal)))
                (puthash "foo" inner hashtable)
                (setf (digs hashtable "foo" "bar") 42)
                (digs hashtable "foo" "bar")))))

(provide 'digs-test)
;;; digs-test.el ends here
