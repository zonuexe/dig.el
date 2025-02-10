;;; dig.el --- Helper macros for retrieving values from structured lists and hash tables  -*- lexical-binding: t; -*-

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

;; This package provides helper macros for retrieving values from
;; association lists (alist), property lists (plist), hash tables, and
;; indexed lists (nth).  It allows convenient and readable deep access
;; to nested data structures.

;;; Code:
(require 'cl-lib)

;; Internal functions
(defun dig--expand-alist (key subject)
  "Expand an alist lookup expression.

KEY is the key to look up.
SUBJECT is the alist."
  (let ((test (if (symbolp key) #'eq #'equal)))
    `(alist-get ,key ,subject nil nil (function ,test))))

(defun dig--expand-plist (key subject)
  "Expand a plist lookup expression.

KEY is the key to look up.
SUBJECT is the plist."
  (let* ((test (if (symbolp key) #'eq #'equal)))
    `(plist-get ,subject ,key (function ,test))))

(defun dig--expand-hash (key subject)
  "Expand a hash table lookup expression.

KEY is the key to look up.
SUBJECT is the hash table."
  `(when ,subject (gethash ,key ,subject)))

(defun dig--expand-nth (key subject)
  "Expand an indexed list lookup expression.

KEY is the index to retrieve.
SUBJECT is the list."
  `(nth ,key ,subject))

(defun dig--expand-dwim (key subject)
  "Expand a lookup expression based on the type of KEY.

KEY is the key to look up.
SUBJECT is the data structure.
Expands to the appropriate lookup form based on the type of KEY."
  (cond
   ((stringp key) (dig--expand-hash key subject))
   ((integerp key) (dig--expand-nth key subject))
   ((keywordp key) (dig--expand-plist key subject))
   ((dig--expand-alist key subject))))

;; Macros
(defmacro dig-alist (subject first-key &rest keys)
  "Retrieve a nested value from an alist (association list).

SUBJECT is the initial alist.
FIRST-KEY is the first key to retrieve.
KEYS is an optional sequence of additional keys to traverse the alist.
Returns the value associated with the final key."
  (let ((result subject))
    (cl-loop for key in (cons first-key keys)
             do (setq result (dig--expand-alist key result)))
    result))

(defmacro dig-plist (subject first-key &rest keys)
  "Retrieve a nested value from a plist (property list).

SUBJECT is the initial plist.
FIRST-KEY is the first key to retrieve.
KEYS is an optional sequence of additional keys to traverse the plist.
Returns the value associated with the final key."
  (let ((result subject))
    (cl-loop for key in (cons first-key keys)
             do (setq result (dig--expand-plist key result)))
    result))

(defmacro dig-hash (subject first-key &rest keys)
  "Retrieve a nested value from a hash table.

SUBJECT is the initial hash table.
FIRST-KEY is the first key to retrieve.
KEYS is an optional sequence of additional keys to traverse the hash table.
Returns the value associated with the final key."
  (let ((result subject))
    (cl-loop for key in (cons first-key keys)
             do (setq result (dig--expand-hash key result)))
    result))

(defmacro dig-nth (subject first-key &rest keys)
  "Retrieve a nested value from a list using indexed access.

SUBJECT is the initial list.
FIRST-KEY is the first index to retrieve.
KEYS is an optional sequence of additional indices to traverse the list.
Returns the value at the final index."
  (let ((result subject))
    (cl-loop for key in (cons first-key keys)
             do (setq result (dig--expand-nth key result)))
    result))

(defmacro dig (subject first-key &rest keys)
  "Expand a nested lookup expression based on the type of each key.

SUBJECT is the initial data structure (alist, plist, hash table, or list).
FIRST-KEY is the first key to retrieve.
KEYS is an optional sequence of additional keys or indices, each of which
determines how to expand its lookup expression at macro expansion time.
Returns the value associated with the final key or index."
  (let ((result subject))
    (cl-loop for key in (cons first-key keys)
             do (setq result (dig--expand-dwim key result)))
    result))

(provide 'dig)
;;; dig.el ends here
