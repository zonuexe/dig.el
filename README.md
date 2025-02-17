# 🪖 digs.el

`digs.el` is an Emacs Lisp package that provides helper macros for retrieving values from structured lists and hash tables. The name “digs” stands for “Dig (data) structures,” reflecting the package's ability to help users conveniently *dig* into nested data structures such as:

 * [Association Lists][] (alist)
 * [Property Lists][] (plist)
 * [Hash tables][] (hash)
 * [Sequences][] (lists and arrays)

Instead of manually traversing these structures, `digs.el` expands lookup expressions at macro expansion time, ensuring efficient and readable deep access.

[Association Lists]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html
[Property Lists]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html
[Hash tables]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Hash-Tables.html
[Sequences]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html

## Motivations

Languages ​​like JavaScript have a symmetric syntax for reading and writing nested objects.

```js
const obj = {foo: {bar: {buz: 0}}};
obj.foo.bar.buz = 42;
obj.foo.bar.buz++;
console.log(obj.foo.bar.buz);
```

Reading nested structures is complicated in Lisp:

``` elisp
(setq plist '(:foo (:bar (:buz 0))))
(plist-get (plist-get (plist-get plist :foo) :bar) :buz)

;; Another way... is it easy to read?
(require 'subr-x)
(thread-first plist
              (plist-get :foo)
              (plist-get :bar)
              (plist-get :buz))

;; How to replace 0 with 42? That's overly complicated compared to JS.
```

If you replace it with digs, it will look like this:

``` elisp
(setq plist '(:foo (:bar (:buz 0))))
(setf (digs plist :foo :bar :buz) 42)      ;; obj.foo.bar.buz = 42
(cl-incf (digs plist :foo :bar :buz))      ;; obj.foo.bar.buz++
(message "%S" (digs plist :foo :bar :buz)) ;; console.log(obj.foo.bar.buz)
```

There are more characters than in JS, but it's symmetrical and much easier to read 🎉

## Usage

### Retrieving values from different data structures

```el
(setq my-alist '((foo . ((bar . 42)))))
(setq my-plist '(:foo (:bar 42)))
(setq my-hash (make-hash-table :test 'equal))
(puthash "foo" (make-hash-table :test 'equal) my-hash)
(puthash "bar" 42 (gethash "foo" my-hash))
(setq my-list '((40 41) (42 43)))
(setq my-vector '[[40 41] [42 43]])

(digs-alist my-alist 'foo 'bar) ;; => 42
(digs-plist my-plist :foo :bar) ;; => 42
(digs-hash my-hash "foo" "bar") ;; => 42
(digs-elt my-list 1 0)          ;; => 42
(digs-elt my-vector 1 0)        ;; => 42
```

### Using `digs` for automatic key expansion

The `digs` macro determines the appropriate lookup method based on the key type. The first key (first-key) is required, and additional keys (keys) allow deeper traversal.

```el
(digs my-alist 'foo 'bar)   ;; Expands to alist lookup
(digs my-plist :foo :bar)   ;; Expands to plist lookup
(digs my-hash "foo" "bar")  ;; Expands to hash lookup
(digs my-list 1 0)          ;; Expands to elt lookup
(digs my-vector 1 0)        ;; Expands to elt lookup
```

This approach makes retrieving deeply nested values more expressive and concise.

> [!NOTE]
> Please note that these retrieval methods are not dynamically determined by the target data structure, but rather the macros are statically expanded depending on the key to be retrieved.

## Why “dig”?

The name “dig” reflects the idea of digging into structured data to retrieve nested values.
This package is inspired by Ruby's [`Hash#dig`](https://docs.ruby-lang.org/en/master/Hash.html#method-i-dig) method.

This package was originally just called “dig”, but was renamed to “digs” to avoid a conflict with the Emacs built-in [`dig`](https://en.wikipedia.org/wiki/Dig_(command)) command (short for “Domain Information Groper”).

## Comparison with let-alist

Emacs provides the built-in `let-alist` macro for working with association lists.  However, `let-alist` is designed for binding nested values to variables in a more readable manner, while `digs.el` focuses on retrieving values with a concise expression.

### Example using let-alist

```el
(let-alist my-alist
  .foo.bar) ;; => 42
```

### Equivalent using `digs-alist`

```elisp
(digs-alist my-alist 'foo 'bar) ;; => 42
```

Unlike `let-alist`, digs supports property lists, hash tables, and indexed lists, making it more versatile for different data structures.

## `setf` (generalized variables) support

Emacs Lisp makes it easy to set values ​​in a variety of data structures through **generalized variables** known as `setf`.  Of course, all modify macros can be used, not just `setf`.

 * [Setting Generalized Variables (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Generalized-Variables.html)
 * [Modify Macros (Common Lisp Extensions)](https://www.gnu.org/software/emacs/manual/html_node/cl/Modify-Macros.html)

```elisp
(setq my-plist '(:foo (:bar 0)))
(setf (digs my-plist :foo :bar) 42)
(cl-incf (digs my-plist :foo :bar)) ;=> 43
(cl-incf (digs my-plist :foo :bar)) ;=> 44

my-plist ;=> (:foo (:bar 44))
```

> [!WARNING]
> [The behavior is undefined][undefined_behavior] when no value exists for the target key -- it is useful for updating the value of existing elements, but not for helping to construct new data structures.

[undefined_behavior]: https://en.wikipedia.org/wiki/Undefined_behavior

## Copyright

This package is released under GPL-3.0.  See [`LICENSE`](LICENSE) file.

> Copyright (C) 2024  USAMI Kenta
>
> This program is free software; you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
>
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> GNU General Public License for more details.
>
> You should have received a copy of the GNU General Public License
> along with this program.  If not, see <https://www.gnu.org/licenses/>.
