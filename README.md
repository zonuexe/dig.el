# 🪖 dig.el

`dig.el` is an Emacs Lisp package that provides helper macros for retrieving values from structured lists and hash tables. This package allows users to conveniently "dig" into nested data structures such as:

 * Association lists (alist)
 * Property lists (plist)
 * Hash tables
 * Indexed lists (nth)

Instead of manually traversing these structures, `dig.el` expands lookup expressions at macro expansion time, ensuring efficient and readable deep access.

## Why “dig”?

The name "dig" reflects the idea of digging into structured data to retrieve nested values.
This package is inspired by Ruby's [`Hash#dig`](https://docs.ruby-lang.org/en/master/Hash.html#method-i-dig) method.

## Comparison with let-alist

Emacs provides the built-in `let-alist` macro for working with association lists.  However, `let-alist` is designed for binding nested values to variables in a more readable manner, while `dig.el` focuses on retrieving values with a concise expression.

### Example using let-alist

```el
(let-alist my-alist
  .foo.bar) ;; => 42
```

### Equivalent using `dig-alist`

```elisp
(dig-alist my-alist 'foo 'bar) ;; => 42
```

Unlike `let-alist`, dig supports property lists, hash tables, and indexed lists, making it more versatile for different data structures.

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
