;;; hellow.el --- a Hello, World! example            -*- lexical-binding: t; -*-

;; Copyright (C) 2022 J Leadbetter

;; Author: J Leadbetter <j@jleadbetter.com>
;; Keywords: tutorial
;; Version: 0.0.1

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

;;; Commentary:

;; This is an intro to creating an Emacs package, using "Hello, World!" as an
;; example. This package does the following:
;;
;; 1. Pops up "Hello, World!" in the status line, a buffer, and a partial buffer.
;;
;; 2. Allows the user to change "World" to any other variable, either via a
;;    config variable or via a command prompt.

;;; Code:


(provide 'hellow)
;;; hellow.el ends here
