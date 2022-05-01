;;; hellow.el --- a Hello, World! example            -*- lexical-binding: t; -*-

;; Copyright (C) 2022 J Leadbetter

;; Author: J Leadbetter <j@jleadbetter.com>
;; Keywords: tutorial
;; Version: 0.1.1

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

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  This is an intro to creating an Emacs package, using "Hello, World!" as   ;;
;;  an example. This package does the following:                              ;;
;;                                                                            ;;
;;  1. Pops up "Hello, World!" in the status line, a buffer, and a partial    ;;
;;     buffer.                                                                ;;
;;                                                                            ;;
;;  2. Allows the user to change "World" to any other variable, either via a  ;;
;;     config variable or via a command prompt.                               ;;
;;                                                                            ;;
;;  3. Define a `hellow-mode` with keybindings for launching the commands.    ;;
;;                                                                            ;;
;;  This is written with a lot of obvious notes about what is going on, to    ;;
;;  help a person new to programming in Elisp and making Emacs packages.      ;;
;;                                                                            ;;
;;  Installation of this package is done by adding the following to your      ;;
;;  `init.el`:                                                                ;;
;;                                                                            ;;
;;      (use-package hellow                                                   ;;
;;          :ensure nil                                                       ;;
;;          :load-path "~/path-to-emacs-hellow-folder)                        ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  A word about comments:                                                    ;;
;;                                                                            ;;
;;  - A normal comment starts with two ;;                                     ;;
;;  - Comments starting with ;;; represent major headers                      ;;
;;                                                                            ;;
;;  There are two major headers in this document, Commentary and Code.        ;;
;;                                                                            ;;
;;  Subheaders start with multiple ;;;; indicating the depth of the           ;;
;;  subheader.                                                                ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;

;;; Code:

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  According to the emacs documenatation:                                    ;;
;;                                                                            ;;
;;  > Each Emacs Lisp package should have one main customization group which  ;;
;;  > contains all the options, faces and other groups in the package.        ;;
;;                                                                            ;;
;;  Basically, we want a way to let users customize our packages, and we      ;;
;;  should define a group for those customizations. In this case, we want to  ;;
;;  be able to define a config option to replace "World" in the output, so    ;;
;;  we'll first define a group to keep this option in.                        ;;
;;                                                                            ;;
;;  You can read more about groups here:                                      ;;
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Group-Definitions.html
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
(defgroup hellow nil
  "Hello, World! examples for learning how to build Emacs packages."
  ;; TODO: confirm this: I think you can get a list of groups to use here
  ;; by doing `C-h v` and searching for 'finder-known-keywords'.
  :group 'help
  :link '(url-link :tag "repository" "https://github.com/kamni/emacs-hellow"))

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  In your emacs config file, you can define different config options for    ;;
;;  a package that can be overridden. Here we define a configuration option   ;;
;;  for what to print after the "Hello, " part of the string. The default     ;;
;;  will be "World" (of course!).                                             ;;
;;                                                                            ;;
;;  Update your `use-package` declaration to override this variable:          ;;
;;                                                                            ;;
;;      (use-package hellow                                                   ;;
;;          :ensure nil                                                       ;;
;;          :load-path "~/path-to-emacs-hellow-folder                         ;;
;;          :init (setq hellow-greeting-string "Jay"))                        ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
(defcustom hellow-greeting-string nil
  "What to print after the \"Hello\" when displaying the greeting."
  :group 'hellow
  :type 'string)

(defvar hellow--greeting-default "World"
  "Fall back to the traditional greeting.")

(defvar hellow--greeting-base "Hello, %s!"
  "Format string to say hello.")

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  First we're going to create a function to output a basic "Hello, X"       ;;
;;  message to the status bar.                                                ;;
;;                                                                            ;;
;;  We're going to add an `;;;###autoload` tag to make the function available ;;
;;  in Emacs, but not load the entire file until it's actually called.        ;;
;;                                                                            ;;
;;  You can read more about autoload here:                                    ;;
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html   ;;
;;                                                                            ;;
;;  Functions in this package should all start with `hellow-` in order to     ;;
;;  mark them as part of this as part of the package.                         ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
;;;###autoload
(defun hellow-message ()
  "Prints a \"Hello\" message to the status bar."
;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  The `(interactive)` call here turns this into a command that can be       ;;
;;  called using `M-x hellow-message`.                                        ;;
;;                                                                            ;;
;;  You can read more about interactive by hitting `C-h f` and then typing    ;;
;;  'interactive' (without quotes), to get the help documentation.            ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
  (interactive)
  (let ((greeting-string (or hellow-greeting-string
                             hellow--greeting-default)))
    (message (format hellow--greeting-base greeting-string))))

;;;###autoload
(defun hellow-message-default ()
  "Outputs the traditional greeting, even if it has been overridden."
  (interactive)
  (message (format hellow--greeting-base hellow--greeting-default)))

(provide 'hellow)
;;; hellow.el ends here
