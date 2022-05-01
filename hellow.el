;;; hellow.el --- a Hello, World! example            -*- lexical-binding: t; -*-

;; Copyright (C) 2022 J Leadbetter

;; Author: J Leadbetter <j@jleadbetter.com>
;; Keywords: tutorial
;; Version: 0.3.0

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

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  Functions and variables in this package should all start with "hellow-"   ;;
;;  in order to mark them as part of this package.                            ;;
;;                                                                            ;;
;;  Notice that the following two variables start with `hellow`, per the      ;;
;;  name of the package, and then are followed by two `--`. This indicates    ;;
;;  that the variables are intended to be only used internally to the         ;;
;;  package and aren't for primary public use.                                ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
(defvar hellow--greeting-default "World"
  "Fall back to the traditional greeting.")

(defvar hellow--greeting-base "Hello, %s!"
  "Format string to say hello.")

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  The following are elisp macros.                                           ;;
;;                                                                            ;;
;;  We're going to be inputing the greeting string in one of three ways:      ;;
;;                                                                            ;;
;;  1. User-defined via a prompt.                                             ;;
;;  2. User-defined via a config variable.                                    ;;
;;  3. The default greeting string ("World").                                 ;;
;;                                                                            ;;
;;  Additionally, we're going to be outputing the collected string in three   ;;
;;  ways:                                                                     ;;
;;                                                                            ;;
;;  1. As a message on the status bar.                                        ;;
;;  2. As new buffer window.                                                  ;;
;;  3. As a popup window.                                                     ;;
;;                                                                            ;;
;;  Rather than write nine different functions to encompass all possible      ;;
;;  actions, we're going to write a macro that takes an input and output      ;;
;;  function that ties the pieces together.                                   ;;
;;                                                                            ;;
;;  Also, because learning about macros is fun. The macro is explained        ;;
;;  line-by-line.                                                             ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
(defmacro hellow--construct-hello (func-name interactive-prompt input-func output-func)
  "Construct a function that outputs \"Hello, \" plus some greeting.

The INTERACTIVE-PROMPT is a string to prompt the user for input (can be nil if
no prompt is desired)
The FUNC-NAME is what to call the function ('hellow' will be added in the macro)
The INPUT-FUNC returns a string (e.g. World)
The OUTPUT-FUNC displays it in some way (e.g., via popup or message).

Example:
    (hellow--construct-hello \"foo\" \"sPrompt: \" hello--input message)

This creates a function called `hello-foo` that prompts the user for a string
and then outputs the collected string via a status bar message."
;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  Here we define the function name. So if we want to have a function        ;;
;;  called "hellow-foo", we can pass the `func-name` of "foo" in the          ;;
;;  arguments to the macro.                                                   ;;
;;                                                                            ;;
;;  The `intern` function creates a symbol that can be used as a function     ;;
;;  name ('hellow-foo).                                                       ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
  (let ((hello-funcname (intern (concat "hellow-" func-name))))
;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  In creating a macro, you're basically defining a template for functions.  ;;
;;  The beginning of the template is marked by a back-tick (`).               ;;
;;                                                                            ;;
;;  When we want to insert a variable into the template, or evaluate some     ;;
;;  code that will be rendered into the template, we preface it with a comma  ;;
;;  (,). So we're inserting the literal `hellow-foo` function name we         ;;
;;  created in the line above.                                                ;;
;;                                                                            ;;
;;  Some of our input functions will require input from the user, so we're    ;;
;;  allowing for an `input-greeting-string`. This is optional, because not    ;;
;;  all of our input functions need input.                                    ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
    `(defun ,hello-funcname (&optional input-greeting-string)
;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  Here we define what kind of interactive prompt we want to use. Typically  ;;
;;  for this we'll use an "s" indicating we want a string, followed by some   ;;
;;  text that indicates what we want the prompt to say.                       ;;
;;                                                                            ;;
;;  In the case of the example, "sPrompt: ", this will display the word       ;;
;;  "Prompt: " to the user, and then collect a string. This string will be    ;;
;;  assigned to the `input-greeting-string` variable.                         ;;
;;                                                                            ;;
;;  If you put nil here, it won't prompt.                                     ;;
;;                                                                            ;;
;;  You can read more about interactive by hitting `C-h f` and then typing    ;;
;;  'interactive' (without quotes), to get the help documentation.            ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
       (interactive ,interactive-prompt)
;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  The `let*` is an interesting form of `let`, which binds the variables     ;;
;;  sequentially. So here, `greeting-string` is then used to generate the     ;;
;;  subsequent `greeting` variable.                                           ;;
;;                                                                            ;;
;;  Then we use the `input-func` (inserted into the template again using a    ;;
;;  comma) to get the correct greeting string based on the user input.        ;;
;;                                                                            ;;
;;  Finally, we format the `greeting-string` into the display form, e.g.,     ;;
;;  "Hello, World!".                                                          ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
       (let* ((greeting-string (,input-func input-greeting-string))
              (greeting (format hellow--greeting-base greeting-string)))
;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  Use the `output-func` to display the greeting                             ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
         (,output-func greeting)))))
;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  Based on the example:                                                     ;;
;;                                                                            ;;
;;      (hellow--construct-hello "foo"                                        ;;
;;                               "sPrompt: "                                  ;;
;;                               hellow--input                                ;;
;;                               message)                                     ;;
;;                                                                            ;;
;;  We would get the following macro expansion:                               ;;
;;                                                                            ;;
;;      (defun hellow-foo (&optional input-greeting-string)                   ;;
;;        (interactive "sPrompt: ")                                           ;;
;;        (let* ((greeting-string (hellow--input input-greeting-string))      ;;
;;               (greeting (format hellow--greeting-base greeting-string)))   ;;
;;          (message greeting)))                                              ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;

(defun hellow--input (&optional input-greeting-string)
  "Return a string to pass to output function. Provides one of:
1. INPUT-GREETING-STRING collected from a prompt
2. User-defined hellow-greeting-string
3. Default \"World\" string"
  (or input-greeting-string
      hellow-greeting-string
      hellow--greeting-default))

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  We don't need the input-greeting-string here, because we're just          ;;
;;  returning the default, so we preface it with an underscore.               ;;
;;                                                                            ;;
;;  This syntax is mostly to get flycheck or other code checkers to not       ;;
;;  complain that this variable *might* be passed to the input function,      ;;
;;  and then not get used.                                                    ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
(defun hellow--input-default-only (&optional _input-greeting-string)
  "Return a string to pass to output function.
Only provides the greeting default (\"World\")."
  hellow--greeting-default)

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  First we're going to create a function to output a basic "Hello, World!"  ;;
;;  message to the status bar.                                                ;;
;;                                                                            ;;
;;  We're going to add an `;;;###autoload` tag to make the function available ;;
;;  in Emacs, but not load the entire file until it's actually called.        ;;
;;                                                                            ;;
;;  You can read more about autoload here:                                    ;;
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html   ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
;;;###autoload
(hellow--construct-hello "message-default"
                         nil
                         hellow--input-default-only
                         message)
;;;###autoload
(hellow--construct-hello "message"
                         nil
                         hellow--input
                         message)

;;;###autoload
(hellow--construct-hello "message-prompt"
                         "sHello, who?: "
                         hellow--input
                         message)

;; TODO: how can I add docstrings to those ^^?

;;----------------------------------------------------------------------------;;
;;                                                                            ;;
;;  Your file should always end with `(provide '<package-name>)` and a        ;;
;;  `;;; <package-name>.el ends here` comment.                                ;;
;;                                                                            ;;
;;----------------------------------------------------------------------------;;
(provide 'hellow)
;;; hellow.el ends here
