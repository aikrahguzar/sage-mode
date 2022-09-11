;;; sage.el --- A front-end for Sage Math -*- lexical-binding: t -*-

;; URL: https://github.com/aikrahguzar/sage-mode
;; Package-Requires: ((emacs "25.1") (compat "28.1"))
;; Keywords: languages, processes, tools
;; Version: 0.1

;;; License
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a front end for Sage (http://www.sagemath.org/)
;; and a major mode derived from python-mode (sage-mode).

;; To use this package, check the return value of (executable-find "sage").
;; If (executable-find "sage") is a string, you are ready to use this package.

;; Then you can run Sage process in Emacs by M-x sage-run.

;; This is a work in progress. There goal here to replace `sage-shell'
;; with something that is much smaller, as close as possible to vanilla
;; `comint-mode' and `inferior-python-mode' for the repl and directly use
;; the facilities provided by IPython bundled by sage for most of the tasks.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'python)
(require 'compile)
(require 'compat)

;;; Global variables for users
(defgroup sage
  nil "Emacs interface to sagemath."
  :group 'languages)

(defgroup sage-shell-sagetex
  nil "Group for SageTeX."
  :group 'sage)

(defcustom sage-executable nil
  "Name of the Sage executable.
If the Sage executable in your
PATH and (exeutable-find \"sage\") is non-nil, then you do not
have to set this variable."
  :group 'sage
  :type '(choice (string :tag "Executable for sage")
                 (const :tag "Not specified" nil)))

(defcustom sage-history-file nil
  "If non nil, then `comint-input-ring' is saved to this file.
This happens when the Sage process exits."
  :group 'sage
  :type '(choice (file :tag "file")
                 (const :tag "Off" nil)))

;;; sage-shell
(defun sage-executable ()
  "Try to find the sage executable and set `sage-executable' if it is unset."
  (or sage-executable
      (when-let ((exe (executable-find "sage")))
        (file-truename exe))))

(defvar sage-setup-code
  "\
def __PYTHON_EL_eval(source, filename):
    import ast, sys
    from sage.repl.preparse import preparse_file
    if isinstance(source, str):
        source = preparse_file(source)
    else:
        source = preparse_file(source.decode())
    if sys.version_info[0] == 2:
        from __builtin__ import compile, eval, globals
    else:
        from builtins import compile, eval, globals
    try:
        p, e = ast.parse(source, filename), None
    except SyntaxError:
        t, v, tb = sys.exc_info()
        sys.excepthook(t, v, tb.tb_next)
        return
    if p.body and isinstance(p.body[-1], ast.Expr):
        e = p.body.pop()
    try:
        g = globals()
        exec(compile(p, filename, 'exec'), g, g)
        if e:
            return eval(compile(ast.Expression(e.value), filename, 'eval'), g, g)
    except Exception:
        t, v, tb = sys.exc_info()
        sys.excepthook(t, v, tb.tb_next)

get_ipython().cache_size = int(0)
get_ipython().history_manager.enabled = False

import jedi
jedi.settings.case_insensitive_completion = True

from IPython.core.completer import IPCompleter
__IP_completer = IPCompleter(shell=get_ipython() , namespace=locals())
__IP_completer.jedi_compute_type_timeout = int(0)

def __SAGE_complete(str):
    return [x + '        ' + __PYDOC_get_help(x) for x in __IP_completer.all_completions(str)]
"
  "Code used to evaluate statements in sage repl.
Default value is adapted from `python-shell-eval-setup-code' with the
difference that this in addition preparses the sage input.")

(defvar sage-eldoc-setup-doc
  "\
def __PYDOC_get_help(obj):
    import inspect
    doc = None
    ann = ''
    try:
        if isinstance(obj, str):
            obj = eval(obj, globals())
            doc = inspect.getdoc(obj)
        if doc:
            ann = doc.splitlines()[0]
        if callable(obj):
            target = None
            if inspect.isclass(obj) and hasattr(obj, '__init__'):
                target = obj.__init__
                objtype = 'class'
            else:
                target = obj
                objtype = 'def'
            if target:
                args = str(inspect.signature(obj))
                name = obj.__name__
                ann = f'{objtype} {name}{args}' + ': ' + ann
    except:
        ann = ' '
    return ann
"
"Code used to override `python-eldoc-setup-code'.
This is to supress a warning that makes its way to eldoc string.
It also combines both the argspec and the first line of documentation.")

(defvar-local sage--process-busy-p nil)

(defvar-local sage-next-redirect-command nil
  "A cons (FUN . ARGS). FUN will be called with ARGS when redirect finishes.")

(defvar-local sage--saved-input nil
  "Saved input to restore after the process.")

(defun sage--input-filter (_)
  "Hook added to `comint-input-filter-functions'."
  (setq sage--process-busy-p t))

(defun sage--output-filter (output)
  "Restore the input that was previously saved when OUTPUT has prompt."
  (when (string-match (rx (regexp comint-prompt-regexp) eos) output)
    (setq sage--process-busy-p nil)
    (when sage--saved-input
      (comint-goto-process-mark)
      (insert sage--saved-input)
      (setq sage--saved-input nil))))

(defun sage--redirect-done ()
  "Addition to `comint-redirect-hook'."
  (with-current-buffer comint-redirect-output-buffer
    (goto-char (point-min)))
  (when sage-next-redirect-command
    (let ((com sage-next-redirect-command))
      (setq sage-next-redirect-command nil)
      (apply (car com) (cdr com)))))

(defun sage--save-input ()
  "Save the current input."
  (setq sage--saved-input (progn (comint-goto-process-mark)
                                 (delete-and-extract-region (point) (point-max)))))

(defun sage-first-prompt-setup ()
  "Setup the interactive session using `python-shell-first-prompt-hook'."
  (python-shell-send-string-no-output sage-eldoc-setup-doc)
  (python-shell-send-string-no-output sage-setup-code))

(define-derived-mode sage-shell-mode inferior-python-mode
  "Sage Repl" "Execute Sage commands interactively."
  :group 'sage

  (setq-local comint-prompt-regexp (rx (regexp comint-prompt-regexp) eos)
              comint-redirect-perform-sanity-check nil
              comint-redirect-completed t
              comint-input-ring-file-name sage-history-file
              comint-input-ignoredups t)

  (setq-local python-shell-completion-native-enable nil)

  (comint-read-input-ring t)

  (setq-local completion-at-point-functions '(sage-completions-at-point t))

  (add-hook 'kill-buffer-hook #'comint-write-input-ring)

  (add-hook 'comint-input-filter-functions #'sage--input-filter nil t)
  (add-hook 'comint-output-filter-functions #'sage--output-filter t t)
  (add-hook 'comint-redirect-filter-functions #'ansi-color-apply nil t)
  (add-hook 'comint-redirect-hook #'sage--redirect-done nil t)
  (add-hook 'python-shell-first-prompt-hook #'sage-first-prompt-setup))

(defvar python-shell--interpreter)
(defvar python-shell--interpreter-args)

(defun sage--run (buffer-name display &rest switches)
  "Run Sage.
If BUFFER-NAME is a string, it will be the name of the process buffer. If it
is any other non-nil value a new buffer based on `*Sage*' is used. If it is
nil the buffer `*Sage*' is used and resued if it already present.

If buffer thus found has no associated process a sage process is satrted.
SWITCHES are the arguments passed to it as in `make-comint-in-buffer'.
If DISPLAY is non-nil the buffer is displayed."
  (let* ((proc (python-shell-get-process-name buffer-name))
         (buf (get-buffer-create (if (stringp buffer-name)
                                     buffer-name
                                   (format "*%s*" proc)))))
    (if display (display-buffer buf))
    (unless (get-buffer-process buf)
      (apply #'make-comint-in-buffer proc buf (sage-executable) nil switches)
      (with-current-buffer buf
        (let ((inhibit-read-only t)) (erase-buffer))
        (let ((python-shell--interpreter "sage")
              (python-shell--interpreter-args "--simple-prompt")
              (python-shell-prompt-detect-enabled nil)
              (python-shell-interpreter-interactive-arg "")
              (python-shell-prompt-regexp "sage: "))
          (sage-shell-mode))))
    buf))

;;;###autoload
(defun sage-run (arg &optional display)
  "Run sage. If ARG is non-nil start a dedicated sage process.
If DISPLAY is non-nil display the resulting buffer."
  (interactive (list current-prefix-arg t))
  (sage--run arg display "--simple-prompt"))

(defun sage-restart ()
  "Restart the Sage process for current buffer."
  (interactive)
  (when-let ((buf (python-shell-get-buffer))
             (proc (get-buffer-process buf)))
    (with-current-buffer buf
      (comint-send-eof) (set-process-buffer proc nil) (kill-process proc)))
  (sage-run (get-buffer (format "*%s*" (python-shell-get-process-name t)))))

(defvar-local sage--completion-table nil)

(defun sage-busy-p (buf)
  "Return non-nil if sage process in BUF is busy."
  (or (not (buffer-local-value 'comint-redirect-completed buf))
      (buffer-local-value 'sage--process-busy-p buf)))

(defun sage-completions-list (symbol proc)
  "Return a list of completions for SYMBOL from sage process PROC."
  (let ((raw (comint-redirect-results-list-from-process
              proc
              (concat "__SAGE_complete('" symbol "')")
              (rx (or (seq ?\" (group-n 1 (* (not ?\"))) ?\")
                      (seq ?' (group-n 1 (* (not ?'))) ?'))) 1))
        (completions))
    (dolist (r raw completions)
      (let ((pos (string-search " " r)))
        (push (propertize (substring r 0 pos)
                          :annotation (substring r (1+ pos)))
              completions)))))

(defun sage-completion-table ()
  "A completion table returning completions from sage process PROC.
It returns a lambda which accepts one argumet SYMBOL and returns
a list of all completions for SYMBOL.

The design is taken from `completion-table-with-cache' but accounts
for the fact that we get new completions after encountering a new dot."
  (let* (last-arg
         last-result
         (proc (python-shell-get-process))
         (new-fun
          (lambda (arg)
            (if (and last-arg (string-prefix-p last-arg arg t)
                     (not (string-match-p (rx (or ?.)) arg (max 0 (1- (length last-arg))))))
                last-result
            (message arg)
              (unless (process-live-p proc)
                (setq proc (python-shell-get-process)))
              (when-let (((not (sage-busy-p (process-buffer proc))))
                         (completions (while-no-input (sage-completions-list arg proc)))
                         ((not (booleanp completions))))
                (setq last-arg arg)
                (setq last-result completions))))))
    (completion-table-dynamic new-fun)))

(defun sage-completions-at-point ()
  "Completions at point from the sage process."
  (with-syntax-table python-dotty-syntax-table
    (if (not (save-excursion (goto-char (line-beginning-position))
                             (looking-at (rx (or "from " "import ")))))
        (when-let ((symbol (thing-at-point 'symbol))
                   ((not (string-match (rx bos ?.) symbol)))
                   (bounds (bounds-of-thing-at-point 'symbol)))
          (list (car bounds) (cdr bounds)
                (or sage--completion-table
                    (setq sage--completion-table (sage-completion-table)))
                :annotation-function
                (lambda (cand) (propertize (get-text-property 0 :annotation cand)
                                      'face font-lock-comment-face))))
      (when-let ((completions (while-no-input
                                (comint-redirect-results-list-from-process
                                 (python-shell-get-process)
                                 (format "get_ipython().complete('' , '%s' , %s)"
                                         (buffer-substring (line-beginning-position)
                                                           (line-end-position))
                                         (- (point) (line-beginning-position)))
                                 (rx (seq ?' (group-n 1 (* (not ?'))) ?')) 1)))
                 ((not (booleanp completions))))
        (list (- (point) (length (car completions))) (point) (cdr completions))))))

;;;###autoload
(define-derived-mode sage-mode python-mode "Sage"
  :group 'sage
  (add-hook 'completion-at-point-functions
            'sage-completions-at-point nil t)
  (setq-local python-shell-buffer-name "Sage"
              python-shell-interpreter (sage-executable)
              python-shell-interpreter-args "--simple-prompt"
              python-eldoc-setup-code sage-eldoc-setup-doc))

;;;###autoload
(cl-pushnew `(,(rx ".sage" eos) . sage-mode) auto-mode-alist :test #'equal)

(defun sage-complete-redirection (proc)
  "Accept input from PROC until redirection completes."
  (with-current-buffer (process-buffer proc)
    (while (and (null comint-redirect-completed)
                (accept-process-output proc)))))

(defun sage-lookup-doc (symbol &optional proc display)
  "Look up documentation for the SYMBOL.
It is the symbol at point when called interactively. PROC is the sage process.
If DISPLAY is non-nil, buffer is displayed."
  (interactive (list (python-info-current-symbol) (python-shell-get-process) t))
  (let ((buf (get-buffer-create "*Sage Documentation*"))
        (proc (or proc (python-shell-get-process))))
    (prog1 buf
      (with-current-buffer buf
        (erase-buffer)
        (setq mode-line-format nil)
        (visual-line-mode)
        (font-lock-mode))
      (comint-redirect-send-command-to-process
       (format "get_ipython().run_line_magic('pinfo', '%s')" symbol)
       buf proc nil nil)
      (when display (pop-to-buffer buf)))))

(defun sage-send-line (proc)
  "Send current line to the sage process PROC."
  (interactive (list (python-shell-get-process)))
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (with-current-buffer (process-buffer proc)
      (sage--save-input)
      (insert line)
      (comint-send-input))))

(defun sage--send-region (start end &optional send-main msg
                                no-cookie)
  "Send the region delimited by START and END to inferior Python process.
SEND-MAIN, MSG and NO-COOKIE are as in `python-shell-send-region'."
  (when (eq major-mode #'sage-mode)
    (let* ((string (python-shell-buffer-substring start end (not send-main)
                                                  no-cookie))
           (process (python-shell-get-process-or-error msg))
           (original-string (buffer-substring-no-properties start end))
           (_ (string-match "\\`\n*\\(.*\\)" original-string)))
      ;; Recalculate positions to avoid landing on the wrong line if
      ;; lines have been removed/added.
      (with-current-buffer (process-buffer process)
        (sage--save-input)
        (insert (truncate-string-to-width
                 (format "# %s: %s..."
                         this-command
                         (match-string 1 original-string))
                 (- (window-width (get-buffer-window)) 15)
                 nil nil "..."))
        (let ((comint-input-sender 'ignore)
              (comint-input-filter-functions nil)
              (comint-input-filter #'ignore))
          (comint-send-input t t))
        (compilation-forget-errors))
      (python-shell-send-string string process)
      (deactivate-mark)
      t)))

(advice-add 'python-shell-send-region :before-until #'sage--send-region)

(provide 'sage)
;;; sage.el ends here
