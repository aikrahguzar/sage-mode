;;; sage-blocks.el --- Support for structuring Sage code in sheets

;; Copyright (C) 2013-2018 Johan Rosenkilde

;; Author: Johan Rosenkilde <jsrn@jsrn.dk>
;; URL: https://github.com/sagemath/sage-shell-mode
;; Keywords: Sage, math

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

;; This file adds functionality which supports structuring experimental Sage
;; code in "sheets", where the code is bundled in "blocks" akin to the boxes of
;; the Notebook. Functions are provided for convenient handling of such blocks.
;; The file injects a few keybindings into `sage-shell:sage-mode' as well as
;; `sage-shell-mode'.

;; A block is defined by a line beginning with `sage-blocks-delimiter'.

;;; Code:
(require 'sage)

(defcustom sage-blocks-delimiter "###"
  "The regular expression defining the start of a block.

Note that '^' to match at the beginning of the line should not be added to
`sage-blocks-delimiter'. Strange behaviour might arise if
`sage-blocks-delimiter' matches multiple lines at a time."
  :type 'string
  :group 'sage)

;;
;; Functionality for Sage source files
;;
;;;###autoload
(defun sage-blocks-backward (arg)
  "Move backwards to the ARGth previous beginning of a block."
  (interactive "p")
  (if (< arg 0)
      (sage-blocks-forward (- arg))
    (while (and (> arg 0)
                (search-backward-regexp (concat "^" sage-blocks-delimiter) nil 'move))
      (setq arg (- arg 1)))))

;;;###autoload
(defun sage-blocks-forward (arg)
  "Move forwards to the next ARGth beginning of a block."
  (interactive "p")
  (if (< arg 0)
      (sage-blocks-backward (- arg))
    ;; If point is on a delimiter, we should skip this, so search from beginning of
    ;; next line (this will match immediately, if next line is a delimiter)
    (let ((re  (concat "^" sage-blocks-delimiter)))
      (when (looking-at re)
        (forward-line))
      ;; search forward: if it worked, move to begin of delimiter, otherwise end of file
      (while (and (> arg 0)
                  (search-forward-regexp re nil 'move))
        (setq arg (- arg 1)))
      ;; We successfully found something so move to the beginning of the match
      (when (= arg 0)
        (goto-char (match-beginning 0))))))

;;;###autoload
(defun sage-blocks-send-current ()
  "Send the block that the point is currently in to the inferior shell.
Move to end of block sent."
  (interactive)
  ;; Border-case: if we're standing on a delimiter, sage-blocks-backward will go
  ;; to previous delimiter, but we should send from this delimiter and forwards.
  (save-excursion
    (sage-blocks-forward 1)
    (python-shell-send-region (point)
                              (progn (sage-blocks-backward 1) (point)))))


(defun sage-blocks-find-all ()
  "Find all the blocks in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((blocks nil)
          (pos nil))
      (while (setq pos (re-search-forward
                        (rx line-start (literal sage-blocks-delimiter))
                        nil t))
        (goto-char pos)
        (push (string-trim (thing-at-point 'line t)
                           (rx (0+ (or space (literal sage-blocks-delimiter)))))
              blocks))
      blocks)))

;;;###autoload
(defun sage-blocks-goto (block)
  "Goto the named BLOCK from the current buffer using completion."
  (interactive
   (list (completing-read "Send block: " (sage-blocks-find-all) nil t)))
  (when-let (pos (save-excursion
                   (goto-char (point-min))
                   (re-search-forward
                    (rx line-start (literal sage-blocks-delimiter)
                        (0+ space) (literal block)))))
    (goto-char pos)))

;;;###autoload
(defun sage-blocks-send (block)
  "Send the named BLOCK from the current buffer using completion."
  (interactive
   (list (completing-read "Send block: " (sage-blocks-find-all) nil t)))
  (save-excursion (when (sage-blocks-goto block)
                    (sage-blocks-send-current))))

(provide 'sage-shell-blocks)
;;; sage-blocks.el ends here
