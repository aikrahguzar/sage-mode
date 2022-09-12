;;; sage-consult.el --- Description -*- lexical-binding: t; -*-
;;
;;
;; Maintainer: azeem <azeem@Laptop>
;; Created: September 10, 2022
;; Modified: September 10, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/azeem/sage-consult
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'sage)
(require 'consult)

(defvar sage-consult--history nil)

;;;###autoload
(defun sage-consult () "A documentation browser for sage objects." (interactive)
       (if (sage-busy-p (get-buffer (python-shell-get-buffer)))
           (message "Sage process is busy. Retry once it is done.")
         (let* ((doc-buf (get-buffer-create "*Sage Documentation*" t))
                (proc (python-shell-get-process))
                (buf (process-buffer proc))
                (comps (sage-completions-list "" proc))
                (cached (make-hash-table :test #'equal)))
           (cl-flet
               ((doc-fun (cand)
                         (unless (gethash cand cached)
                           (with-current-buffer buf
                             (setq sage-next-redirect-command
                                   `(,(lambda () (when-let ((cl (while-no-input
                                                             (sage-completions-list
                                                              (concat cand ".") proc)))
                                                       ((not (booleanp cl))))
                                              (cl-callf append comps cl)
                                              (puthash cand t cached)))))))
                         (sage-lookup-doc cand proc)))
             (with-selected-window (display-buffer doc-buf)
               (unwind-protect
                   (consult--read
                    (lambda (_) comps)
                    :prompt "Sage Objects: "
                    :state (lambda (action cand)
                             (when cand
                               (setq cand (substring-no-properties cand))
                               (pcase action
                                 ('preview (prog1 nil
                                             (if (buffer-local-value
                                                  'comint-redirect-completed buf)
                                                 (doc-fun cand)
                                               (with-current-buffer buf
                                                 (setq sage-next-redirect-command
                                                       `(,(lambda () (doc-fun cand)))))))))))
                    :preview-key 'any
                    :history sage-consult--history
                    :require-match t)
                 (kill-buffer-and-window)))))))



(provide 'sage-consult)
;;; sage-consult.el ends here
