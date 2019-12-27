;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; set tasks states
(setq org-todo-keywords '((sequence "TODO" "BLOCKED" "INPROGRESS" "|" "DONE" "ARCHIVED")))


;; Setting Colours (faces) for todo states to give clearer view of work 
;; (setq org-todo-keyword-faces
;;       '(("TODO" . org-warning)
;;         ("BLOCKED" . "magenta")
;;         ("DONE" . "green")
;;         ("ARCHIVED" . "lightblue")))

;; set default file for TODO stuff 
(setq org-default-notes-file "~/Desktop/notes.org")

(defun wrap-example (b e)
  "wraps active region into #+BEGIN_EXAMPLE .. #+END_EXAMPLE construct"
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (insert "#+BEGIN_EXAMPLE\n\t") 
    (goto-char (point-max)) 
    (insert "\n\t#+END_EXAMPLE\n")))

(defun wrap-src (b e)
  "wraps active region into #+BEGIN_SRC .. #+END_SRC construct"
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (insert "\n#+BEGIN_SRC\n") 
    (goto-char (point-max)) 
    (insert "\n#+END_SRC\n")))


(global-set-key (kbd "C-x M-e") 'wrap-example)
(global-set-key (kbd "C-x M-s") 'wrap-src)



;; Don't enlarge and fontify headers
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)


;; enable export to markdown
(eval-after-load "org"
  '(require 'ox-md nil t))
'(region ((t (:background "steel blue"))))


(provide 'setup-org)
