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
  "wraps active region into #+begin_example .. #+end_example construct"
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (insert "#+begin_example\n") 
    (goto-char (point-max)) 
    (insert "\n#+end_example\n")))

(defun wrap-src (b e)
  "wraps active region into #+begin_src .. #+end_src construct"
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (insert "\n#+begin_src\n") 
    (goto-char (point-max)) 
    (insert "\n#+end_src\n")))


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

(use-package org-tempo)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))




(provide 'setup-org)
