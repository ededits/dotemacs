;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)


;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\M-?" 'help-command)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)


;; Killing text
;;Kill the entire current line and reposition point at indentation
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; join lines
(global-set-key (kbd "C-c C-j") (lambda () (interactive) (join-line -1)))

 ;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") '(lambda () (interactive) (save-region-or-current-line 1)))


;; Make shell more convenient, and suspend-frame less
;; suspicious
;; (global-set-key (kbd "C-z") 'shell)
;; (global-set-key (kbd "C-x M-z") 'suspend-frame)


;; vim's ci and co commands
;;(global-set-key (kbd "M-I") 'change-inner)
;;(global-set-key (kbd "M-O") 'change-outer)



;; Create/delete new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)


;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'rotate-windows)
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)


;; Help should search more than just commands
;; (global-set-key (kbd "<f1> a") 'apropos)
                                               
;; Navigation bindings                         
(global-set-key [remap goto-line] 'goto-line-with-feedback)
                                               
;; Completion at point                         
(global-set-key (kbd "C-<tab>") 'completion-at-point)
                                               
;; Like isearch, but adds region (if any) to history and deactivates mark
(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)
                                               
;; Like isearch-*-use-region, but doesn't fuck with the active region
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)
                                               
;; Move more quickly                           
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))
                                               
;; Query replace regex key binding             
(global-set-key (kbd "M-&") 'query-replace-regexp)
                                               
                                               
;; ;; Comment/uncomment block                  
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
                                               
;; Create scratch buffer                       
(global-set-key (kbd "C-c b") 'create-scratch-buffer)
                                               
;; Move windows, even in org-mode              
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)   
(global-set-key (kbd "<s-down>") 'windmove-down)
                                               
                                               
;; Clever newlines                             
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
;;(global-set-key (kbd "<M-return>") 'new-line-in-between)
                                               
                                               
;; Duplicate region                            
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Sortingm
(global-set-key (kbd "M-s l") 'sort-lines)

;; Increase number at point (or other change based on prefix arg)
(global-set-key (kbd "C-+") 'change-number-at-point)


;; Buffer file functions
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)


;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)


;; increase font
(global-set-key (kbd "C-=") (quote text-scale-increase))
;; decrease font
(global-set-key (kbd "C--") (quote text-scale-decrease))

;; move to next window
(global-set-key "\C-x\C-n" 'other-window)
;; move to previous window
(global-set-key "\C-x\C-p" 'other-window-backward)


;; "Ctrl+c <-" will restore the previous window configuration and 
;; "Ctrl+c ->" will redo the configuration you just destroyed.
(winner-mode 1)


;; Add color to a shell running in emacs M-x shell
(global-set-key (kbd "C-c s") 'eshell)


(provide 'setup-keybindings)

