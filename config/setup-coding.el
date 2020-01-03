;; python -------------------------------
;; --------------------------------------
(elpy-enable)
;; to fix a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c l") 'iedit-mode)

;; please use my custom python here
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(setq exec-path (append exec-path '("/opt/anaconda3/bin")))
(setq python-shell-interpreter "/opt/anaconda3/bin/ipython"
        python-shell-interpreter-args "-i")

(setq python-indent-offset 4)


(provide 'setup-coding)
