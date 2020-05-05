;; python -------------------------------
;; --------------------------------------
;; (elpy-enable)
;; ;; to fix a key binding bug in elpy
;; (define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; (define-key global-map (kbd "C-c l") 'iedit-mode)

;; please use my custom python here
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(setq exec-path (append exec-path '("/opt/anaconda3/bin")))
(setq python-shell-interpreter "/opt/anaconda3/bin/ipython"
        python-shell-interpreter-args "-i --nosep")

(setq python-indent-offset 4)


;;--------------------------------------------------------
;; programming: make
(global-set-key "\C-c\C-]" (quote compile))
;; compilation window size
(setq compilation-window-height 8)
;; to make compilation window go away
;; if there are no compilation errors
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "compilation errors, press C-x ` to visit")
          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))
;;--------------------------------------------------------




(provide 'setup-coding)
