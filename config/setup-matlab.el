
;;------------------------------------------------------------
;; MATLAB MODE
;;------------------------------------------------------------
(add-to-list 'load-path "/Users/eugene/.emacs.d/config/matlab-emacs")
(load-library "matlab-load")

;; use matlab-mode when you load .m files
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(setq matlab-shell-command '"/Applications/MATLAB_R2016a.app/bin/matlab")
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))
;; enble matla history to be available in the matlab-shell
(setq comint-input-ring-file-name "/Users/eugene/.matlab/R2016a/history.m") 
;;(comint-read-input-ring t)
;;(custom-set-variables
;; '(matlab-shell-command-switches '("-nodesktop -nosplash")))


;; octave part
;; (add-to-list 'exec-path "/Applications/Octave-cli.app/Contents/MacOS")
;; (autoload 'octave-mode "octave-mod" nil t)
;; (setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
;; (add-hook 'octave-mode-hook
;;           (lambda ()
;;             (abbrev-mode 1)
;;             (auto-fill-mode 1)
;;             (if (eq window-system 'x) (font-lock-mode 1))
;;             )
;;           )
;;(autoload 'run-octave "octave-inf" nil t)



(provide 'setup-matlab)
