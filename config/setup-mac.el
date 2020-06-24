;; change command to meta, and ignore option to use weird Norwegian keyboard
;; (setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)


;; mac friendly font
;;
;;(set-face-attribute 'default nil :font "Monaco-12")

;; (defvar myfont
;;   "Font that I want to use by default")

;; ;(setq myfont "-*-Ubuntu-normal-normal-normal-*-15-*-*-*-p-0-iso10646-1")
;; (setq myfont  "Pragmata Pro-14")

;; ;; Set default font
;; (set-face-attribute 'default nil
;;                     :family "Pragmata Pro"
;;                     :height 140
;;                     :weight 'normal
;;                     :width 'normal)

;; (set-default-font myfont)
;; (set-face-attribute 'default nil :font myfont)
;; (set-fontset-font "fontset-default" 'cyrillic myfont)
;; (set-fontset-font "fontset-default" 'greek myfont)

                                        ; this section is to be deleted
;; (set-default-font "-*-Ubuntu-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1")
;; (set-face-attribute 'default nil :font "-*-Ubuntu-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1")
;; (set-fontset-font "fontset-default" 'cyrillic '("-*-Ubuntu-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1"))
;; (set-fontset-font "fontset-default" 'greek '("-*-Ubuntu-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1"))
;;(set-default-font "PragmataPro-14")
;;(set-face-attribute 'default nil :font "PragmataPro-16")
;; (set-fontset-font "fontset-default" 'cyrillic '("PragmataPro-14"))
;; (set-fontset-font "fontset-default" 'greek '("PragmataPro-14"))
;;(set-face-attribute 'default nil :family "Anonymous Pro" :height 140)

;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; keybinding to toggle full screen mode
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreenq
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth))
  )
(global-set-key (quote [M-f10]) (quote toggle-frame-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/opt/local/bin/aspell")

(provide 'setup-mac)
