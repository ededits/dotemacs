;;------------COLOR THEME--------------------------------------
(require 'setup-colorscheme)

;; (require 'powerline)
;; (setq powerline-arrow-shape 'curves)
;; (powerline-vim-theme)

;; let's use telephone-line instead of powerline
(require 'telephone-line)
(telephone-line-mode 1)




;; stop annoying beeps anf flash warnings
;;(setq visible-bell t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)


;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1) 


;; No menu bars
(menu-bar-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))



;------------Modeline LOOK----------------------------------------
;; display current time in the modeline
(display-time)
(column-number-mode)

(provide 'setup-appearance)
