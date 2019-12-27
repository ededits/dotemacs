;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DARK THEMES DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;

(defun set-dark-scheme () ;;---------------------------------
  (interactive)
  (load-theme 'darkokai 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey20")
  (setq-default cursor-type '(bar . 3))
  (set-cursor-color "red")
  (set-face-background 'region "steel blue"))


(defun set-dark-scheme() ;;---------------------------------
  (interactive)
  (load-theme 'dracula 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey15")
  (set-cursor-color 'red)
  (set-face-attribute 'region nil :background "thistle4")
  (setq-default cursor-type '(bar . 2)))


(defun set-dark-scheme() ;;---------------------------------
  (interactive)
  (load-theme 'molokai 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey20")
  (set-cursor-color 'red)
  (set-face-attribute 'region nil :background "thistle4")
   (setq-default cursor-type '(bar . 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LIGHT THEMES DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;



(defun set-light-scheme() ;;---------------------------------
  (interactive)
  (load-theme 'whiteboard 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey90")
  (set-cursor-color 'red)
  (setq-default cursor-type '(bar . 2)))

(defun set-light-scheme() ;;---------------------------------
  (interactive)
  (load-theme 'leuven 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey90")
  (set-cursor-color "red")
  (setq-default cursor-type '(bar . 2)))


(defun set-light-scheme() ;;---------------------------------
  (interactive)
  (load-theme 'solarized-light 1)
  (global-hl-line-mode 1)
  (set-cursor-color 'red)
  (setq-default cursor-type '(bar . 2)))

(defun set-light-scheme() ;;---------------------------------
  (interactive)
  (load-theme 'tango 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey90")
  (set-cursor-color "red")
  (setq-default cursor-type '(bar . 2)))


(defun set-light-scheme() ;;---------------------------------
  (interactive)
  (load-theme 'sanityinc-tomorrow-day 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey90")
  (set-cursor-color "red")
  (setq-default cursor-type '(bar . 2)))


(defun set-dark-scheme () ;;---------------------------------
  (interactive)
  (load-theme 'monokai)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey20")
  (setq-default cursor-type '(bar . 3))
  (set-cursor-color "red")
  (set-face-background 'region "steel blue"))



(set-dark-scheme)
;;(set-light-scheme)

(provide 'setup-colorscheme)


