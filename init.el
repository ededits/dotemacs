;; Turn off mouse interface early in startup to avoid momentary display

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; that's OK, leave me menu bar pls
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;

;;;; No splash screen please ... jeez
(setq inhibit-startup-message t)
;;
;;;; Set path to dependencies
;;(setq config-dir
;;      (expand-file-name "~/.emacs.d/config" user-emacs-directory))
(setq config-dir
      (expand-file-name "~/.emacs.d/config/"))

;; Set up load path
(add-to-list 'load-path config-dir)
(let ((default-directory  "~/.emacs.d/config/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)
  )

;; restore the same buffers that were opened last time
(setq desktop-restore-eager 20)
(desktop-save-mode 1)
(savehist-mode 1)




;; place all backups in one directory (~/.emacs.d/backup)
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups
;; the same thing for temporary files.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/\\1" t)))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup packages
(require 'setup-package)
;;
;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   (cons 'smooth-scrolling melpa)
   (cons 'undo-tree melpa)
   (cons 'expand-region melpa)
   (cons 'move-text melpa)
   (cons 'powerline melpa)
   (cons 'molokai-theme melpa)
   (cons 'monokai-theme melpa)
   (cons 'dracula-theme melpa)
   (cons 'popup melpa)
   (cons 'smartparens melpa)
   (cons 'smex melpa)
   ;;(cons 'frame-cmds melpa)
   ;;(cons 'frame-fns melpa)
   (cons 'auto-complete melpa)
   (cons 'dash melpa)
   (cons 'auctex melpa)
   (cons 'yasnippet melpa)
   (cons 'highlight-indent-guides melpa)
   ))
;; (condition-case nil
;;     (init--install-packages)
;;   (error
;;    (package-refresh-contents)
;;    (init--install-packages))
;;   )


;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
;; I am not using dired much, so commenting out for now
;; but dired is cool, take a look at it
;;(eval-after-load 'dired '(require 'setup-dired))
(require 'setup-latex)
(require 'setup-matlab) ;; both matlab and octave
(require 'expand-region) 
(require 'change-inner) ;; to enable VIM-like ci' and  co'
;; I want to play with Julia programming language
(require 'julia-mode)
;;
;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "config/defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))


;; Smart M-x is smart...
;; smex gives me suggestions about commands with fuzzy matching ido-style
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'setup-keybindings)

;; use smartparen for highlighted parenthesis
(require 'smartparens-config)


;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
;;
;;
;;;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'autopair-newline 'disabled nil)


;; Misc
(require 'setup-appearance)
(require 'setup-misc)
(when is-mac (require 'setup-mac))
(require 'helm)
(require 'helm-config)
;; enable company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (highlight-indent-guides auto-complete frame-cmds smex ido-ubiquitous dracula-theme monokai-theme molokai-theme powerline move-text dired-details main-line auctex color-theme-sanityinc-tomorrow company helm ggtags julia-mode yasnippet smartparens change-inner expand-region undo-tree smooth-scrolling)))
 '(preview-gs-command "/usr/local/bin/gs")
 '(preview-gs-options
   (quote
    ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(region ((t (:background "steel blue")))))

