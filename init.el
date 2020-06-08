
;; get rid of all the unneeded GUI elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; No splash screen please
(setq inhibit-startup-message t)
;; Set path to dependencies
(setq config-dir
      (expand-file-name "~/.emacs.d/config" user-emacs-directory))
(setq config-dir
      (expand-file-name "~/.emacs.d/config/"))

;; Set up load path
(add-to-list 'load-path config-dir)
(let ((default-directory  "~/.emacs.d/config/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

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
;; this section does not need to be ran every
;; time emacs starts. Uncomment when needed
;; (which is first start on new config)
(require 'setup-package)
;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   (cons 'auctex gnu)
   ;;(cons 'eglot gnu)
   (cons 'use-package melpa)
   (cons 'smooth-scrolling melpa)
   (cons 'undo-tree gnu)
   (cons 'expand-region melpa) ;; Increase selected region by semantic units.
   (cons 'helm melpa)
   (cons 'helm-swoop melpa)
   (cons 'migemo melpa) ;; this is for japaneze character and helm,
                        ;; but for some reason helm-swoop won't work
                        ;; without it. (cons 'move-text melpa)
   (cons 'telephone-line melpa)
   (cons 'monokai-theme melpa)
   ;;    (cons 'popup melpa)
   (cons 'smartparens melpa)
   (cons 'smex melpa)
   (cons 'eglot melpa)
   ;;(cons 'change-inner melpa)
   ;;    (cons 'auto-complete melpa)
   ;;    (cons 'dash melpa)
   ;;    (cons 'yasnippet melpa)
   ;;    (cons 'highlight-indent-guides melpa)
   (cons 'spinner gnu) ;; needed by lsp-mode and I have problems
   ;;                     ;; installing it as an lsp-mode dependancy
   (cons 'lsp-mode melpa)
   (cons 'lsp-ui melpa)
   (cons 'company-lsp melpa)
   (cons 'lsp-treemacs melpa)
   (cons 'projectile melpa)
   (cons 'iedit melpa) ;; interactive edit, very powerful in combination with narrow-to-region
   ;;    ;; these I will only keep here as a reference for the future
   ;;    ;;(cons 'powerline melpa)
   ;;    ;;(cons 'molokai-theme melpa)
   ;;    ;;(cons 'dracula-theme melpa)
   ;;    ;;(cons 'frame-cmds melpa)
   ;;    ;;(cons 'frame-fns melpa)
   ;;    ;;(cons 'auctex melpa)
   ;;    ;;(cons 'elpy melpa)
   ;;    ;; (cons 'counsel melpa)    
   ))
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages))
  )



;; start with a bit of sane misc settings
(require 'sane-defaults)
;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
;; I am not using dired much, so commenting out for now
;; but dired is cool, take a look at it
;; (eval-after-load 'dired '(require 'setup-dired))
(require 'setup-latex)
;;(require 'setup-matlab) ;; both matlab and octave
(require 'expand-region);;  C-. to expand, C-, to contract
;;(require 'change-inner)  ;;to enable VIM-like ci' and  co', M-I for inner M-O for outter
;; smex gives me suggestions about commands with fuzzy matching ido-style for M-x
(require 'smex)
(smex-initialize)
;; Setup key bindings
(require 'setup-keybindings)
;; use smartparen for highlighted parenthesis
(smartparens-global-mode t) 
(require 'smartparens-config)  
;; when I am on the mac, do some specific settings
(when is-mac (require 'setup-mac))

;; load all the LSP goodness
(require 'setup-lsp)


(require 'setup-appearance)
;; instead of helm look at ivy in the future
(require 'setup-helm)


;; setup for coding 
(require 'setup-coding)
;; enable company mode in all buffers
;; (add-hook 'after-init-hook 'global-company-mode)
;; Misc
(require 'setup-misc)
;; iedit mode
(require 'iedit)
;; ;; I want to play with Julia programming language
;; ;;(require 'julia-mode)


;; change prefix for all the lsp- commads:
(setq lsp-keymap-prefix "C-c C-l")
(require 'lsp-mode)
(define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)


;; ;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;; Run at full power please, and don't ask for confirmation for these commands
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'autopair-newline 'disabled nil)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "config/defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1 -shell-escape")
 '(TeX-PDF-mode t)
 '(TeX-command-list
   '(("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
 '(TeX-kpathsea-path-delimiter ":")
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
 '(TeX-view-program-selection
   '(((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Skim")
     (output-html "xdg-open")))
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("f9aede508e587fe21bcfc0a85e1ec7d27312d9587e686a6f5afdbb0d220eab50" default))
 '(fci-rule-color "#3C3D37")
 '(font-latex-fontify-script nil)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(migemo helm-swoop company counsel use-package undo-tree telephone-line smooth-scrolling smex smartparens projectile monokai-theme iedit helm change-inner auctex))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(preview-gs-command "/usr/local/bin/gs")
 '(preview-gs-options
   '("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-1-face :height 1.0))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :height 1.0))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face :height 1.0))))
 '(font-latex-sectioning-5-face ((t (:inherit default :foreground "yellow"))))
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil)))
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
