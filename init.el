
;; get rid of all the unneeded GUI elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;;; No splash screen please
(setq inhibit-startup-message t)
;;;; Set path to dependencies
(setq config-dir
      (expand-file-name "~/.emacs.d/config" user-emacs-directory))
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

;; ;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup packages
;; this section does not need to be ran every
;; time emacs starts. Uncomment when needed
;; (which is first start on new config)
(require 'setup-package)
;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   (cons 'smooth-scrolling melpa)
   (cons 'undo-tree melpa)
   (cons 'expand-region melpa)
   (cons 'move-text melpa)
   (cons 'telephone-line melpa)
   (cons 'monokai-theme melpa)
   (cons 'popup melpa)
   (cons 'smartparens melpa)
   (cons 'smex melpa)
   (cons 'change-inner melpa)
   (cons 'auto-complete melpa)
   (cons 'dash melpa)
   (cons 'yasnippet melpa)
   (cons 'highlight-indent-guides melpa)
   (cons 'lsp-mode melpa)
   (cons 'lsp-ui melpa)
   (cons 'company-lsp melpa)
   (cons 'lsp-treemacs melpa)
   (cons 'projectile melpa)
   ;; these I will only keep here as a reference for the future
   ;;(cons 'powerline melpa)
   ;;(cons 'molokai-theme melpa)
   ;;(cons 'dracula-theme melpa)
   ;;(cons 'frame-cmds melpa)
   ;;(cons 'frame-fns melpa)
   ;;(cons 'auctex melpa)
   ;;(cons 'elpy melpa)
   ))
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages))
  )


;; ;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
;; I am not using dired much, so commenting out for now
;; but dired is cool, take a look at it
;;(eval-after-load 'dired '(require 'setup-dired))
(require 'setup-latex)
;;(require 'setup-matlab) ;; both matlab and octave
(require 'expand-region) 
(require 'change-inner) ;; to enable VIM-like ci' and  co'


;; I want to play with Julia programming language
;;(require 'julia-mode)

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


;; ;; Emacs server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))
;; ;;
;; ;;
; ;;;; Run at full power please
;; (put 'downcase-region 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)
;; (put 'autopair-newline 'disabled nil)


;; ;; Misc
(require 'setup-appearance)
(require 'setup-misc)
(when is-mac (require 'setup-mac))
;; (require 'helm)
;; (require 'helm-config) 


;; setup for coding in python
(require 'setup-coding)

;; ;; enable company mode in all buffers
;; (add-hook 'after-init-hook 'global-company-mode)


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
 '(custom-safe-themes
   '("f9aede508e587fe21bcfc0a85e1ec7d27312d9587e686a6f5afdbb0d220eab50" "83ae405e25a0a81f2840bfe5daf481f74df0ddb687f317b5e005aa61261126e9" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "947190b4f17f78c39b0ab1ea95b1e6097cc9202d55c73a702395fc817f899393" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" default))
 '(font-latex-fontify-script nil)
 '(package-selected-packages
   '(lsp-mode projectile undo-tree smooth-scrolling smex smartparens powerline move-text monokai-theme molokai-theme highlight-indent-guides elpy dracula-theme darkokai-theme change-inner auto-complete))
 '(preview-gs-command "/usr/local/bin/gs")
 '(preview-gs-options
   '("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
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
 '(font-latex-superscript-face ((t nil))))


