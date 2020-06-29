;; Bootstrap `use-package'
(require 'package)

;;(defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "http://melpa.org/packages/"))

;; Add melpa to package repos
(add-to-list 'package-archives melpa)

(package-initialize)


(setq package-enable-at-startup nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; load my config
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;   END OF MY INIT   ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 '(font-latex-fontify-script nil)
 '(package-selected-packages
   '(ido-vertical-mode auto-complete counsel org-bullets monokai-theme iedit projectile lsp-treemacs company-lsp lsp-ui lsp-mode spinner eglot smex smartparens telephone-line expand-region undo-tree smooth-scrolling try use-package))
 '(preview-gs-command "/usr/local/bin/gs")
 '(preview-gs-options
   '("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
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
