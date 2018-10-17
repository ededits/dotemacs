;;------------------------------------------------------------
;; LaTeX
;;------------------------------------------------------------
;; SHIFT+CMD+click -- opens Skim and positions cursor at the same place
;;(setq exec-path (append exec-path '("/usr/texbin/")))
;--------------------------------------------------
;; (setq exec-path (append exec-path '("/opt/local/bin")))
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
;; (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; set PATH to see pygmentize
(setenv "PATH" (concat "/opt/anaconda2/bin:" (getenv "PATH")))

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;---------------------------------------------------
(custom-set-variables
 '(LaTeX-command "latex  -synctex=1 --shell-escape")
 '(TeX-PDF-mode t)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(preview-gs-command "/usr/local/bin/gs")
 '(preview-gs-options (quote
                       ("-q"
                        "-dNOPAUSE"
                        "-DNOPLATFONTS"
                        "-dPrinted"
                        "-dTextAlphaBits=4"
                        "-dGraphicsAlphaBits=4"))
                      )
 '(LaTeX-command "latex -synctex=1")
 '(TeX-view-program-list
   (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))))
 '(TeX-view-program-selection
   (quote (
           ((output-dvi style-pstricks) "dvips and gv")
           (output-dvi "xdvi")
           (output-pdf "Skim")
           (output-html "xdg-open")
           ))
   )
 )
;;  integrate auctex with reftex
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; prompt me for all labels
(setq reftex-insert-label-flags (quote ("s" "slreft")))


;; Don't enlarge and fontify latex sections please
(custom-set-faces
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-1-face :height 1.0))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :height 1.0))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face :height 1.0))))
 '(font-latex-sectioning-5-face ((t (:inherit default :foreground "yellow"))))
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil)))
 )


;; a note about building autex. I do it by using
;; ./configure --prefix=/Users/eugene/.emacs.d/site-lisp/auctex/ \
;; --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs \
;; --with-lispdir=/Users/eugene/.emacs.d/site-lisp/auctex \
;; --with-texmf-dir=/usr/local/texlive/texmf-local
;;
;; and then make & make install
;;
;; finally autoload latex-math-mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(provide 'setup-latex)
