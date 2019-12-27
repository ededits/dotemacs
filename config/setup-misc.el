;; Seed the random-number generator
(random t)

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Add Urban Dictionary to webjump (C-x g)
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '("Urban Dictionary" .
                                 [simple-query
                                  "www.urbandictionary.com"
                                  "http://www.urbandictionary.com/define.php?term="
                                  ""])))




;; A bit of misc cargo culting in misc.el
(setq xterm-mouse-mode t)
;; some more stuff about the mouse usage
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)


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





;; Map Modifier-CyrillicLetter to the underlying Modifier-LatinLetter, so that
;; control sequences can be used when keyboard mapping is changed outside of
;; Emacs.
;;
;; For this to work correctly, .emacs must be encoded in the default coding
;; system.
;;
(require 'cl)
(mapcar*
 (lambda (r e) ; R and E are matching Russian and English keysyms
   ;; iterate over modifiers
   (mapc (lambda (mod)
           (define-key input-decode-map
             (vector (list mod r)) (vector (list mod e))))
         '(control meta super hyper))
   ;; finally, if Russian key maps nowhere, remap it to the English key without
   ;; any modifiers
   (define-key local-function-key-map (vector r) (vector e)))
 "йцукенгшщзхъфывапролджэячсмитьбю"
 "qwertyuiop[]asdfghjkl;'zxcvbnm,.")


;; autopair braces
(electric-pair-mode)


;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)



;;------------------------------------------------------------
;; AUTOCOMPLETE
;;------------------------------------------------------------
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; (defun ac-c-header-init ()
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'ac-sources 'ac-source-c-headers)
;;   (add-to-list 'achead:include-directories '"/opt/gcc/6.2.0/include/c++/6.2.0")
;;   (add-to-list 'achead:include-directories '"/opt/gcc/6.2.0/include/c++/6.2.0/x86_64-apple-darwin16.1.0")
;;   (add-to-list 'achead:include-directories '"/opt/gcc/6.2.0/include/c++/6.2.0/backward")
;;   (add-to-list 'achead:include-directories '"/opt/gcc/6.2.0/include/c++/6.2.0/backward")
;;   (add-to-list 'achead:include-directories '"/opt/gcc/6.2.0/include")
;;   )
;; (add-hook 'c++-mode-hook 'ac-c-header-init)
;; (add-hook 'c-mode-hook 'ac-c-header-init)
;; ;;; set the trigger key so that it can work together with yasnippet on tab key,
;; ;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;; ;;; activate, otherwise, auto-complete will
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")


;;------------------------------------------------------------
;; IEdit
;;------------------------------------------------------------
;; fix mac keybinding bug
(define-key global-map (kbd "C-c ;") 'iedit-mode)




;;------------------------------------------------------------
;; CEDET
;;------------------------------------------------------------
;; (semantic-mode 1)
;; ;; adds semantic as a suggestion backend to autocomplete
;; ;; and hooks this function to c-mode-common-hook
;; (defun add-semanctic-to-autocomplete()
;;   (add-to-list 'ac-sources 'ac-source-semantic))
;; (add-hook 'c-mode-common-hook 'add-semanctic-to-autocomplete)


;; clear sreen in eshell the same way as in regular terminal
(defun eshell-clear ()
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  (interactive)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
	;; simply delete the region
	(delete-region (point-min) (point-max)))
        (eshell-send-input) )
(add-hook 'eshell-mode-hook
           '(lambda () (define-key eshell-mode-map "\C-l" 'eshell-clear)))


(provide 'my-misc)



(setq debug-on-error t)

(provide 'setup-misc)
