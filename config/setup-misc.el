;; Seed the random-number generator
(random t)

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)


;; A bit of misc cargo culting in misc.el
(setq xterm-mouse-mode t)
;; some more stuff about the mouse usage
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)


;; В новой версии Емакс 24.1 при включенной системной русской
;; раскладке можно вводить командные комбинации с любыми
;; символами (с модификаторами и даже без), которые привязаны к
;; командам, кроме `self-insert-command'. При этом, русские буквы
;; автоматически транслируются в соответствующие английские.
;; Например, последовательность `C-ч и' переводится в `C-x b' и
;; запускает `switch-to-buffer'. Всё это получается при помощи такой
;; функции:
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))
(reverse-input-method 'russian-computer)


;;; yasnippet
;;; should be loaded before auto complete so that they can work together
;; (require 'yasnippet)
;; (yas-global-mode 1)



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


(provide 'setup-misc)
