(use-package try
  :ensure t)
(use-package smooth-scrolling
  :ensure t)
(use-package undo-tree
  :ensure t)
(use-package expand-region
  :ensure t)
(use-package telephone-line
  :ensure t)
(use-package smartparens
  :ensure t)
(use-package smex 
  :ensure t)
(use-package eglot
  :ensure t)
(use-package spinner
  :ensure t)
(use-package lsp-mode
  :ensure t)
(use-package lsp-ui
  :ensure t)
(use-package company-lsp
  :ensure t)
(use-package lsp-treemacs
  :ensure t)
(use-package projectile
  :ensure t)
(use-package iedit 
  :ensure t)
(use-package monokai-theme
  :ensure t)

;; get rid of all the unneeded GUI elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; No splash screen please
(setq inhibit-startup-message t)  
;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)
;; Always display line and column numbers in the modeline
(setq line-number-mode t)
(setq column-number-mode t)
;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)
;; stop annoying beeps anf flash warnings
;;(setq visible-bell t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Set up load path
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


    ;; Allow pasting selection outside of Emacs
    (setq x-select-enable-clipboard t)

    ;; Auto refresh buffers
    (global-auto-revert-mode 1)

    ;; Also auto refresh dired, but be quiet about it
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)

    ;; Show keystrokes in progress
    (setq echo-keystrokes 0.1)
    ;; Move files to trash when deleting
    (setq delete-by-moving-to-trash t)

    ;; Real emacs knights don't use shift to mark things
    (setq shift-select-mode nil)

    ;; UTF-8 please
    (setq locale-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)

    ;; Remove text in active region if inserting text
    (delete-selection-mode 1)

    ;; Lines should be 80 characters wide, not 72
    (setq fill-column 80)

    ;; Save a list of recent files visited. (open recent file with C-x f)
    (recentf-mode 1)
    (setq recentf-max-saved-items 100)

    ;; Undo/redo window configuration with C-c <left>/<right>
    (winner-mode 1)

    ;; Never insert tabs
    (set-default 'indent-tabs-mode nil)

    ;; Show me empty lines after buffer end
    (set-default 'indicate-empty-lines t)

    ;; Easily navigate sillycased words
    ;; treats constructions like 'thisIsSubwordsSet' as a bunch of words
    (global-subword-mode 1)

    ;; Don't break lines for me, please
    (setq-default truncate-lines t) 

    ;; Keep cursor away from edges when scrolling up/down
    ;; see ./site-lisp/smooth-scrolling.el
    (require 'smooth-scrolling)
    ;; fix the laggish scrolling please.
    (setq scroll-conservatively 10000)
    ;; scroll one line at a time (less "jumpy" than defaults)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse '1) ;; scroll window under mouse
    (setq scroll-step 1) ;; keyboard scroll one line at a time
    (setq scroll-margin 5)

    ;; A bit of misc cargo culting
    (setq xterm-mouse-mode t)

    ;; Represent undo-history as an actual tree (visualize with C-x u)
    ;; see ./site-lisp/undo-tree.el
    (setq undo-tree-mode-lighter "")
    (require 'undo-tree)
    (global-undo-tree-mode)

    ;; Sentences do not need double spaces to end
    (set-default 'sentence-end-double-space nil)

    ;; Add parts of each file's directory to the buffer name if not unique
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'forward)

    ;; A saner ediff
    (setq ediff-diff-options "-w")
    (setq ediff-split-window-function 'split-window-horizontally)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)

    ;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
    ;; that you can always see what's happening.
    (setq eval-expression-print-level nil)

    ;; When popping the mark, continue popping until the cursor actually moves
    ;; Also, if the last command was a copy - skip past all the expand-region cruft.
    (defadvice pop-to-mark-command (around ensure-new-position activate)
      (let ((p (point)))
        (when (eq last-command 'save-region-or-current-line)
          ad-do-it
          ad-do-it
          ad-do-it)
        (dotimes (i 10)
          (when (= p (point)) ad-do-it))))

    ;;;; Run at full power please, and don't ask for confirmation for these commands
    (put 'downcase-region 'disabled nil)
    (put 'narrow-to-region 'disabled nil)
    (put 'dired-find-alternate-file 'disabled nil)
    (put 'autopair-newline 'disabled nil)

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

;; move to next window
(global-set-key "\C-x\C-n" 'other-window)
;; move to previous window
(global-set-key "\C-x\C-p" 'other-window-backward)


;; "Ctrl+c <-" will restore the previous window configuration and 
;; "Ctrl+c ->" will redo the configuration you just destroyed.
(winner-mode 1)

;;------------COLOR THEME--------------------------------------
;; let's use telephone-line
(require 'telephone-line)
(telephone-line-mode 1)
;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1) 

;; ;; No menu bars please
;; (menu-bar-mode)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; ------------Modeline LOOK----------------------------------------
;; display current time in the modeline
;;(diplay-time)
;;(column-number-mode)

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

(defun set-dark-scheme () ;;---------------------------------
  (interactive)
  (load-theme 'monokai t)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey20")
  (setq-default cursor-type '(bar . 3))
  (set-cursor-color "red")
  (set-face-background 'region "steel blue"))

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
  (load-theme 'sanityinc-tomorrow-day 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey90")
  (set-cursor-color "red")
  (setq-default cursor-type '(bar . 2)))

(defun set-light-scheme() ;;---------------------------------
  (interactive)
  (load-theme 'tango 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "grey90")
  (set-cursor-color "red")
  (setq-default cursor-type '(bar . 2)))


;; set the theme
(set-dark-scheme)
;;(set-light-scheme)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(when is-mac
  ;; change command to meta, and ignore option to use weird Norwegian keyboard
  ;; (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)

  ;; Set default font
  (set-face-attribute 'default nil
                      :family "Pragmata Pro"
                      :height 140
                      :weight 'normal
                      :width 'normal)


  ;; make sure path is correct when launched as application
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path)

  ;; keybinding to toggle full screen mode
  (defun toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))
    )
  (global-set-key (quote [M-f10]) (quote toggle-frame-fullscreen))

  ;; Move to trash when deleting stuff
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")

  ;; Ignore .DS_Store files with ido mode
  ;;(add-to-list 'ido-ignore-files "\\.DS_Store")

  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)

  ;; Use aspell for spell checking: brew install aspell --lang=en
  (setq ispell-program-name "/opt/local/bin/aspell")
  )

;; Buffer-related defuns
(require 'imenu)

(defvar buffer-local-mode nil)
(make-variable-buffer-local 'buffer-local-mode)

(defun mode-keymap (mode-sym)
  (symbol-value (intern (concat (symbol-name mode-sym) "-map"))))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))

;; move to previous window 
;; inverse of other-window
(defun other-window-backward (&optional n)
  "Select Nth the previous window."
  (interactive "p")
  (other-window (- 1)))



(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))


(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

(defun file-name-with-one-directory (file-name)
  (concat (cadr (reverse (split-string file-name "/"))) "/"
          (file-name-nondirectory file-name)))

(defun recentf--file-cons (file-name)
  (cons (file-name-with-one-directory file-name) file-name))


;; commenting this out bacause I want to use helm-recentf
;; (defun recentf-ido-find-file ()
;;   "Find a recent file using ido."
;;   (interactive)
;;   (let* ((recent-files (mapcar 'recentf--file-cons recentf-list))
;;          (files (mapcar 'car recent-files))
;;          (file (completing-read "Choose recent file: " files)))
;;     (find-file (cdr (assoc file recent-files)))))

;; Basic text editing defuns
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (duplicate-region arg)
      (duplicate-current-line arg))))

(defun duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char start)
    (dotimes (i num)
      (insert region))))

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (when (eq (point-at-eol) (point-max))
    (goto-char (point-max))
    (newline)
    (forward-char -1))
  (duplicate-region num (point-at-bol) (1+ (point-at-eol))))


;; kill region if active, otherwise kill backward word
(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

;; copy region if active
;; otherwise copy to end of current line
;;   * with prefix, copy N whole lines
(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-whole-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation"
  (interactive)
  (back-to-indentation)
  (kill-line))

;; kill all comments in buffer
(defun comment-kill-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos)))))

(defun incs (s &optional num)
  (number-to-string (+ (or num 1) (string-to-number s))))

(defun change-number-at-point (arg)
  (interactive "p")
  (unless (or (looking-at "[0-9]")
              (looking-back "[0-9]"))
    (error "No number to change at point"))
  (while (looking-back "[0-9]")
    (forward-char -1))
  (re-search-forward "[0-9]+" nil)
  (replace-match (incs (match-string 0) arg) nil nil))

;; Defuns for working with files
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


(defun touch-buffer-file ()
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

(provide 'file-defuns)

;; Misc defuns go here
;; It wouldn't hurt to look for patterns and extract once in a while
(defmacro create-simple-keybinding-command (name key)
  `(defmacro ,name (&rest fns)
     (list 'global-set-key (kbd ,key) `(lambda ()
                                         (interactive)
                                         ,@fns))))

(create-simple-keybinding-command f2 "<f2>")
(create-simple-keybinding-command f5 "<f5>")
(create-simple-keybinding-command f6 "<f6>")
(create-simple-keybinding-command f7 "<f7>")
(create-simple-keybinding-command f8 "<f8>")
(create-simple-keybinding-command f9 "<f9>")
(create-simple-keybinding-command f10 "<f10>")
(create-simple-keybinding-command f11 "<f11>")
(create-simple-keybinding-command f12 "<f12>")

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

;; Add spaces and proper formatting to linum-mode. It uses more room
;; than necessary, but that's not a problem since it's only in use
;; when going to lines.
(setq linum-format
      (lambda (line)
        (propertize
         (format (concat " %"
                         (number-to-string
                          (length (number-to-string
                                   (line-number-at-pos (point-max)))))
                         "d ")
                 line)
         'face 'linum)))

(defun isearch-yank-selection ()
  "Put selection from buffer into search string."
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (isearch-yank-internal (lambda () (mark))))

(defun region-as-string ()
  (buffer-substring (region-beginning)
                    (region-end)))

(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

(eval-after-load "multiple-cursors"
  '(progn
     (unsupported-cmd isearch-forward-use-region ".")
     (unsupported-cmd isearch-backward-use-region ".")))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Fix kmacro-edit-lossage, it's normal implementation
;; is bound tightly to Cg-h
(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))

;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; expand-region -- Increase selected region by semantic units.
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\M-?" 'help-command)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)


;; Killing text
;;Kill the entire current line and reposition point at indentation
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; join lines
(global-set-key (kbd "C-c C-j") (lambda () (interactive) (join-line -1)))

 ;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") '(lambda () (interactive) (save-region-or-current-line 1)))

;; ;; File finding
;; (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; (global-set-key (kbd "C-c y") 'bury-buffer)
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;; (global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;; ;; helm-recentf instead please
;; (global-set-key (kbd "C-x f") 'helm-recentf)


;; ;; Edit file with sudo
;; (global-set-key (kbd "M-s e") 'sudo-edit)


;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'rotate-windows)
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)


;; Help should search more than just commands
;; (global-set-key (kbd "<f1> a") 'apropos)

;; Navigation bindings                         
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Completion at point                         
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Like isearch, but adds region (if any) to history and deactivates mark
(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; Like isearch-*-use-region, but doesn't fuck with the active region
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

;; Move more quickly                           
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; Query replace regex key binding             
(global-set-key (kbd "M-&") 'query-replace-regexp)


;; ;; Comment/uncomment block                  
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Create scratch buffer                       
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Move windows, even in org-mode              
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)   
(global-set-key (kbd "<s-down>") 'windmove-down)


;; Clever newlines                             
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
;;(global-set-key (kbd "<M-return>") 'new-line-in-between)


;; Duplicate region                            
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Sortingm
(global-set-key (kbd "M-s l") 'sort-lines)

;; Increase number at point (or other change based on prefix arg)
(global-set-key (kbd "C-+") 'change-number-at-point)


;; Buffer file functions
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)


;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)


;; increase and decrease font
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Add color to a shell running in emacs M-x shell
(global-set-key (kbd "C-c s") 'eshell)

;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

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
(setenv "PATH" (concat "/opt/anaconda3/bin:" (getenv "PATH")))

;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

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
 ;;'(LaTeX-command "latex -synctex=1")
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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1 -shell-escape")
 '(TeX-PDF-mode t)
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
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
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-kpathsea-path-delimiter ":")
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   (quote
    (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Skim")
     (output-html "xdg-open"))))
 '(font-latex-fontify-script nil)
 '(preview-gs-command "/usr/local/bin/gs")
 '(preview-gs-options
   (quote
    ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))))


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

;; bullets to look pretty
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)
;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)
;; set tasks states
(setq org-todo-keywords '((sequence "TODO" "BLOCKED" "INPROGRESS" "|" "DONE" "ARCHIVED")))

;; Setting Colours (faces) for todo states to give clearer view of work 
;; (setq org-todo-keyword-faces
;;       '(("TODO" . org-warning)
;;         ("BLOCKED" . "magenta")
;;         ("DONE" . "green")
;;         ("ARCHIVED" . "lightblue")))

;; set default file for TODO stuff 
(setq org-default-notes-file "~/Desktop/notes.org")

;; wrap test in the example and src construct
(defun wrap-example (b e)
  "wraps active region into #+begin_example .. #+end_example construct"
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (insert "#+begin_example\n") 
    (goto-char (point-max)) 
    (insert "\n#+end_example\n")))

(defun wrap-src (b e)
  "wraps active region into #+begin_src .. #+end_src construct"
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (insert "\n#+begin_src\n") 
    (goto-char (point-max)) 
    (insert "\n#+end_src\n")))
(global-set-key (kbd "C-x M-e") 'wrap-example)
(global-set-key (kbd "C-x M-s") 'wrap-src)

;; Don't enlarge and fontify headers
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 )

;; enable export to markdown
(eval-after-load "org"
  '(require 'ox-md nil t))
'(region ((t (:background "steel blue"))))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(require 'expand-region)  ;;C-. to expand, C-, to contract
;; use smartparen for highlighted parenthesis
(smartparens-global-mode t) 
(require 'smartparens-config)  

;; Seed the random-number generator
(random t)
;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; IEdit
(require 'iedit)
;; fix mac keybinding bug
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; smex gives me suggestions about commands with fuzzy matching ido-style for M-x
(require 'smex)
(smex-initialize)


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

;; load all the LSP goodness
(require 'lsp-mode)

;; change prefix for all the lsp- commads:
(setq lsp-keymap-prefix "C-c C-l")
(define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)

(require 'server)
(unless (server-running-p)
  (server-start))