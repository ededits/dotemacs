

;; change prefix for all the lsp- commads:
(setq lsp-keymap-prefix "C-c C-l")

(require 'lsp-mode)
(define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)


(provide 'setup-lsp)





