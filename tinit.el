;; test init.el for experimets


(global-set-key "\M-?" 'help-command)

(global-set-key "\C-h" 'backward-delete-char)

(global-set-key "\C-x\C-n" 'other-window)
(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))
(global-set-key "\C-x\C-p" 'other-window-backward)




