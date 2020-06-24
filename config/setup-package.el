(require 'package)

(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
;;(defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages"))

;; Add melpa to package repos
(add-to-list 'package-archives melpa)

(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
(package-install 'use-package))

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa"))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

(provide 'setup-package)
