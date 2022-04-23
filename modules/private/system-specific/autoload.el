;;; private/system-specific/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun jrb-sys/set-mac ()
  (setq jrb-sys/ext-term-command "open -aiterm&")
  (add-to-list 'org-file-apps '("\\.pptx\\'" . "open %s"))
  (setq comp-deferred-compilation t)
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
  (setq exec-path (append exec-path '("/Library/TeX/texbin"))))

;;;###autoload
(defun jrb-sys/set-linux-hpc ()
  (setq jrb-sys/ext-term-command "gnome-terminal"))

;;;###autoload
(defun jrb-sys/set-linux-personal ()
  (setq comp-deferred-compilation t)
  (setq jrb-sys/ext-term-command "termite&"))

;;;###autoload
(defun jrb-sys/set-wsl ()
  (setq comp-deferred-compilation t)
  (setq jrb-sys/ext-term-command "gnome-terminal"))

;;;###autoload
(defun jrb-sys/open-ext-term ()
  (interactive)
  (call-process-shell-command jrb-sys/ext-term-command nil 0))
