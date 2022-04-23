;;; private/system-specific/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun jrb-sys/set-mac ()
  (setq jrb-sys/ext-term-command "open -aiterm&"))

;;;###autoload
(defun jrb-sys/set-linux-hpc ()
  (setq jrb-sys/ext-term-command "gnome-terminal"))

;;;###autoload
(defun jrb-sys/set-linux-personal ()
  (setq jrb-sys/ext-term-command "termite&"))

;;;###autoload
(defun jrb-sys/set-wsl ()
  (setq jrb-sys/ext-term-command "gnome-terminal"))

;;;###autoload
(defun jrb-sys/open-ext-term ()
  (interactive)
  (call-process-shell-command jrb-sys/ext-term-command nil 0))
