;;; private/secrets/config.el -*- lexical-binding: t; -*-

(setq! epg-pinentry-mode 'loopback)
(let ((file-name-handler-alist '(("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" . epa-file-handler))))
  (load-file (expand-file-name "modules/private/secrets/secrets.el.gpg" doom-user-dir)))
