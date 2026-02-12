;;; private/secrets/config.el -*- lexical-binding: t; -*-

(setq! epg-pinentry-mode 'loopback)
(defvar jrb/secret-identity nil)
(defvar llm-apikey-alist nil)
(defvar epa-file-encrypt-to nil)
(ignore-errors
  (let ((file-name-handler-alist '(("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" . epa-file-handler))))
    (load-file (expand-file-name "modules/private/secrets/secrets.el.gpg" doom-user-dir))))
