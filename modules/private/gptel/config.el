;;; private/gptel/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (let ((file-name-handler-alist '(("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" . epa-file-handler))))
    (load-file (expand-file-name "modules/private/gptel/secrets/apikeys.el.gpg" doom-user-dir)))
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-api-key (alist-get 'openai llm-apikey-alist))
  (set-popup-rules!
    '(("^\\*ChatGPT.*" :ignore t)))
  (gptel-make-gemini "Gemini" :key (alist-get 'gemini llm-apikey-alist) :stream t)
  )
