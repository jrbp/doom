;;; private/gptel/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (let ((file-name-handler-alist '(("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" . epa-file-handler))))
    (load-file (expand-file-name "modules/private/gptel/secrets/apikeys.el.gpg" doom-user-dir)))
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-api-key (alist-get 'openai llm-apikey-alist))
  (set-popup-rules!
    '(("^\\*ChatGPT.*" :quit nil)
      ("^\\*Gemini.*" :quit nil)
      ("^\\*Claude.*" :quit nil)))
  (gptel-make-gemini "Gemini" :key (alist-get 'gemini llm-apikey-alist) :stream t)
  (gptel-make-anthropic "Claude" :stream t :key (alist-get 'claude llm-apikey-alist))
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))
