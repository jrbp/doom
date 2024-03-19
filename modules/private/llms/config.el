;;; private/llms/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (load-library "~/.config/doom/modules/private/llms/apikeys.el.gpg")
  (setq! gptel-api-key (alist-get 'openai llm-apikey-alist)))
