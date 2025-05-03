;;; private/ox-zola/config.el -*- lexical-binding: t; -*-
(use-package! ox-hugo)
(use-package! ox-zola
  :after (ox-hugo)
  :config (require 'ox-hugo))
