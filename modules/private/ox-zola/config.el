;;; private/ox-zola/config.el -*- lexical-binding: t; -*-
(use-package! ox-zola
  :derfer-incrementally ox-hugo
  :after (ox)
  :config
  (require 'ox-hugo))
