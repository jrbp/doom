;;; private/astro/config.el -*- lexical-binding: t; -*-

(use-package! astro-ts-mode
  :config
  (global-treesit-auto-mode)
  (add-hook 'astro-ts-mode-hook #'lsp 'append)
  ;; need to do the following in project for formatting to work
  ;; npm i --save-dev prettier prettier-plugin-astro
  (after! apheleia
    (add-to-list 'apheleia-mode-alist '(astro-ts-mode . prettier-astro))
    (add-to-list 'apheleia-formatters
                 '(prettier-astro npx "prettier" "--stdin-filepath"
                   filepath "--plugin=prettier-plugin-astro" "--parser=astro"
                   (apheleia-formatters-indent "--use-spaces" "--tab-width"
                                               'astro-ts-mode-indent-offset))))

  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision "master"
                       :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe))
  :after treesit-auto)
