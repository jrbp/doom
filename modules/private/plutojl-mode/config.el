;;; private/plutojl-mode/config.el -*- lexical-binding: t; -*-


(use-package! plutojl-mode
  :after julia-mode
  :config
  (add-hook #'julia-mode-hook #'plutojl-maybe-enable-plutojl-mode)
  (add-hook #'plutojl-mode-hook #'auto-revert-mode)
  (map! :map plutojl-mode-map
        (:leader
         (:prefix "i"
          :desc "insert cell at point"  :n "c" 'plutojl-insert-cell-at-point
          :desc "insert makrdown cell at point"  :n "m" 'plutojl-insert-markdown-cell-at-point
          :desc "insert makrdown cell at point"  :n "o" 'plutojl-insert-org-cell-at-point
          :desc "insert makrdown cell at point"  :n "h" 'plutojl-insert-html-cell-at-point
          ))
        :desc "go to previous cell"  :n "[[" 'plutojl-goto-previous-cell
        :desc "go to next cell"  :n "]]" 'plutojl-goto-next-cell
        :desc "fold cell"  :n "Za" 'plutojl-toggle-fold-cell ; folds in pluto, not emacs
        )
                                        ;(define-key map (kbd "C-c C-d") 'plutojl-delete-cell-at-point)
                                        ;(define-key map (kbd "M-<down>") 'plutojl-move-cell-down)
                                        ;(define-key map (kbd "M-<up>") 'plutojl-move-cell-up)

  )
