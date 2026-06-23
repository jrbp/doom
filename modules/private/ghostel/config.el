;;; private/ghostel/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  ;; :bind
  :config
  (if (assoc "rpc" ghostel-tramp-shells #'equal)
      (setf (alist-get "rpc" ghostel-tramp-shells nil nil #'equal) 'login-shell)
    (push (list "rpc" 'login-shell) ghostel-tramp-shells))
  )

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))
;; (use-package ghostel-eshell
;;   :after (ghostel)
;;   :hook (eshell-load . ghostel-eshell-visual-command-mode))

;; (use-package ghostel-comint
;;   :after (ghostel)
;;   :hook (after-init . ghostel-comint-global-mode))
