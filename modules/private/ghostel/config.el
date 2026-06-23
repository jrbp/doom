;;; private/ghostel/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  ;; :bind
  :config
  (let ((method (cadr (assoc-string "rpc" ghostel-tramp-shells))))
    (if method
        (setf method 'login-shell)     
      (push (list "rpc" 'login-shell) ghostel-tramp-shells)))
  (map! :map evil-ghostel-mode-map
        :localleader
        "m" #'ghostel-semi-char-mode
        "M" #'ghostel-char-mode
        "e" #'ghostel-emacs-mode
        "E" #'ghostel-copy-mode
        "l" #'ghostel-line-mode))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))
;; (use-package ghostel-eshell
;;   :after (ghostel)
;;   :hook (eshell-load . ghostel-eshell-visual-command-mode))

;; (use-package ghostel-comint
;;   :after (ghostel)
;;   :hook (after-init . ghostel-comint-global-mode))
