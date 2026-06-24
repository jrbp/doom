;;; private/ghostel/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  ;; :bind
  :config
  (let ((method (cadr (assoc-string "rpc" ghostel-tramp-shells))))
    (if method
        (setf method 'login-shell)     
      (push (list "rpc" 'login-shell) ghostel-tramp-shells)))
  ;; TODO: doom vterm popup like behavior
  (map! :leader :prefix-map ("o" . "open")
        :desc "ghostel term" "g" #'ghostel
        :desc "ghostel Term" "G" (lambda () (interactive) (ghostel '(4)))))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  (map! (:map evil-ghostel-mode-map
         :localleader
         "m" #'ghostel-semi-char-mode
         "M" #'ghostel-char-mode
         "e" #'ghostel-emacs-mode
         "E" #'ghostel-copy-mode
         "l" #'ghostel-line-mode
         "t" #'evil-ghostel-toggle-send-escape)))

;; (use-package ghostel-eshell
;;   :after (ghostel)
;;   :hook (eshell-load . ghostel-eshell-visual-command-mode))

;; (use-package ghostel-comint
;;   :after (ghostel)
;;   :hook (after-init . ghostel-comint-global-mode))
