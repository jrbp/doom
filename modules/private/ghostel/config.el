;;; private/ghostel/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  ;; :bind
  :config
  ;; tell ghostel how to know what shell to use in rpc tramp sessions
  (let ((method (cadr (assoc-string "rpc" ghostel-tramp-shells))))
    (if method
        (setf method 'login-shell)     
      (push (list "rpc" 'login-shell) ghostel-tramp-shells)))
  (set-popup-rule! "^\\*jrb:ghostel-popup" :size 0.35 :height 0.35 :vslot -5 :select t :modeline t :quit nil :ttl nil :parameters '((:transient . t) (:no-other-window . t)))
  ;; HACK: currently still have vterm installed, shadowing bindings while testing ghostel
  ;; note: julia-snail still relies on vterm so not ready to delete yet
  (map! :leader :prefix-map ("o" . "open")
        :desc "Toggle ghostel popup" "t" #'+ghostel/toggle
        :desc "Open ghostel terminal here" "T" (lambda () (interactive) (ghostel '(4)))))

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
