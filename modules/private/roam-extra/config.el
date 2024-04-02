;;; private/roam-extra/config.el -*- lexical-binding: t; -*-

(after! org-roam
  (add-hook 'find-file-hook #'roam-extra:update-todo-tag)
  (add-hook 'before-save-hook #'roam-extra:update-todo-tag)
  (advice-add 'org-agenda :before #'roam-extra:update-todo-files))

(use-package! org-ql
  :after org
  :commands org-ql-search
  )

(use-package! org-ql-agenda
  :after org
  :commands org-ql-agenda
  )

(use-package! websocket
    :after org-roam)
(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))
