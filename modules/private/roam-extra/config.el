;;; private/roam-extra/config.el -*- lexical-binding: t; -*-

(after! org-roam
  (add-hook 'before-save-hook #'roam-extra:update-todo-tag)
  (advice-add 'org-agenda :before #'roam-extra:update-todo-files)
  ;;  TODO: make hooks for things like logging to dailies
  (defun roam-extra:message-todo-tag-removed ()
    (message "Removing roam todo tag from %s." (buffer-file-name)))
  (add-hook 'before-remove-roam-todo-tag-hook #'roam-extra:message-todo-tag-removed)

(defun roam-extra:dailies-capture-appointment ()
  (interactive)
  "Create an entry in the daily-note for an appointment using the calendar."
  ;; for unexplained reasons daily-goto-* functions break when I add templates
  (defvar org-roam-dailies-capture-templates) ;; make it dynamic I guess?
  (let ((org-roam-dailies-capture-templates
         '(("d" "default" entry "* %?" :target
            (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
           ("a" "appointment" entry "* APPT %T %?\n:PROPERTIES:\n:APPT_WARNTIME: 30\n:END:"
            ;;TODO: do not include time if it is not given?
            :target (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Appointments"))))))
    (org-roam-dailies-capture-date nil nil "a"))
  ;; TODO can I just specify the file to look at for appts?
  (org-agenda-to-appt)))

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
