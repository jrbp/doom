;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "Monospace" :size 18))

                                        ;(setq initial-buffer-choice "~/org/master.org")

;; disable doom splash image
(setq +doom-dashboard-functions (cdr +doom-dashboard-functions))

(after! lsp-julia
  (setq lsp-julia-default-environment "~/.julia/environments/v1.7")
  (setq lsp-julia-package-dir nil))

(after! (:and julia-repl inheritenv)
  (inheritenv-add-advice 'julia-repl-inferior-buffer))

(after! julia-repl
  (set-popup-rules!
    '(("^\\*julia.*" :ignore t)))
  (julia-repl-set-terminal-backend 'vterm)
  (when (modulep! :ui workspaces)
    ;(advice-remove '+julia--namespace-repl-buffer-to-workspace-a #'julia-repl--inferior-buffer-name)
    (defadvice! +julia--namespace-repl-buffer-to-workspace-a (&optional executable-key suffix)
      "Name for a Julia REPL inferior buffer. Uses workspace name (or non-nil suffix) for doom emacs"
      :override #'julia-repl--inferior-buffer-name
      (concat julia-repl-inferior-buffer-name-base ":" (or suffix (+workspace-current-name)))))
  )

(after! org
  (defadvice! +ob-julia-execute-in-repl (body params)
    :override #'org-babel-execute:julia
    (interactive)
    ;(let* ((session (cdr (assq :session params)))
    ;       (julia-repl-inferior-buffer-name-suffix (pcase session
    ;                                                 ("none" nil)
    ;                                                 (_ session))))
    ;       (julia-repl--send-string
    ;        (org-babel-expand-body:julia body params)))
    ; HACK
    ; just setting global var when evaluating so that e.g. julia-repl-edit etc work in that session
    ; better way would be to add babel block aware advice to those
    (let ((session (cdr (assq :session params))))
      (setq julia-repl-inferior-buffer-name-suffix (pcase session
                                                     ("none" nil)
                                                     (_ session)))
      (julia-repl--send-string
       (org-babel-expand-body:julia body params)))
    ))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\from_materials_cloud\\'"))

(map! :map evil-org-mode-map
      :after julia-repl
      :desc "sub latex to character" :ni "<A-tab>" 'julia-latexsub-or-indent
      :desc "repl run line"  :n "gl" 'julia-repl-send-line
      :desc "repl @edit"  :n "gd" 'julia-repl-edit
      :desc "repl @doc"  :n "gk" 'julia-repl-doc
      :desc "repl expand macro"  :n "gM" 'julia-repl-macroexpand
      :desc "repl list methods" :n    "gm" 'julia-repl-list-methods)

;; non warnings do not show up in lsp (mainly to remove "is not accessed" messages cluttering everything)
(setf lsp-diagnostic-filter (lambda (param work)
                              (puthash "diagnostics"
                                       (cl-remove-if (lambda (diag) (gethash "tags" diag))
                                                     (gethash "diagnostics" param))
                                       param)
                              param))

(defun jrb/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], insert the file name exactly as
  it appears in the minibuffer prompt.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to expand the file name to
  its fully canocalized path.  See `expand-file-name'."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (expand-file-name filename)))))

(set-popup-rules!
  '(("^\\*jupyter.*" :ignore t)))

;; make it so that by default ESC is sent to vterm
(add-hook! 'vterm-mode-hook #'evil-collection-vterm-toggle-send-escape)
(defun jrb/vterm-execute-current-line ()
  "Insert text of current line in vterm and execute."
  (interactive)
  (require 'vterm)
  (eval-when-compile (require 'subr-x))
  (let ((command (string-trim (buffer-substring
                               (save-excursion
                                 (beginning-of-line)
                                 (point))
                               (save-excursion
                                 (end-of-line)
                                 (point))))))
    (let ((buf (current-buffer)))
      (if-let (vwin (get-buffer-window
                      (format "*doom:vterm-popup:%s*"
                              (if (bound-and-true-p persp-mode)
                                  (safe-persp-name (get-current-persp))
                                "main"))))
          (select-window vwin)
        (+vterm/toggle nil))
      (vterm--goto-line -1)
      (message command)
      (vterm-send-string command)
      (vterm-send-return)
      (switch-to-buffer-other-window buf))))

                                        ; never did the google developers steps
                                        ; (defun my-open-calendar ()
                                        ;   (interactive)
                                        ;   (cfw:open-calendar-buffer
                                        ;    :contents-sources
                                        ;    (list
                                        ;     (cfw:org-create-source "Green")  ; orgmode source
                                        ;     ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
                                        ;    )))
                                        ;
                                        ; (defun cfw:open-org-calendar-with-cal1 ()
                                        ;   (interactive)
                                        ;   (let ((org-agenda-files '("/Users/jbonini/Dropbox_simons/org/gcal.org"))) ;;can use directory
                                        ;     (call-interactively #'+calendar/open-calendar)))
                                        ;
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(after! org
  ;; fix jupyter output see https://github.com/nnicandro/emacs-jupyter/issues/366
  (defun display-ansi-colors ()
    (ansi-color-apply-on-region (point-min) (point-max)))
  (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)
  ;; temp fix attachment bug invalid base64 data bug, see https://github.com/hlissner/doom-emacs/issues/3185
  (defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
    :override #'+org-inline-image-data-fn
    "Interpret LINK as base64-encoded image data. Ignore all errors."
    (ignore-errors
      (base64-decode-string link)))


  (after! org-noter
    (setq org-noter-default-notes-file-names '("notes.org"))
    (setq org-noter-notes-search-path '("/Users/jbonini/Dropbox_simons/org/references/misc" "/Users/jbonini/Dropbox_simons/org/"))
    (map! :mode pdf-view-mode
          :desc "insert a note"
          :n "i" ))

  (defun +org-agenda-open-in-new-workspace ()
    (interactive)
    (let* ((marker (or (org-get-at-bol 'org-marker)
                       (org-agenda-error)))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      ;; FIXME: use `org-switch-to-buffer-other-window'?
      (+workspace/new)
      (switch-to-buffer buffer)
      (widen)
      (push-mark)
      (goto-char pos)))
                                        ;(when (derived-mode-p 'org-mode)
                                        ;  (org-show-context 'agenda)
                                        ;  (recenter (/ (window-height) 2))
                                        ;  (org-back-to-heading t)
                                        ;  (let ((case-fold-search nil))
                                        ;    (when (re-search-forward org-complex-heading-regexp nil t)
                                        ;      (goto-char (match-beginning 4)))))
                                        ;(run-hooks 'org-agenda-after-show-hook)
                                        ;(and highlight (org-highlight (point-at-bol) (point-at-eol)))

  (map! :mode  org-agenda-mode
        :desc  "goto in new workspace" :g     [?\S-\t] #'+org-agenda-open-in-new-workspace
        :desc  "goto in new workspace" :g     [backtab] #'+org-agenda-open-in-new-workspace)

  (defun jrb/org-file-from-subtree (&optional name)
    "Cut the subtree currently being edited and create a new file
from it.

If called with the universal argument, prompt for new filename,
otherwise use the subtree title."
    (interactive "P")
    (org-back-to-heading)
    (let ((filename (cond
                     (current-prefix-arg
                      (expand-file-name
                       (read-file-name "New file name: ")))
                     ((not (null name)) name)
                     (t
                      (concat
                       (expand-file-name
                        (org-element-property :title
                                              (org-element-at-point))
                        default-directory)
                       ".org")))))
      (org-cut-subtree)
      (find-file-noselect filename)
      (with-temp-file filename
        (org-mode)
        (yank))))

  (defun jrb/subtree-to-journal-file ()
    (interactive)
    (org-back-to-heading)
    (jrb/org-file-from-subtree
     (expand-file-name
      (concat "./journal_org/"
              (org-element-property :title
                                    (org-element-at-point))
              ".org"))))

  (map! :map evil-org-mode-map
        :desc "copy subtree to new file" :n "gz" 'jrb/org-file-from-subtree
        :desc "copy subtree to new journal file" :n "gZ" 'jrb/subtree-to-journal-file)

  ;; so that in inspect buffer we can sort of go to the definition (at least the file)
  (map! :mode help-mode
        :desc "find-file-at-point"
        :n [C-return] 'find-file-at-point)


  ;; for inline latex
  (setq org-latex-packages-alist '(("" "braket" t) ("" "amsmath" t) ("bb=dsserif" "mathalpha" t) ("" "hyperref" t)))
  (plist-put org-format-latex-options :scale 2)
  (defun jrb/org-scale-latex (&optional arg)
    (interactive "P")
    (let ((scale  (if arg (prefix-numeric-value arg) 2)))
      (message "latex scale set to %s" scale)
      (plist-put org-format-latex-options :scale scale)))
  (defun jrb/org-latex-yas ()
    "Activate org and LaTeX yas expansion in org-mode buffers."
    (yas-minor-mode-on)
    (yas-activate-extra-mode 'latex-mode))
  (add-hook 'org-mode-hook #'jrb/org-latex-yas)

  (remove-hook! 'org-mode-hook #'+org|enable-auto-update-cookies)
  (advice-remove #'evil-org-open-below #'+org*evil-org-open-below) ; didn't like this anyway

  ;; Didn't like that new headings on C-return weren't put in at point
  (setq org-insert-heading-respect-content nil)
  (map! :map evil-org-mode-map
        :desc "New header at point"
        :ni [C-return] 'org-insert-heading)

  (add-to-list 'org-file-apps '("\\.vesta\\'" . "VESTA %s"))
  (add-to-list 'org-file-apps '("\\.nb\\'" . "mathematica %s"))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (add-to-list 'org-file-apps '("\\.odp\\'" . "libreoffice %s"))
  (setq org-export-with-sub-superscripts (quote {}))
  (setq org-image-actual-width 700)
  ;; make code look nice even before session started
  (add-to-list 'org-src-lang-modes '("ipython" . python))
  (if (modulep! :private frames-only)
      (setq org-src-window-setup 'other-frame) ;; other-window doesn't close as I'd like on exit
    (setq org-src-window-setup 'other-window)
    )

  (add-to-list 'org-file-apps '("\\.xoj\\'" . "xournal %s"))
  (setq org-refile-targets (quote ((nil :maxlevel . 3)
                                   (org-agenda-files :maxlevel . 3)
                                   ("someday.org" :maxlevel . 3)
                                   ("archive.org" :maxlevel . 3))))
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)
  (setq org-overriding-columns-format "%25ITEM %TODO %EFFORT %CLOCKSUM %JOBID %JOBCLUST %JOBDIR")
  (setq org-agenda-custom-commands '(("j" "HPC jobs" tags-todo "HPCJOB") ("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))))

  (defun esf/execute-startup-block ()
    (interactive)
    (org-babel-goto-named-src-block "startup")
    (org-babel-execute-src-block)
    (goto-char (point-min)) ;(beginning-of-buffer)
    (org-overview))

  (setq org-tag-persistent-alist '((:startgroup . nil)
                                   ("@work" . ?w) ("@personal" . ?h)
                                   (:endgroup . nil)
                                   ("reading" . ?r)
                                   ("coding" . ?c)
                                   ("investigating" . ?i)
                                   ("organizing" . ?o)
                                   ("writing/preparing" . ?p)
                                   ("calculations" . ?s)))
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "CANCELED" "DEFERRED" "DONE")))
  (setq org-capture-templates
        '(("t" "TODO" entry (file+headline "~/org/master.org" "Tasks")
           "* TODO %?\n  %i %a %U" :prepend t)
          ("l" "Log (misc)" entry (file+headline "~/org/misc-log.org" "Tasks")
           "* %?\n  %i %a %U")
          ("s" "Someday" entry (file+headline "~/org/someday.org" "Tasks")
           "* TODO %?\n  %i %a %U")
          ("a" "Appointments" entry (file+headline "~/org/master.org" "Appointments")
           "* %?\n  %i %a %U")
          ("n" "Notes" entry (file+headline "~/org/master.org" "Notes")
           "* %?\n  %i %a %U")
          ))

  ;; don't want return to execute src blocks
  (defun jrb/is-org-src-block (&optional arg)
    (interactive "P")
    (member (org-element-type (org-element-context)) (list `src-block `inline-src-block)))
  (advice-add '+org/dwim-at-point :before-until #'jrb/is-org-src-block)

  ) ; end after! org

;; key binds
(map! :leader
      (:desc "App" :prefix "A"
       :desc "Ielm" :n "i" #'ielm
       :desc "elfeed" :n "e" #'elfeed
       :desc "Processes" :n "p" #'list-processes
       :desc "Jupyter-repl" :n "j" #'jupyter-run-repl)
      :prefix "m" :desc "schedule" :n "s" #'org-schedule)

(map! :leader
      :mode process-menu-mode
      :desc "Kill process" :n "k" #'process-menu-delete-process)
(map! :mode process-menu-mode
      :desc "Quit" :n "q" (lambda () (interactive)
                            (kill-this-buffer) (evil-quit)))
(map! :desc "store(grab) link" "C-c C-g" #'org-store-link)

(defun jrb/copy-window ()
  (interactive)
  (let ((pos (point)))
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos))
  )

(map!
 :desc "open copy of current window" :m "go" 'jrb/copy-window)

(after! jupyter
  (defun jupyter-refresh-kernel-env ()
    "workaround to update cache of available
jupyter kernels after pyenv env is changed"
    (interactive)
    (jupyter-available-kernelspecs t)))

(defun jrb/goto-long-line (&optional arg)
  "Go to the first line that is at least LEN characters long.
        Use a prefix arg to provide LEN.
        no prefix uses `fill-column' as LEN."
  (interactive "P")
  (let ((len (if arg (prefix-numeric-value arg) fill-column))
        (start-line                 (line-number-at-pos))
        (len-found                  0)
        (found                      nil)
        (inhibit-field-text-motion  t))
    (while (and (not found) (not (eobp)))
      (forward-line 1)
      (setq found  (< len (setq len-found  (- (line-end-position) (point))))))
    (if found
        (message "Line %d: %d chars" (line-number-at-pos) len-found)
      (forward-line (- start-line (line-number-at-pos))) ;(goto-line start-line)
      (message "Not found"))))

(if (modulep! :private frames-only)
    (setq org-src-window-setup 'other-frame) ;; other-window doesn't close as I'd like on exit
  (setq org-src-window-setup 'other-window)
  (after!  persp-mode
    (setq persp-interactive-init-frame-behaviour-override -1
          persp-emacsclient-init-frame-behaviour-override -1))
  )

;; fixing where this was broken used to use evil-write instead of save-buffer
;; it used to be that I could just redefine here, but that seems to not work
;; renaming my functiona and putting in as advice instead
                                        ; (defun evil-org-edit-src-exit ()
(defun replace-evil-org-edit-src-exit ()
  "Save then `evil-edit-src-exit'."
  (interactive)
  (mapc #'call-interactively '(save-buffer org-edit-src-exit)))
(advice-add 'evil-org-edit-src-exit :override
            'replace-evil-org-edit-src-exit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((org-ref-pdf-directory . "/Documents/papers/pto_divacancies/")
     (org-ref-pdf-directory . "~/Documents/papers/wannier_pol/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
