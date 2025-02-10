;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
                                        ;(setq doom-font (font-spec :family "Hack Nerd Font" :size 18))

(setq doom-font (font-spec :family "Fira Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-symbol-font (font-spec :family "JuliaMono")
      doom-big-font (font-spec :family "Fira Mono" :size 22))

                                        ;(setq initial-buffer-choice "~/org/master.org")

;; disable doom splash image
(setq +doom-dashboard-functions (cdr +doom-dashboard-functions))

(progn ;;julia config
  (progn ;;julia-snail
    (setq julia-snail-extensions '(ob-julia))
    ;; setq alone only works if ran after snail loads
    (add-hook 'julia-snail-mode-hook
              (lambda ()
                ;; enable using julia version from direnv
                (inheritenv-add-advice 'julia-snail--start)
                ;; snail popup gets slow for large output, more trouble than worht
                (setq julia-snail-popup-display-eval-results nil)
                ;; do not treat repl as popup
                (set-popup-rules! '(("^\\*julia.*" :ignore t)))
                ))
    (after! org
      (add-to-list '+org-babel-mode-alist '(julia . julia-snail))))
  (progn ;; lsp-julia
    ;; I have nix make a separate, wrapped executable + sysimage
    (setq! lsp-julia-command "julia-ls")
    (setq! lsp-julia-package-dir 'nil) ; shouldn't matter (set in julia-ls script)
    )
  )
;; Disabling a bunch of older julia things I did in the past and don't understand
;; move them above above as needed
                                        ;(setq +tree-sitter-hl-enabled-modes '(not web-mode typescript-tsx-mode julia-mode))
                                        ;(set-lookup-handlers! '(julia-snail-mode)
                                        ;  :definition '(xref-find-definitions)
                                        ;  :documentation '(julia-snail-doc-lookup))
                                        ;(setq lsp-enable-xref nil)
                                        ;(setq lsp-julia-lint-missingrefs "none") ; until it becomes usable
                                        ;(defun jrb/lsp-nil-ifnotfound (&rest arg)

                                        ;  "alternate lookup-handlers are tried when it can't find symbol"
                                        ;  (if (equal (car arg) "No content at point.")
                                        ;      (error "LSP can't find it") ; returning nil apparently doesn't work, throw error instead
                                        ;    t))
                                        ;(advice-add 'lsp--info :before-while #'jrb/lsp-nil-ifnotfound)
                                        ;(add-hook 'julia-mode-hook
                                        ;          (lambda ()
                                        ;            (remove-hook 'completion-at-point-functions #'julia-mode-latexsub-completion-at-point-around t)
                                        ;            (remove-hook 'completion-at-point-functions #'julia-mode-latexsub-completion-at-point-before t)
                                        ;            (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
                                        ;            (remove-hook 'completion-at-point-functions #'julia-snail-completions-doc-capf t)
                                        ;            (remove-hook 'completion-at-point-functions #'julia-snail-repl-completion-at-point t)
                                        ;            (add-hook 'completion-at-point-functions #'julia-mode-latexsub-completion-at-point-around -30 t)
                                        ;            (add-hook 'completion-at-point-functions #'julia-mode-latexsub-completion-at-point-before -30 t)
                                        ;            (add-hook 'completion-at-point-functions #'julia-snail-completions-doc-capf -2 t)
                                        ;            (add-hook 'completion-at-point-functions #'julia-snail-repl-completion-at-point -2 t)
                                        ;            (add-hook 'completion-at-point-functions #'lsp-completion-at-point -1 t) ; I'd let it be higher than snail if it would still let snail run
                                        ;            ))

                                        ;(setq lsp-julia-package-dir "/home/john/.config/emacs/.local/straight/repos/lsp-julia/languageserver")
                                        ;       1) the default lsp-julia-package-dir is in a sense preferable so leave it (latest version w/o incompatability with project)
                                        ; but   2) you may need to go to that directory and instantiate things
                                        ; also  3) default-env below doesn't matter if in a project, to use outside: =] activate --shared default=
                                        ;       4) for some reason using dftk as a library seems to not work with the languageserver
                                        ;(after! lsp-julia
                                        ;  (setq lsp-julia-default-environment "/home/john/.julia/environments/default"))
                                        ; (after! (:and julia-repl inheritenv)
                                        ;   (inheritenv-add-advice 'julia-repl-inferior-buffer))
                                        ;
                                        ; (after! julia-repl
                                        ;   (set-popup-rules!
                                        ;     '(("^\\*julia.*" :ignore t)))
                                        ;   (julia-repl-set-terminal-backend 'vterm)
                                        ;   (when (modulep! :ui workspaces)
                                        ;                                         ;(advice-remove '+julia--namespace-repl-buffer-to-workspace-a #'julia-repl--inferior-buffer-name)
                                        ;     (defadvice! +julia--namespace-repl-buffer-to-workspace-a (&optional executable-key suffix)
                                        ;       "Name for a Julia REPL inferior buffer. Uses workspace name (or non-nil suffix) for doom emacs"
                                        ;       :override #'julia-repl--inferior-buffer-name
                                        ;       (concat julia-repl-inferior-buffer-name-base ":" (or suffix (+workspace-current-name)))))
                                        ;   )
                                        ;(after! org
                                        ; (defadvice! +ob-julia-execute-in-repl (body params)
                                        ;   :override #'org-babel-execute:julia
                                        ;   (interactive)
                                        ;   ;(let* ((session (cdr (assq :session params)))
                                        ;   ;       (julia-repl-inferior-buffer-name-suffix (pcase session
                                        ;   ;                                                 ("none" nil)
                                        ;   ;                                                 (_ session))))
                                        ;   ;       (julia-repl--send-string
                                        ;   ;        (org-babel-expand-body:julia body params)))
                                        ;   ; HACK
                                        ;   ; just setting global var when evaluating so that e.g. julia-repl-edit etc work in that session
                                        ;   ; better way would be to add babel block aware advice to those
                                        ;   (let ((session (cdr (assq :session params))))
                                        ;     (setq julia-repl-inferior-buffer-name-suffix (pcase session
                                        ;                                                    ("none" nil)
                                        ;                                                    (_ session)))
                                        ;     (julia-repl--send-string
                                        ;      (org-babel-expand-body:julia body params))))

                                        ;https://discourse.doomemacs.org/t/override-built-in-src-blocks-with-emacs-jupyter/3185/2
                                        ;(+org-babel-load-jupyter-h 'jupyter-python)

                                        ;)
                                        ; (map! :map evil-org-mode-map
                                        ;       :after julia-repl
                                        ;       :desc "sub latex to character" :ni "<A-tab>" 'julia-latexsub-or-indent
                                        ;       :desc "repl run line"  :n "gl" 'julia-repl-send-line
                                        ;       :desc "repl @edit"  :n "ge" 'julia-repl-edit
                                        ;       :desc "repl @doc"  :n "gk" 'julia-repl-doc
                                        ;       :desc "repl expand macro"  :n "gM" 'julia-repl-macroexpand
                                        ;       :desc "repl list methods" :n    "gm" 'julia-repl-list-methods)
                                        ;
                                        ; (map! :map julia-repl-mode-map
                                        ;       :after julia-repl
                                        ;       :desc "repl run line"  :n "gl" 'julia-repl-send-line
                                        ;       :desc "repl @edit"  :n "ge" 'julia-repl-edit
                                        ;       :desc "repl @doc"  :n "gk" 'julia-repl-doc
                                        ;       :desc "repl expand macro"  :n "gM" 'julia-repl-macroexpand
                                        ;       :desc "repl list methods" :n    "gm" 'julia-repl-list-methods)

;; auctex with pdf+src in different frames, adapted from https://emacs.stackexchange.com/questions/55395/auctex-and-pdf-tools-in-2-separate-frames-for-dual-monitor-setup
(defun jrb/framesMenus-display-buffer-use-some-frame (fun &rest args)
  "Use `display-buffer-use-some-frame' as `display-buffer-overriding-action'.
Then run FUN with ARGS."
  (let ((display-buffer-overriding-action '(display-buffer-use-some-frame)))
    (apply fun args)))

(defun jrb/tex-pdf-sep-frame ()
  "Run in auctex pdf window to move it to it's own frame"
  (interactive)
  (let (
                                        ;(start-frame (selected-frame))
        (start-win (selected-window)))
    (make-frame '((name . "*tex-pdf*")))
    (advice-add 'TeX-pdf-tools-sync-view :around #'jrb/framesMenus-display-buffer-use-some-frame)
    (advice-add 'pdf-sync-backward-search-mouse :around #'jrb/framesMenus-display-buffer-use-some-frame)
    (advice-add 'pdf-isearch-sync-backward :around #'jrb/framesMenus-display-buffer-use-some-frame)
                                        ;(select-frame start-frame)
    (quit-window nil start-win)
    ))

(defun jrb/tex-pdf-same-frame ()
  "Turn off auctex pdf separate frame behavior (retore default)"
  (interactive)
  (advice-remove 'pdf-isearch-sync-backward #'jrb/framesMenus-display-buffer-use-some-frame)
  (advice-remove 'TeX-pdf-tools-sync-view #'jrb/framesMenus-display-buffer-use-some-frame)
  (advice-remove 'pdf-sync-backward-search-mouse #'jrb/framesMenus-display-buffer-use-some-frame))

(after! apheleia
  (setq apheleia-formatters (map-insert apheleia-formatters 'alejandra '("alejandra")))
  (setq apheleia-mode-alist (map-insert apheleia-mode-alist 'nix-mode 'alejandra)))

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\from_materials_cloud\\'"))

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
  (setq org-roam-directory (file-truename "~/org/roam"))
  (setq org-export-with-toc nil)
  (setq org-agenda-files '("~/org/roam/20240326123755-tasks.org" "20240326123840-someday.org" "20240326123910-appointments.org" "20240326124519-log.org"))
  ;; fix jupyter output see https://github.com/nnicandro/emacs-jupyter/issues/366
  ;; https://github.com/emacs-jupyter/jupyter/issues/380
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
    (setq org-noter-notes-search-path '("/Users/jbonini/Dropbox/org/references/misc" "/Users/jbonini/Dropbox/org/"))
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
  (setq org-image-actual-width (list 700))
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
  (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                  (todo . " %i")
                                  (tags . " %i %-12:c")
                                  (search . " %i %-12:c")))

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
  (defun org-zola-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Zola post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                   "\n")))
  (setq org-roam-capture-templates
        `(
          ("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("t" "task" entry "* TODO ${title}%?\n%U\n" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n∈ [[id:678c0cf6-54fc-408c-ba0e-b4da26c8791d][tasks]]\n#+filetags: :todo:\n")
           :unnarrowed t)
          ("s" "someday" entry "* TODO [#C] ${title}%?\n%U\n" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n∈ [[id:521a6dfa-58a1-49a7-9a5e-e107f3e26562][someday]]\n#+filetags: :todo:\n")
           :unnarrowed t)
          ("e" "encrypted" plain ,(concat "%?Hit C-c C-c now if you want to use below id \n\n;; Local Variables:\n;;  epa-file-encrypt-to: (" (format "%S" jrb/secret-identity) ")\n;; End:\n")
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}")
           ;; Want the local var applied, but can only seem to do so after reloading file
           :immediate-finish t ;; doesn't seem to work? ideally would not even allow editing
           :kill-buffer t      ;; kill buffer after C-c C-c
           :jump-to-captured t ;; reload file after killed
           :unnarrowed t
           )
          ))
  ;; below would ideally apply the local vars before saving, but I couldn't get it to work
  ;; making the above encrypted thing way smoother, but it doesn't seem to work
  ;; (add-hook 'org-roam-capture-new-node-hook #'hack-local-variables)
  ;;
  ;; We also need the presence of unreadable files to not break agenda on systems without the key
  (defun jrb/can-read-gpg-file (&optional file)
    (let ((path (or file (buffer-file-name (buffer-base-buffer))))
          (epa-suppress-error-buffer t))
      (if (string-match-p "\\.gpg$" path) ;; just return true for non gpg
          (not (null (ignore-errors (kill-buffer (find-file-noselect path)))))
        t)))
  (advice-add 'org-roam-file-p :after-while 'jrb/can-read-gpg-file)

  (setq org-capture-templates ;TODO switch to org-roam-capture
        '(("t" "TODO" entry (file "~/org/roam/20240326123755-tasks.org")
           "* TODO %?\n  %i %a %U" :prepend t)
          ("l" "Log (misc)" entry (file "20240326124519-log.org")
           "* %?\n  %i %a %U")
          ("s" "Someday" entry (file "20240326123840-someday.org")
           "* TODO %?\n  %i %a %U")
          ("a" "Appointments" entry (file "20240326123910-appointments.org")
           "* %?\n  %i %a %U")
          ("n" "Notes" plain (function org-roam-capture)
           "%?" :immediate-finish t)
          ("z" "Zola post" entry (file+olp "all-posts.org" "Misc Posts")
                 (function org-zola-new-subtree-post-capture-template))
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
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))
