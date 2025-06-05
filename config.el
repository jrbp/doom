;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 20)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 22)
      doom-symbol-font (font-spec :family "Fira Code Nerd Font"))
(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))
;; disable doom splash image
(setq +doom-dashboard-functions (cdr +doom-dashboard-functions))

;; (setq +tree-sitter-hl-enabled-modes '(not web-mode typescript-tsx-mode julia-mode nix-mode))

;; https://github.com/doomemacs/doomemacs/issues/2447https://github.com/doomemacs/doomemacs/issues/2447
(after! general
  (general-evil-setup t)
  (general-nmap "c" (general-key-dispatch 'evil-change
                      "c" #'evil-change-whole-line))
  (general-vmap "c" #'evil-change))

(progn ;; janet
  ;; doom's module indent thing was giving errors removed it
  (use-package! janet-mode
    :mode "\\.\\(jdn\\|janet\\)\\'"
    :interpreter "janet[0-9]*\\'"
    :config
    (add-hook 'janet-mode-hook (lambda () (lispy-mode 1))))
  (use-package! ajrepl
    :after janet-mode
    :config (add-hook 'janet-mode-hook #'ajrepl-interaction-mode)))

(require 'notifications)
(defalias 'jrb/system-notifications-notify
  (if (string-equal system-type "android")
      #'android-notifications-notify
    #'notifications-notify))

(after! org ;; org notification stuff
  (appt-activate 1)
  (setq appt-message-warning-time 15)
  (setq appt-display-interval 5)
  (setq appt-display-format 'window)
  (defun jrb/appt-action (id key)
    (pcase key
      ("default" (org-roam-dailies-goto-today))
      ("agenda" (org-agenda "a" "a"))))
  (defun jrb/appt-reminder (min-to-app new-time appt-msg)
    "notification about incoming diary and agenda events"
    ;; TODO: keep the notification-ids to not spam so much
    (jrb/system-notifications-notify
     :title (format "Appointment in %s minutes" min-to-app)
     :body (format "%s" appt-msg)
     :actions '("default" "daily" "agenda" "agenda")
     :on-action #'jrb/appt-action))
  (setq appt-disp-window-function 'jrb/appt-reminder)
  (run-at-time t (* 15 60) #'(lambda () (org-agenda-to-appt t))))

(when (string-equal system-type "android")
  (setq overriding-text-conversion-style '())
  (setq! touch-screen-display-keyboard t)
  (setenv "PREFIX" "/data/data/com.termux/files/usr")
  (setenv "SHELL" "/data/data/com.termux/files/usr/bin/bash")
  (setq! vterm-shell "/data/data/com.termux/files/usr/bin/bash")
  (keymap-global-unset "<f3>") ;; silence switch
  ;; following for making thumb-key usable for now
  (keymap-global-set "<volume-down>" "<down>")
  (keymap-global-set "<volume-up>" "<up>")
  (keymap-global-set "°" "ESC")
  ;; (tool-bar-add-item "" DEF KEY &rest PROPS)
  ;; (menu-bar-mode y)
  ;; (tool-bar-mode y)
  ;; (modifier-bar-mode 'y)
  ;; (setq! tool-bar-position 'bottom)
  )

(progn   ;;julia config
  (setenv "JULIA_EDITOR" "emacsclient")
  (progn ;;julia-snail
    ;; TODO: default bind to `+julia/open-snail-repl'
    (setq julia-snail-extensions '(ob-julia))
    ;; setq alone only works if ran after snail loads?
    (add-hook 'julia-snail-mode-hook
              (lambda ()
                ;; enable using julia version from direnv
                (inheritenv-add-advice 'julia-snail--start)
                (setq julia-snail/ob-julia-resource-directory (file-truename "~/org/assets/ob-julia-snail"))
                ;; do not treat repl as popup
                (set-popup-rules! '(("^\\*julia.*\\*" :quit nil :ttl nil)))))
    ;; I wanted to wrap the send commands with let statements to control the
    ;; popup and have an alt prefix for the popup, but it seems something deeper
    ;; in snail prevents this, so for now I bind changing the variable:
    (map! (:localleader
           (:map (julia-snail-mode-map)
                 (:prefix ("m" . "eval with popup")
                          "n" (lambda () (interactive) (setq julia-snail-popup-display-eval-results nil))
                          "m" (lambda () (interactive) (setq julia-snail-popup-display-eval-results :command))
                          "c" (lambda () (interactive) (setq julia-snail-popup-display-eval-results :change))))))

    (after! org
      (add-to-list '+org-babel-mode-alist '(julia . julia-snail)))

    (defun jrb/julia-snail--interupt-all ()
      (interactive)
      (mapcar (lambda (reqid)
                (let* ((repl-buf (get-buffer julia-snail-repl-buffer))
                       (resp (julia-snail--send-to-server
                               '("JuliaSnail" "Tasks")
                               (format "interrupt(\"%s\")" reqid)
                               :repl-buf repl-buf
                               :async nil))
                       (res (car resp)))
                  (if res
                      (message "Interrupt scheduled for Julia reqid %s" reqid)
                    (message "Unknown reqid %s on the Julia side" reqid)
                    (remhash reqid julia-snail--requests))))
              (hash-table-keys julia-snail--requests)))

    (progn ;; towards https://ianthehenry.com/posts/my-kind-of-repl/
      ;; TODO: if eval takes time and the buffer changes above point we print to wrong place
      ;; maybe write a placeholder key, then come back and try replacing the key?
      (defun jrb/julia-snail--print-eval-result (print-pos-start buf data)
        (let* ((read-data (read data))
               (eval-data (eval read-data))
               (the-data (when (and (listp eval-data) (car eval-data))
                           (cadr eval-data)))
               (data-lines (split-string the-data "\n")))
          (with-current-buffer buf)
          (goto-char print-pos-start)
          (skip-chars-backward " \t\n")
          (let ((hspace (make-string (current-column) ?\s)))
            (insert " # -> " (car data-lines))
            (when (cdr data-lines)
              (mapcar (lambda (ln)
                        (insert "\n" hspace " #    " ln))
                      (cdr data-lines))))))

      (cl-defun jrb/julia-snail--setup-expect-test
          (block-start block-end buf data &optional (prepend-test "@test "))
        (when-let* ((read-data (read data))
                    (eval-data (eval read-data))
                    (the-data (when (and (listp eval-data) (car eval-data))
                                (read (cadr eval-data)))))
          (with-current-buffer buf)
          (goto-char block-end)
          (skip-chars-backward " \t\n")
          ;; following is robust to trailing comments and multiline regions, but also ugly
          ;; (insert "\nend |> isequal(" the-data ")")
          ;; (goto-char block-start)
          ;; (insert "@test begin \n")
          ;; Really we want to wrap only the final expression, but need better parsing
          (insert " |> ")
          (let ((endpt (+ (length prepend-test) (point))))
            (insert "isequal(" the-data ")")
            (goto-char block-start)
            (skip-chars-forward " \t\n")
            (insert prepend-test)
            (goto-char endpt))))

      (cl-defun jrb/julia-snail--send-eval-print-last-exp
          (block-start block-end
                       &key (print-pos-start block-end) (message-prefix "Evaluated and printed"))
        (let ((text (buffer-substring-no-properties block-start block-end))
              (filename (julia-snail--efn (buffer-file-name (buffer-base-buffer))))
              (module (julia-snail--module-at-point))
              (line-num (line-number-at-pos block-start))
              (buf (current-buffer))
              (comment-only current-prefix-arg)
              (julia-snail-popup-display-eval-results :command))
          (cl-flet
              ((callbackf (if comment-only
                              (lambda (data)
                                (jrb/julia-snail--print-eval-result print-pos-start buf data))
                            (lambda (data)
                              (jrb/julia-snail--setup-expect-test block-start block-end buf data)))))
            (julia-snail--flash-region block-start block-end)
            (julia-snail--send-to-server-via-tmp-file
                module
                (if comment-only text (concat "repr( begin " text "\n end)"))
              filename
              line-num
              :popup-display-params '(80 80) ;; (julia-snail--popup-params block-end)
              :callback-success (lambda (_request-info &optional data)
                                  (callbackf data)
                                  (message "%s; module %s"
                                           message-prefix
                                           (julia-snail--construct-module-path module)))))))

      (defun jrb/julia-snail-send-eval-print-line ()
        (interactive)
        (let ((block-start (line-beginning-position))
              (block-end (line-end-position)))
          (unless (eq block-start block-end)
            (jrb/julia-snail--send-eval-print-last-exp
             block-start block-end))))

      (defun jrb/julia-snail-send-eval-print-region ()
        (interactive)
        (if (null (use-region-p))
            (user-error "No region selected")
          (let ((block-start (region-beginning))
                (block-end (region-end)))
            (jrb/julia-snail--send-eval-print-last-exp
             block-start block-end))))

      (defun jrb/julia-snail-send-eval-print-dwim ()
        (interactive)
        (if (use-region-p)              ; region
            (jrb/julia-snail-send-eval-print-region)
          (condition-case _err
              (jrb/julia-snail-send-eval-print-line) ;line
            ;; (jrb/julia-snail-send-eval-print-top-level-form) ;; not implemented
            ;; (user-error (jrb/julia-snail-send-eval-print-line) ; block fails, so send line
            ;;  )
            )))
      (map! (:localleader
             (:map (julia-snail-mode-map)
                   (:prefix ("e" . "eval")
                            "L" #'jrb/julia-snail-send-eval-print-line
                            "R" #'jrb/julia-snail-send-eval-print-region
                            "E" #'jrb/julia-snail-send-eval-print-dwim))))))
  (progn ;; lsp-julia
    (setq! lsp-julia-command "julia-ls")
    (setq! lsp-julia-package-dir nil))

  (after! julia-mode
    (set-ligatures! 'julia-mode
      :def "function"
      :src_block "begin"
      :src_block_end "end")))
;; Disabling a bunch of older julia things I did in the past and don't understand
;; move them above above as needed
                                        ;(set-lookup-handlers! '(julia-snail-mode)
                                        ;  :definition '(xref-find-definitions)
                                        ;  :documentation '(julia-snail-doc-lookup))
                                        ;(setq lsp-enable-xref nil)
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

                                        ;(after! lsp-julia
                                        ; (after! (:and julia-repl inheritenv)
                                        ;   (inheritenv-add-advice 'julia-repl-inferior-buffer))
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
  (add-to-list 'apheleia-mode-alist '(nix-mode . alejandra))
  (add-to-list 'apheleia-formatters '(alejandra "alejandra")))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                    :major-modes '(nix-mode)
                    :priority 0
                    :server-id 'nixd)))
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
  '(("^\\*jupyter.*" :quit nil :ttl nil)))

(after! vterm
  (map! :mode vterm-mode
        :i "C-V" #'vterm-yank))

;; +make it so that by default ESC is sent to vterm+ disabled -> C-c C-z to toggle this
;; (add-hook! 'vterm-mode-hook #'evil-collection-vterm-toggle-send-escape)
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
  (setq org-log-done 'time)
  (progn ;; footnote stuff
    (defcustom org-footnote-define-indrawer t
      "Non-nil means define footnotes in a drawer, below reference location."
      :group 'org-footnote
      :type 'boolean
      :safe #'booleanp)

    (defun jrb/org-footnote-new-drawer-maybe ()
      "When 'org-footnote-define-indrawer insert a new footnote in
 it in a drawer on a new next line after reference point and return 't, otherwise return 'nil.
 This is meant to be used as an advice with #'org-footnote-new e.g.
 (advice-add #'org-footnote-new :before-until #'jrb/org-footnote-new-drawer-maybe)"
      (interactive)
      (unless (org-footnote--allow-reference-p)
        (user-error "Cannot insert a footnote here"))
      (when org-footnote-define-indrawer
        (let* ((all (org-footnote-all-labels))
               (label
                (unless (eq org-footnote-auto-label 'anonymous)
                  (if (eq org-footnote-auto-label 'random)
                      (format "%x" (abs (random)))
                    (org-footnote-normalize-label
                     (let ((propose (org-footnote-unique-label all)))
                       (if (eq org-footnote-auto-label t) propose
                         (completing-read
                          "Label (leave empty for anonymous): "
                          (mapcar #'list all) nil nil
                          (and (eq org-footnote-auto-label 'confirm) propose)))))))))
          (cond ((not label)
                 (insert "[fn::]")
                 (backward-char 1))
                ((member label all)
                 (insert "[fn:" label "]")
                 (message "New reference to existing note"))
                (t
                 (insert "[fn:" label "]")
                 (org-insert-drawer nil "fn")
                 (let ((labeln (org-footnote-normalize-label label))
                       electric-indent-mode) ; Prevent wrong indentation.
                   (insert "[fn:" labeln "] ")
                   (org-footnote-auto-adjust-maybe))))
          t)))
    (advice-add #'org-footnote-new :before-until #'jrb/org-footnote-new-drawer-maybe))
  (setq org-roam-directory (file-truename "~/org/roam"))
  (setq org-export-with-toc nil)
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
  (plist-put org-format-latex-options :scale 4)
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
        '(;; (type "APPT" "|" "CANCELED") ;; I don't actually understand the 'type keyword
          (sequence "TODO" "IN-PROGRESS" "WAITING" "APPT" "|" "CANCELED" "DEFERRED" "DONE")))
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
           :immediate-finish t ;; doesn't seem to work? ideally would not even allow editing. Untested, but maybe the issue is that it needs the title
           :kill-buffer t      ;; kill buffer after C-c C-c
           :jump-to-captured t ;; reload file after killed
           :unnarrowed t
           )))

  (progn ;; Ignore gpg files which we don't have the key for. Memoize result.
    (defvar jrb/gpg-file-cache (make-hash-table :test 'equal))
    (defun jrb/can-read-gpg-file (&optional file)
      (let ((path (or file (buffer-file-name (buffer-base-buffer)))))
        (if (string-match-p "\\.gpg$" path)
            (or (gethash path jrb/gpg-file-cache)
                (let ((result (jrb/attempt-read-gpg-file path)))
                  (puthash path result jrb/gpg-file-cache)
                  result))
          t)))
    (defun jrb/attempt-read-gpg-file (path)
      (let ((epa-suppress-error-buffer t)
            (filename (abbreviate-file-name (expand-file-name path)))) ;; normalize buffer name
        (not (null
              (or (get-file-buffer filename) ;; already open?
                  (find-buffer-visiting      ;; open with different buffer name?
                   filename
                   (lambda (buffer) ;; filter dead symlinks
                     (let ((file (buffer-local-value
                                  'buffer-file-name buffer)))
                       (and file (file-exists-p file)))))
                  ;; other wise try opening and closing
                  (ignore-errors (kill-buffer (find-file-noselect path))))))))
    (advice-add 'org-roam-file-p :after-while 'jrb/can-read-gpg-file))

  ;; (setq org-capture-templates ; switched to org-roam-capture
  ;;       '(("t" "TODO" entry (file "~/org/roam/20240326123755-tasks.org")
  ;;          "* TODO %?\n  %i %a %U" :prepend t)
  ;;         ("l" "Log (misc)" entry (file "20240326124519-log.org")
  ;;          "* %?\n  %i %a %U")
  ;;         ("s" "Someday" entry (file "20240326123840-someday.org")
  ;;          "* TODO %?\n  %i %a %U")
  ;;         ("a" "Appointments" entry (file "20240326123910-appointments.org")
  ;;          "* %?\n  %i %a %U")
  ;;         ("n" "Notes" plain #'org-roam-capture
  ;;          "%?" :immediate-finish t)))

  ;; don't want return to execute src blocks
  (defun jrb/is-org-src-block (&optional arg)
    (interactive "P")
    (member (org-element-type (org-element-context)) (list `src-block `inline-src-block)))
  (advice-add '+org/dwim-at-point :before-until #'jrb/is-org-src-block)) ; end after! org

;; key binds
(map! :leader
      (:desc "App" :prefix "A"
       :desc "Ielm" :n "i" #'ielm
       :desc "Cal" :n "c" #'cfw:open-org-calendar
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

(after! lispyville
  (lispyville-set-key-theme ;; so that it applies when executed interactively
   (setq lispyville-key-theme
         '((operators normal) ;; safe version of evil normal ops
           c-w                ;; safe version of delete prev word
           (prettify insert)  ;; tab will format
           text-objects ;; experimental (a)tom (l)ist se(x)p (f)unction (c)omment (S)tring
           (atom-movement t) ;; evil B/W/E(g E) movement over atoms instead of words
           commentary          ;; gc, gy followed by motion
           slurp/barf-lispy
           additional
           additional-insert))))

(map! :after (lispyville)
      :map lispyville-mode-map
      ;; add a subset of  the additional-movement theme
      :m "(" #'lispyville-backward-up-list
      :m ")" #'lispyville-up-list
      :m "{" #'lispyville-previous-opening
      :m "}" #'lispyville-next-opening)

(map! :after (lispy)
      :map lispy-mode-map-lispy
      ;; unbind individual bracket keys (annoying for writing strings containing brackets)
      "[" nil
      "]" nil
      "}" nil)

(if (modulep! :private frames-only)
    (setq org-src-window-setup 'other-frame) ;; other-window doesn't close as I'd like on exit
  (setq org-src-window-setup 'other-window)
  (after! persp-mode
    (setq persp-interactive-init-frame-behaviour-override -1
          persp-emacsclient-init-frame-behaviour-override -1)))
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
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode)
  (setq treesit-font-lock-level 6))

(after! emacs-everywhere
  (defun jrb/emacs-everywhere-write ()
    "Copy buffer content, maybe (paste,  select all), refocus emacs.
Must only be called within a emacs-everywhere buffer. "
    (interactive)
    (when emacs-everywhere-mode
      ;; won't support org to md, but fine
      ;; (run-hooks 'emacs-everywhere-final-hooks)
      ;; First ensure text is in kill-ring and system clipboard
      (let ((text (buffer-string)))
        (kill-new text)
        ;; Use macOS specific clipboard command
        (when (eq system-type 'darwin)
          (call-process "osascript" nil nil nil
                        "-e" (format "set the clipboard to %S" text)))
        ;; Also try GUI selection methods
        (gui-select-text text)
        (gui-backend-set-selection 'PRIMARY text))
      ;; Extra clipboard handling if needed
      (when emacs-everywhere-copy-command ; handle clipboard finicklyness
        (let ((inhibit-message t)
              (require-final-newline nil)
              write-file-functions)
          ;; Add this to your config to exclude tempf file from recent files
          ;; (with-eval-after-load 'recentf
          ;;   (dolist (pattern emacs-everywhere-file-patterns)
          ;;     (add-to-list 'recentf-exclude pattern)))
          (with-file-modes 384
            (write-file buffer-file-name))
          (apply #'call-process (car emacs-everywhere-copy-command)
                 nil nil nil
                 (mapcar (lambda (arg)
                           (replace-regexp-in-string "%f" buffer-file-name arg))
                         (cdr emacs-everywhere-copy-command)))))
      (sleep-for emacs-everywhere-clipboard-sleep-delay) ; prevents weird multi-second pause, lets clipboard info propagate
      (when emacs-everywhere-window-focus-command
        (let* ((the-emacseverywhere-app (funcall emacs-everywhere-app-info-function))
               (emacseverywhere-window-id (emacs-everywhere-app-id the-emacseverywhere-app))
               (window-id (emacs-everywhere-app-id emacs-everywhere-current-app)))
          (apply #'call-process (car emacs-everywhere-window-focus-command)
                 nil nil nil
                 (mapcar (lambda (arg)
                           (replace-regexp-in-string "%w" window-id arg))
                         (cdr emacs-everywhere-window-focus-command)))
          ;; The frame only has this parameter if this package initialized the temp
          ;; file its displaying. Otherwise, it was created by another program, likely
          ;; a browser with direct EDITOR support, like qutebrowser.
          (when (and (frame-parameter nil 'emacs-everywhere-app)
                     emacs-everywhere-paste-command)
            ;; Add small delay before paste
            (sleep-for emacs-everywhere-clipboard-sleep-delay)
            (apply #'call-process (car emacs-everywhere-paste-command)
                   (if (cdr emacs-everywhere-paste-command) nil
                     (make-temp-file nil nil nil "key shift+insert")) nil nil
                   (cdr emacs-everywhere-paste-command)))
          (apply #'call-process "ydotool" ;; select all
                 nil nil nil
                 '("key" "29:1" "30:1" "29:0" "30:0"))
          (apply #'call-process (car emacs-everywhere-window-focus-command)
                 nil nil nil
                 (mapcar (lambda (arg)
                           (replace-regexp-in-string "%w" emacseverywhere-window-id arg))
                         (cdr emacs-everywhere-window-focus-command)))
          (emacs-everywhere-mode 1)
          (setq emacs-everywhere--contents (buffer-string))))))
  (defun emacs-everywhere--app-info-linux-hyprland ()
    "Return information on the current active window, on a Linux Sway session."
    (let* ((activewindow (json-read-from-string
                          (emacs-everywhere--call "hyprctl" "-j" "activewindow")))
           (at (alist-get 'at activewindow))
           (size (alist-get 'size activewindow))
           (geometry (append at size nil)))
      (make-emacs-everywhere-app
       :id (alist-get 'address activewindow)
       :class (alist-get 'class activewindow)
       :title (alist-get 'title activewindow)
       :geometry geometry)))
  (defun jrb/float-on-parent ()
    (let* (
           (geometry (emacs-everywhere-app-geometry emacs-everywhere-current-app))
           ;; (geometry '(3708 2180 1273 1381))
           (ox (car geometry))
           (oy (cadr geometry))
           (ow (caddr geometry))
           (oh (cadddr geometry))
           (x ox)
           (w ow)
           (h (* oh 0.618))
           (y (- oy (- h oh))))
      (call-process "hyprctl" nil nil nil "dispatch" "movewindowpixel" "exact"
                    (int-to-string x) (format "%d,initialtitle:emacs\-everywhere" y))
      (call-process "hyprctl" nil nil nil "dispatch" "resizewindowpixel" "exact"
                    (int-to-string w) (format "%d,initialtitle:emacs\-everywhere" h))))
  (when (eq 'Hyprland (cdr emacs-everywhere--display-server))
    (add-hook 'emacs-everywhere-init-hooks #'jrb/float-on-parent)
    (setq emacs-everywhere-window-focus-command (list "hyprctl" "dispatch" "focuswindow" "address:%w"))
    (setq emacs-everywhere-app-info-function #'emacs-everywhere--app-info-linux-hyprland))
  (map! :mode emacs-everywhere-mode (:leader "r" #'jrb/emacs-everywhere-write)))
