;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
;(setq enable-local-variables t)
;(setq enable-local-eval t)
(setq doom-font (font-spec :family "Monospace" :size 18))
;
;pyvenv venv location
;(setenv "WORKON_HOME" "/home/jbonini/.cache/pypoetry/virtualenvs")
;(setq comp-deferred-compilation t)
;(setq initial-buffer-choice "~/org/master.org")

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\from_materials_cloud\\'"))

;; TODO: it would be a good idea to start all my custom functions with jrb/ or something
(defun insert-file-name (filename &optional args)
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

(map!
 :desc "copy subtree to new file" :n "gz" 'jrb/org-file-from-subtree)
(map!
 :desc "copy subtree to new journal file" :n "gZ" 'jrb/subtree-to-journal-file)

;(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
;(setq exec-path (append exec-path '("/Library/TeX/texbin")))

;; popup rules break esc key in emacs mode within popup terminals
;; kind of a shame because otherwise the popup rules would be nice
; JUST USE C-c C-z
;(after! vterm
;  (set-popup-rules!
;    '(("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"  ; editing buffers (interaction required)
;                                        ; :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
;       :ignore t)
;      ("^vterm" :ignore t)
;      )))
(set-popup-rules!
  '(("^\\*jupyter.*" :ignore t)))

; temp fix attachment bug invalid base64 data bug, see https://github.com/hlissner/doom-emacs/issues/3185
(defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
  :override #'+org-inline-image-data-fn
  "Interpret LINK as base64-encoded image data. Ignore all errors."
  (ignore-errors
    (base64-decode-string link)))

; make it so that by default ESC is sent to vterm
(add-hook! 'vterm-mode-hook #'evil-collection-vterm-toggle-send-escape)

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

  ;; macro to convert old ob-ipython blocks to emacs-jupyter blocks
  (fset 'obipy-to-jup
        (lambda (&optional arg) "Keyboard macro." (interactive "p")
          (kmacro-exec-ring-item (quote ([3 22 117 69 108 108 67 106 117 112 121 116 101 114 45 112 121 116 104 111 110 32 58 115 101 115 115 105 111 110 32 112 121 32 58 97 115 121 110 99 32 121 101 115 escape] 0 "%d")) arg)))
  ;; so that in inspect buffer we can sort of go to the definition (at least the file)
  (map! :mode help-mode
        :desc "find-file-at-point"
        :n [C-return] 'find-file-at-point)


  ;; for inline latex
  (plist-put org-format-latex-options :scale 3)
  (setq org-latex-packages-alist '(("" "braket" t)))

  ; (add-hook! 'org-mode-hook (setq-local display-line-numbers 'nil)) ; for newer version of doom i reverted from
  ; org was getting slow, disabling some things for speed up here:
  (remove-hook! 'org-mode-hook #'+org|enable-auto-update-cookies)
  (advice-remove #'evil-org-open-below #'+org*evil-org-open-below) ; didn't like this anyway
  ; realzied there were some bad json dumps in some files, cleaning up the really long lines helped a lot
  ; see goto-long-line which has been added to this config
  ;
  ; Didn't like that new headings on C-return weren't put in at point
  (setq org-insert-heading-respect-content nil)
  (map! :map evil-org-mode-map
        :desc "New header at point"
        :ni [C-return] 'org-insert-heading)

  (add-to-list 'org-file-apps '("\\.vesta\\'" . "VESTA %s"))
  (add-to-list 'org-file-apps '("\\.nb\\'" . "mathematica %s"))
  ; (add-to-list 'org-file-apps '("\\.pdf\\'" . "open %s"))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  ;(add-to-list 'org-file-apps '("\\.pptx\\'" . "open %s"))
  (add-to-list 'org-file-apps '("\\.odp\\'" . "libreoffice %s"))
  (setq org-export-with-sub-superscripts (quote {}))
  (setq org-image-actual-width 700)
  ;; make code look nice even before session started
  (add-to-list 'org-src-lang-modes '("ipython" . python))
  ;; I like when org opens links in new windows/frames
  ;no i dont ;; (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)
  (if (featurep! :private frames-only)
      (setq org-src-window-setup 'other-frame) ;; other-window doesn't close as I'd like on exit
    (setq org-src-window-setup 'other-window)
    )

  (add-to-list 'org-file-apps '("\\.xoj\\'" . "xournal %s"))
 (setq org-refile-targets (quote (("master.org" :maxlevel . 1)
                                  ("archive.org" :maxlevel . 1)
                                  (org-agenda-files :maxlevel . 1))))
 ; default agenda view is just today
 (setq org-agenda-span 'day)
 (setq org-agenda-start-day nil)
 (setq org-agenda-overriding-columns-format "%25ITEM %TODO %EFFORT %CLOCKSUM %JOBID %JOBCLUST %JOBDIR")
 (setq org-agenda-custom-commands '(("j" "HPC jobs" tags-todo "HPCJOB") ("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))))

 (defun esf/execute-startup-block ()
   (interactive)
   (org-babel-goto-named-src-block "startup")
   (org-babel-execute-src-block)
   (beginning-of-buffer)
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
          "* TODO %?\n  %i %a %U")
         ("l" "Log (misc)" entry (file+headline "~/org/misc-log.org" "Tasks")
          "* %?\n  %i %a %U")
         ("s" "Someday" entry (file+headline "~/org/someday.org" "Tasks")
          "* TODO %?\n  %i %a %U")
         ("a" "Appointments" entry (file+headline "~/org/master.org" "Appointments")
          "* %?\n  %i %a %U")
         ("n" "Notes" entry (file+headline "~/org/master.org" "Notes")
          "* %?\n  %i %a %U")
         ))



  ; don't want return to execute src blocks
  ; since function is autoloaded we override it with an advice
 (defun +org/dwim-at-point-no-src-execute (&optional arg)
   "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
   (interactive "P")
   (if (button-at (point))
       (call-interactively #'push-button)
     (let* ((context (org-element-context))
            (type (org-element-type context)))
       ;; skip over unimportant contexts
       (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
         (setq context (org-element-property :parent context)
               type (org-element-type context)))
       (pcase type
         (`headline
          (cond ((memq (bound-and-true-p org-goto-map)
                       (current-active-maps))
                 (org-goto-ret))
                ((and (fboundp 'toc-org-insert-toc)
                      (member "TOC" (org-get-tags)))
                 (toc-org-insert-toc)
                 (message "Updating table of contents"))
                ((string= "ARCHIVE" (car-safe (org-get-tags)))
                 (org-force-cycle-archived))
                ((or (org-element-property :todo-type context)
                     (org-element-property :scheduled context))
                 (org-todo
                  (if (eq (org-element-property :todo-type context) 'done)
                      (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                          'todo)
                    'done))))
          ;; Update any metadata or inline previews in this subtree
          (org-update-checkbox-count)
          (org-update-parent-todo-statistics)
          (when (and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
            (toc-org-insert-toc)
            (message "Updating table of contents"))
          (let* ((beg (if (org-before-first-heading-p)
                          (line-beginning-position)
                        (save-excursion (org-back-to-heading) (point))))
                 (end (if (org-before-first-heading-p)
                          (line-end-position)
                        (save-excursion (org-end-of-subtree) (point))))
                 (overlays (ignore-errors (overlays-in beg end)))
                 (latex-overlays
                  (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                              overlays))
                 (image-overlays
                  (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                              overlays)))
            (+org--toggle-inline-images-in-subtree beg end)
            (if (or image-overlays latex-overlays)
                (org-clear-latex-preview beg end)
              (org--latex-preview-region beg end))))

         (`clock (org-clock-update-time-maybe))

         (`footnote-reference
          (org-footnote-goto-definition (org-element-property :label context)))

         (`footnote-definition
          (org-footnote-goto-previous-reference (org-element-property :label context)))

         ((or `planning `timestamp)
          (org-follow-timestamp-link))

         ((or `table `table-row)
          (if (org-at-TBLFM-p)
              (org-table-calc-current-TBLFM)
            (ignore-errors
              (save-excursion
                (goto-char (org-element-property :contents-begin context))
                (org-call-with-arg 'org-table-recalculate (or arg t))))))

         (`table-cell
          (org-table-blank-field)
          (org-table-recalculate arg)
          (when (and (string-empty-p (string-trim (org-table-get-field)))
                     (bound-and-true-p evil-local-mode))
            (evil-change-state 'insert)))

         (`babel-call
          (org-babel-lob-execute-maybe))

         (`statistics-cookie
          (save-excursion (org-update-statistics-cookies arg)))

        ;((or `src-block `inline-src-block)
        ; (org-babel-execute-src-block arg))

         ((or `latex-fragment `latex-environment)
          (org-latex-preview arg))

         (`link
          (let* ((lineage (org-element-lineage context '(link) t))
                 (path (org-element-property :path lineage)))
            (if (or (equal (org-element-property :type lineage) "img")
                    (and path (image-type-from-file-name path)))
                (+org--toggle-inline-images-in-subtree
                 (org-element-property :begin lineage)
                 (org-element-property :end lineage))
              (org-open-at-point arg))))

         ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
          (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
            (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

         (_
          (if (or (org-in-regexp org-ts-regexp-both nil t)
                  (org-in-regexp org-tsr-regexp-both nil  t)
                  (org-in-regexp org-link-any-re nil t))
              (call-interactively #'org-open-at-point)
            (+org--toggle-inline-images-in-subtree
             (org-element-property :begin context)
             (org-element-property :end context))))))))

 (advice-add '+org/dwim-at-point :override #'+org/dwim-at-point-no-src-execute)

 )

;; key binds
(map! :leader
      (:desc "App" :prefix "a"
        :desc "Ielm" :n "i" #'ielm
        :desc "elfeed" :n "e" #'elfeed
        ;; :desc "Mail" :n "m" #'=email
        ;;:desc "Mail" :n "m" #'mu4e
        :desc "Processes" :n "p" #'list-processes
        :desc "Jupyter-repl" :n "j" #'jupyter-run-repl
        :desc "External Ranger" :n "r" #'open-ranger)
      :prefix "m" :desc "schedule" :n "s" #'org-schedule)

(map! :leader
      :mode process-menu-mode
      :desc "Kill process" :n "k" #'process-menu-delete-process)
(map! :mode process-menu-mode
      :desc "Quit" :n "q" (lambda () (interactive)
                            (kill-this-buffer) (evil-quit)))
(map! :desc "store(grab) link" "C-c C-g" #'org-store-link)

(defun copy-window ()
  (interactive)
  (let ((pos (point)))
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos))
  )

(map!
 :desc "open copy of current window" :m "go" 'copy-window)

(after! jupyter
  (defun jupyter-refresh-kernel-env ()
    "workaround to update cache of available
jupyter kernels after pyenv env is changed"
    (interactive)
    (jupyter-available-kernelspecs t)))

(defun goto-long-line (len)
  "Go to the first line that is at least LEN characters long.
Use a prefix arg to provide LEN.
Plain `C-u' (no number) uses `fill-column' as LEN."
  (interactive "P")
  (setq len  (if (consp len) fill-column (prefix-numeric-value len)))
  (let ((start-line                 (line-number-at-pos))
        (len-found                  0)
        (found                      nil)
        (inhibit-field-text-motion  t))
    (while (and (not found) (not (eobp)))
      (forward-line 1)
      (setq found  (< len (setq len-found  (- (line-end-position) (point))))))
    (if found
        (when (interactive-p)
          (message "Line %d: %d chars" (line-number-at-pos) len-found))
      (goto-line start-line)
      (message "Not found"))))


(if (featurep! :private frames-only)
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


;(defun list-processes--refresh ()
;  "Recompute the list of processes for the Process List buffer.
;Also, delete any process that is exited or signaled."
;  (setq tabulated-list-entries nil)
;  (dolist (p (process-list))
;    (cond ((memq (process-status p) '(exit signal closed))
;           (delete-process p))
;          ((or (not process-menu-query-only)
;               (process-query-on-exit-flag p))
;           (let* ((buf (process-buffer p))
;                  (type (process-type p))
;                  (pid  (if (process-id p) (format "%d" (process-id p)) "--"))
;                  (name (process-name p))
;                  (status (symbol-name (process-status p)))
;                  (buf-label (if (buffer-live-p buf)
;                                 `(,(buffer-name buf)
;                                   face link
;                                   help-echo ,(format-message
;                                               "Visit buffer `%s'"
;                                               (buffer-name buf))
;                                   follow-link t
;                                   process-buffer ,buf
;                                   action process-menu-visit-buffer)
;                               "--"))
;                  (tty (or (process-tty-name p) "--"))
;                  (cmd
;                   (if (memq type '(network serial))
;                       (let ((contact (process-contact p t)))
;                         (if (eq type 'network)
;                             (format "(%s %s)"
;                                     (if (plist-get contact :type)
;                                         "datagram"
;                                       "network")
;                                     (if (plist-get contact :server)
;                                         (format "server on %s"
;                                                 (or
;                                                  (plist-get contact :host)
;                                                  (plist-get contact :local)))
;                                       (format "connection to %s"
;                                               (plist-get contact :host))))
;                           (format "(serial port %s%s)"
;                                   (or (plist-get contact :port) "?")
;                                   (let ((speed (plist-get contact :speed)))
;                                     (if speed
;                                         (format " at %s b/s" speed)
;                                       "")))))
;                     ;; (mapconcat 'identity (process-command p) " "))))
;                     (if (not (stringp (process-command p))) ""
;                       (mapconcat 'identity (process-command p) " ")))))
;             (push (list p (vector name pid status buf-label tty cmd))
;                   tabulated-list-entries)))))
;  (tabulated-list-init-header))

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
