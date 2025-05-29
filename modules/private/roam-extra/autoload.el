;;; private/roam-extra/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defcustom before-remove-roam-todo-tag-hook nil
  "Normal hook that is run before a roam node has the todo-tag removed
 by `roam-extra:update-todo-tag'."
  :options '()
  :type 'hook
  :group 'roam-extra)

;;;###autoload
(defun roam-extra:todo-p ()
  "Return non-nil if current buffer has any TODO entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))
;;;###autoload
(defun roam-extra:update-todo-tag ()
  "Update TODO tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (org-roam-file-p))
    (let* ((prop-tags (org-roam-node-tags (org-roam-node-at-point)))
           (tags prop-tags))
      (if (roam-extra:todo-p)
          (setq tags (seq-uniq (cons "todo" tags)))
        (setq tags (remove "todo" tags))
        (when (string= (car (seq-difference prop-tags tags)) "todo")
          (with-demoted-errors "Before-remove-roam-todo-tag hook error: %S"
	    (run-hooks 'before-remove-roam-todo-tag-hook))))
      (unless (equal prop-tags tags)
        (org-roam-set-keyword "filetags" (combine-and-quote-strings tags ":"))))))
;;;###autoload
(cl-defun roam-extra:todo-files (&key (max-daily 30))
  "Return a list of roam files containing todo tag, only include up to the `max-daily' most recent dailies."
  (org-roam-db-sync)
  (seq-uniq (mapcar 'car (append
                          (org-roam-db-query
                           [:select [nodes:file]
                            :from nodes
                            :inner :join tags :on (like tags:tag "todo") :and (= tags:node-id nodes:id)
                            :where :not nodes:file :like "%daily/%.org"])
                          (org-roam-db-query
                           [:select [nodes:file]
                            :from nodes
                            :inner :join tags :on (like tags:tag "todo") :and (= tags:node-id nodes:id)
                            :where nodes:file :like "%daily/%.org"
                            :order :by [(desc nodes:file)] :limit $s1]
                           max-daily)))))
;;;###autoload
(defun roam-extra:update-todo-files (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (roam-extra:todo-files)))
