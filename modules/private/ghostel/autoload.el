;;; private/ghostel/autoload.el -*- lexical-binding: t; -*-

(defvar +ghostel--id nil)

;;;###autoload
(defun +ghostel/toggle (arg)
  "Toggles a terminal popup window at project root.

If prefix ARG is non-nil, recreate ghostel buffer in the current project's root.

Returns the ghostel buffer."
  (interactive "P")
  (+ghostel--configure-project-root-and-display
   arg
   (lambda ()
     (let ((ghostel-buffer-name
            (format "*jrb:ghostel-popup:%s*"
                    (if (bound-and-true-p persp-mode)
                        (safe-persp-name (get-current-persp))
                      "main")))
           confirm-kill-processes
           current-prefix-arg)
       (when arg
         (let ((buffer (get-buffer ghostel-buffer-name))
               (window (get-buffer-window ghostel-buffer-name)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (when (window-live-p window)
             (delete-window window))))
       (if-let* ((win (get-buffer-window ghostel-buffer-name)))
           (delete-window win)
         (let ((buffer (or (cl-loop for buf in (doom-buffers-in-mode 'ghostel-mode)
                                    if (equal (buffer-local-value '+ghostel--id buf)
                                              ghostel-buffer-name)
                                    return buf)
                           (get-buffer-create ghostel-buffer-name))))
           (with-current-buffer buffer
             (unless (eq major-mode 'ghostel-mode)
               (ghostel--start-process)
               (ghostel--apply-initial-input-mode))
             (setq-local +ghostel--id ghostel-buffer-name))
           (pop-to-buffer buffer)))
       (get-buffer ghostel-buffer-name)))))

(defun +ghostel--configure-project-root-and-display (arg display-fn)
  "Sets the environment variable PROOT and displays a terminal using `display-fn`.

If prefix ARG is non-nil, cd into `default-directory' instead of project root.

Returns the ghostel buffer."
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load ghostel"))
  (let* ((project-root (or (doom-project-root) default-directory))
         (default-directory
           (if arg
               default-directory
             project-root)))
    (setenv "PROOT" project-root)
    (funcall display-fn)))
