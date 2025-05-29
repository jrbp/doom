;;; private/ox-zola/config.el -*- lexical-binding: t; -*-
(use-package! ox-zola
  :after (ox-hugo))

;; moved from main config even though currently not using zola
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
                 "%?\n")                ;Place the cursor here finally
               "\n")))
;; previously main config had org-capture-templates include:
          ;; ("z" "Zola post" entry (file+olp "~/git/mysite/content-org/all-posts.org" "Misc Posts")
          ;;        (function org-zola-new-subtree-post-capture-template))
