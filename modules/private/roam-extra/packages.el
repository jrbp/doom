;; -*- no-byte-compile: t; -*-
;;; private/roam-extra/packages.el

(package! org-ql
  :recipe (:host github
           :repo "alphapapa/org-ql"
           :files (:defaults (:exclude "helm-org-ql.el"))))

(unpin! org-roam)
(package! org-roam-ui)
