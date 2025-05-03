;;; private/ox-zola/packages.el
;; requires ox-hugo (enabled with org +hugo in init.el)
;;(package! ox-zola :recipe (:host github :repo "gicrisf/ox-zola"))
;; using my fork
(package! ox-zola :recipe (:host github :repo "jrbp/ox-zola") :pin "fb5afbc203125ae9a53a516be1924da2a0838543")
