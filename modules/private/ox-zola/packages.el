;;; private/ox-zola/packages.el
;; requires ox-hugo (enabled with org +hugo in init.el)
;;(package! ox-zola :recipe (:host github :repo "gicrisf/ox-zola"))
;; using my fork
(package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "e3365cb4e65c1853d8838b863a21546bbd9e0990")
(package! ox-zola
  :recipe (:host github :repo "jrbp/ox-zola" :includes ox-hugo)
  :pin "fb5afbc203125ae9a53a516be1924da2a0838543")
