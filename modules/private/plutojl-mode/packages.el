;; -*- no-byte-compile: t; -*-
;;; private/plutojl-mode/packages.el

(package! plutojl-mode
  :recipe (
           :type
           git
           :host
           github
           :repo
           "torfjelde/plutojl-mode.el"
           )
  :pin "5ff1b59f296cc9bfc5e24e3ffde4d88c3d3d6ee6"
  )
