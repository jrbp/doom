;; -*- no-byte-compile: t; -*-
;;; private/ghostel/packages.el

(package! ghostel
  :recipe (:host github :repo "dakra/ghostel")
  :pin "0194b15ec957a6ce4be2ff49c4c485f648f86d0d")



(package! evil-ghostel
  :recipe (:host github :repo "dakra/ghostel"
           :files ("extensions/evil-ghostel/evil-ghostel.el"))
  :pin "0194b15ec957a6ce4be2ff49c4c485f648f86d0d")
