;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

(package! pdf-tools :built-in 'prefer) ; use nix installed pdf-tools
(package! treesit-auto) ; because of https://github.com/marienz/nix-doom-emacs-unstraightened/issues/7

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")

;; Disabling esc behavior on 'jk' see https://discourse.doomemacs.org/t/typing-jk-deletes-j-and-returns-to-normal-mode
(package! evil-escape :disable t)

;; janet repl mode
(package! ajrepl
  :recipe (:host github
           :repo "sogaiu/ajrepl"
           :files (:defaults ("ajrepl/" "ajrepl/*"))))

;; TODO: go back to master after merge, see https://github.com/gcv/julia-snail/issues/149
(package! julia-snail
  :recipe (:host github :repo "gcv/julia-snail" :branch "juliasyntax")
  :pin "1bfa18bc300be54efd83a8f1e3e51d725141067c")

;; TODO: go back to doom module maybe?
(package! janet-mode
  :recipe (:files ("*.el"))
  :pin "9e3254a0249d720d5fa5603f1f8c3ed0612695af")

(package! lispy
  :recipe (:host github :repo "jrbp/lispy" :branch "janet-eval")
  :pin "ff3482b404616bca46c794e39a13745d5094a17d")
;; TODO: remove if/when main repo is maintained, see https://github.com/abo-abo/lispy/issues/684
;; (package! lispy
;;   :recipe (:host github :repo "enzuru/lispy" :branch "master")
;;   :pin "b6e1d5c02c0d506a003731dfc310e330094f6749")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
;;
