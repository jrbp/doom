;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  ;; It is important that termuxpath is prepended, not appended.
  ;; Otherwise we will get Androids incompatible diff executable, instead of the one in Termux.
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (format "%s:%s" termuxpath
		       (getenv "PATH")))
    (push termuxpath exec-path)
    (push "~/.config/emacs/bin" exec-path)))

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       ;; (company          ; the ultimate code completion backend
       ;;   ;+childframe
       ;;   )
        ;)
       (corfu +orderless +icons)  ; complete with cap(f), cape and a flying feather!
       ;(helm             ; the *other* search engine for love and life
       ; +fuzzy)          ; enable fuzzy search backend for helm
       ;;ido               ; the other *other* search engine...
       ;;(ivy              ; a search engine for love and life
       ;; +fuzzy)          ; enable fuzzy search backend for ivy
       (vertico
        +icons)

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       ;;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       ;;;hydra
       indent-guides     ; highlighted indent columns
       (ligatures +extra)         ; ligatures and symbols to make your code pretty again
                                        ;Error in a Doom module: /data/john/git/doomemacs/modules/ui/ligatures/config.el, (file-missing Cannot open load file No such file or directory ligature)
       ;;minimap           ; show a map of the code on the side
       modeline ; replaces doom-modeline in the future         ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints ; replaced evil-goggles
       (popup            ; tame sudden yet inevitable temporary windows
        +defaults       ; default popup rules
        ;; +all             ; catch all popups that start with an asterix
        )
       ;;;pretty-code       ; replace bits of code with pretty symbols
       ;;tabs              ; an tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold ; replaced hideshow in newer doom i reverted from
       format  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       snippets          ; my elves. They type so I don't have to
       rotate-text       ; cycle region at point between text candidates
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons +dirvish)            ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ;;ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree
       undo

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell             ; a terminal REPL for Emacs
       ;;term              ; terminals in Emacs
       (vterm +vterm)             ; another terminals in Emacs

       :checkers
       (spell +aspell)
       (syntax
        ;+childframe
        )
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       biblio            ; Writes a PhD for you (citation needed)
       ;;collab            ; buffers with friends
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       ;;docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       eval              ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        ;+docsets ;;TODO add dictionary and such
        )        ; ...or in Dash docsets locally
       (lsp
        +peek)
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;;rgb               ; creating color strings
       ;;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;; tree-sitter         ;  syntax and parsing, sitting in a tree...
       ;; having a number of issues with this tree-sitter module, disabling for now.
       ;; https://github.com/marienz/nix-doom-emacs-unstraightened/issues/7
       ;; there is a workaround for the above in the readme
       ;; though even without unstraightened I didin't like a lot of what it was doing
       ;; I will reexamine when doom changes to using the builtin tree-sitter
       ;; see: https://github.com/doomemacs/doomemacs/issues/7623
       ;; Does seem to be being worked on though (it's one of the 2 things in triage for the next release)
       ;; ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       (tty +osc)               ; improve the terminal Emacs experience (clipboard and mouse support even over ssh)

       :lang
       ;;agda              ; types of types of types of types...
       ;;;assembly          ; assembly for fun or debugging
       ;;beancount         ; mind the GAAP
       (cc +lsp); C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;(fortran +lsp)           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;go                ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       ;;(haskell +intero) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       json              ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       (julia
        +lsp
        +tree-sitter
        +snail)             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex
        +fold)    ; writing papers in Emacs has never been so fun
       ;;lean
       ;;ledger            ; an accounting system in Emacs
       (lua                             ; one-based indices? one-based indices
        ;; +lsp ;; TODO install lsp
        ;; +tree-sitter
        +fennel)
       (markdown +grip)
       ;;nim               ; python + lisp at the speed of c
       (nix
        +tree-sitter
        +lsp
        )               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        ;; +brain
        ;; +contacts
        +dragndrop       ; drag & drop files/images into org buffers
        ;; +crypt ;; TODO  Do I want this, it already works?
        ;; + gnuplot
        +hugo            ; use Emacs for hugo blogging
        ;; +journal
        +jupyter        ; ipython/jupyter support for babel
        +noter
        +pandoc          ; export-with-pandoc support
        ;; passwords
        ;;+pomodoro        ; be fruitful with the tomato technique
        +present        ; Emacs for presentations
        +pretty
        +roam2
        ; are following deprecated settings?
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export)          ; Exporting org to whatever you want
       ;;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python            ; beautiful is better than ugly
         ; +conda
         ; +cpython
         +lsp
         ;+poetry
         ;+pyenv
         +pyright
         +tree-sitter
        )
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust
        +lsp
        )              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       (scheme         ; a fully conniving family of lisps
        +guile)
       ;;(sh +fish)        ; she sells (ba|z|fi)sh shells on the C xor
       sh
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.

       :email
       ;mu4e
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar         ;;TODO use / configure more
       ;;emms
       everywhere        ; *leave* Emacs!? You must be joking ;; TODO get working on hyprland
       ;;irc               ; how neckbeards socialize
       (rss +org)        ; emacs as an RSS reader
       ;;;twitter           ; twitter client https://twitter.com/vnought
       ;;;(write            ; emacs as a word processor (latex + org + markdown)
       ;; +wordnut         ; wordnet (wn) search
       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
       ;;;floobits          ; peer programming for a price
       ;;;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme, a custom yasnippet
       ;; library, and additional ex commands for evil-mode. Use it as a
       ;; reference for your own modules.
       (default +bindings +snippets +evil-commands) ;; TODO add +gnupg?, do snippets go elsewher?

       :private ;; ~/.config/doom/modules/private/{...}
       (system-specific ;Do I still use these?
        ;+mac)
        +linux-hpc)
        ;+linux-personal)
        ;+wsl)
       ;; frames-only
       ;org-dnd
       super-agenda
       ;;org-ref ;trying biblio instead?
       plutojl-mode
       gptel
       roam-extra
       secrets
       astro
       )
