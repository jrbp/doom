;;; private/system-specific/config.el -*- lexical-binding: t; -*-

(cond
 ((modulep! +mac)
  ; my macbook
  (jrb-sys/set-mac))
 ((modulep! +linux-hpc)
  ; cluster environment eg rusty
  (jrb-sys/set-linux-hpc))
 ((modulep! +linux-personal)
  ; for example my thinkpad
  (jrb-sys/set-linux-personal))
 ((modulep! +wsl)
  ; windows subsystem for linux emacs
  (jrb-sys/set-wsl))
 ; TODO else case that tries to determine automaticly by calling hostname
 ; and/or gives sensible defaults
 )

(map! :leader (:desc "App" :prefix "A" (:desc "External term" :n "t" #'jrb-sys/open-ext-term)))
