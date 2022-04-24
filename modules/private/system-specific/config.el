;;; private/system-specific/config.el -*- lexical-binding: t; -*-

(cond
 ((featurep! +mac)
  ; my macbook
  (jrb-sys/set-mac))
 ((featurep! +linux-hpc)
  ; cluster environment eg rusty
  (jrb-sys/set-linux-hpc))
 ((featurep! +linux-personal)
  ; for example my thinkpad
  (jrb-sys/set-linux-personal))
 ((featurep! +wsl)
  ; windows subsystem for linux emacs
  (jrb-sys/set-wsl))
 ; TODO else case that tries to determine automaticly by calling hostname
 ; and/or gives sensible defaults
 )

(map! :leader (:desc "App" :prefix "a" (:desc "External term" :n "t" #'jrb-sys/open-ext-term)))
