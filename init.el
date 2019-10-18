;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!
;; (unless (fboundp 'cider-start-map) (defun cider-start-map ()))
(setq shell-file-name "/run/current-system/sw/bin/dash")
(setq process-connection-type nil)
(setq source-directory (concat doom-private-dir
                               "/emacs-libjit-src/src/"))
(after! find-func
  (setq find-function-C-source-directory source-directory))
(setenv "PATH" "$PATH:$HOME/.guix-profile/bin" t)
(setenv "PATH" "$PATH:$CARGO_HOME/bin/" t)
(setenv "PATH" "$PATH:$HOME/.local/bin/" t)
(setenv "AGDA_DIR" "$HOME.agda/" t)
;; So many errors :(
(defvar internal--daemon-sockname nil)
(defvar coq-keymap (make-sparse-keymap))
(defvar coq-maths-menu-enable nil)
(defvar coq-unicode-tokens-enable nil)
(defvar coq-toolbar-entries nil)
(defun warn! (&rest args)
  (message (car args)))
(setq load-path (nreverse load-path))
(setq load-prefer-newer t)
(defvar personal-keybindings nil)
(doom! :completion
       company
       ;;(helm +fuzzy)
       ;;ido
       (ivy
        ;; +fuzzy
        +prescient
        +childframe
        +icons)

       :ui
       ;; deft
       doom
       doom-dashboard
       ;; doom-quit
       ;; ophints
       fill-column
       hl-todo
       ;; hydra
       ;; indent-guides
       modeline
       nav-flash
       ;; neotree
       ;; treemacs
       (popup
        +all
        +defaults)
       ;; (pretty-code
       ;;  +iosevka)
       ;; tabbar
       ;; unicode
       vc-gutter
       ;; vi-tilde-fringe
       window-select
       workspaces

       :term
       eshell
       vterm

       :editor
       snippets
       ;; (evil
       ;;  +everywhere
       ;;  +commands)
       multiple-cursors
       file-templates
       ;; god
       fold
       (format +onsave)
       lispy
       ;; parinfer
       (objed
        +manual)
       ;; rotate-text
       ;; word-wrap

       :emacs
       (dired
        +icons)
       ;; +ranger
       electric
       vc

       :tools
       ;; debugger
       (lookup
        +docsets)
       rgb
       ;;ansible
       direnv
       docker
       eval
       editorconfig
       ;;ein
       (flycheck
        +childframe)
       (flyspell
        +aspell)
       ;;gist
       lsp
       ;;macos
       magit
       ;; make
       pass
       pdf
       prodigy
       ;; terraform
       ;; tmux
       ;; upload
       ;; wakatime

       :lang
       agda
       assembly
       (cc +lsp)
       clojure
       ;; common-lisp
       coq
       ;; crystal
       ;; csharp
       data
       ;; erlang
       ;; elixir
       ;; elm
       emacs-lisp
       ;; ess
       ;; go
       (haskell +lsp)
       ;; idris
       ;;(java +meghanada)
       ;; (javascript +lsp)
       ;; julia
       latex
       ;; ledger
       ;; lua
       markdown
       ;; nim
       nix
       ;; kotlin
       ;; fsharp
       ocaml
       (org
        ;; +ipython
        ;; +dragndrop
        +gnuplot
        +pandoc
        +present)
       ;; perl
       ;; php
       ;; plantuml
       ;; purescript
       (python +lsp)                    ; +pyenv
       ;; qt
       ;; racket
       scheme
       ;; rest
       ;; ruby
       (rust +lsp)
       ;; scala
       (sh +fish)
       ;; solidity
       ;; swift
       ;; terra
       ;; web
       ;; vala

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;; irc
       ;; (rss +org)
       ;; twitter
       (write
        +wordnut
        +langtool)
       calendar
       regex

       :email
       notmuch
       mu4e
       ;;wanderlust

       ;; :collab
       ;;floobits
       ;;impatient-mode

       :private
       (org +pomodoro)

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;; literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default
         +bindings
         +smartparens
         ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/.doom.d/abbrev.el")
 '(math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit")) t)
 '(math-units-table nil t)
 '(safe-local-variable-values
   '((eval progn
           (defvar agda2-dir-version-plist nil)
           (let
               ((nix-shell-dir
                 (locate-dominating-file default-directory "shell.nix")))
             (unless
                 (and
                  (bound-and-true-p agda2-version)
                  (equal agda2-version
                         (lax-plist-get agda2-dir-version-plist nix-shell-dir)))
               (load-file
                (let
                    ((coding-system-for-read 'utf-8))
                  (if
                      (featurep 'tramp)
                      (defun remove-sudo-tramp-prefix
                          (x)
                        "Remove sudo from path.  Argument X is path."
                        (if
                            (tramp-tramp-file-p x)
                            (let
                                ((tx
                                  (tramp-dissect-file-name x)))
                              (if
                                  (string-equal "sudo"
                                                (tramp-file-name-method tx))
                                  (tramp-file-name-localname tx)
                                x))
                          x))
                    (defsubst remove-sudo-tramp-prefix
                      (x)
                      x))
                  (shell-command-to-string
                   (concat "nix-shell " nix-shell-dir "shell.nix " "--command " "\"agda-mode locate\""))))
               (eval
                `(defun direnv-update-active-agda2-version-transient-hook
                     (&rest _)
                   (require 'agda2-mode)
                   (setq agda2-dir-version-plist
                         (lax-plist-put agda2-dir-version-plist ,nix-shell-dir agda2-version))
                   (advice-remove #'direnv--maybe-update-environment #'direnv-update-active-agda2-version-transient-hook)))
               (advice-add #'direnv--maybe-update-environment :after #'direnv-update-active-agda2-version-transient-hook))
             (when
                 (and
                  (buffer-file-name)
                  (file-name-extension
                   (buffer-file-name))
                  (equal
                   (file-name-extension
                    (buffer-file-name))
                   "agda"))
               (when
                   (featurep 'direnv)
                 (defun direnv-update-agda2-mode-transient-hook
                     (&rest _)
                   (agda2-mode)
                   (advice-remove #'direnv--maybe-update-environment #'direnv-update-agda2-mode-transient-hook))
                 (advice-add #'direnv--maybe-update-environment :after #'direnv-update-agda2-mode-transient-hook)))))))
 '(save-abbrevs 'silently))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
