;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-

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

(require 'bind-key)
(defvar browse-url-mosaic-program "mosaic")
(after! custom
  (defun custom-initialize-reset (symbol value)
    "Initialize SYMBOL based on VALUE.
Set the symbol, using its `:set' function (or `set-default' if it has none).
The value is either the symbol's current value
 \(as obtained using the `:get' function), if any,
or the value in the symbol's `saved-value' property if any,
or (last of all) VALUE."
    (funcall (or (get symbol 'custom-set) 'set-default)
	     symbol
	     (cond ((default-boundp symbol)
		    (funcall (or (get symbol 'custom-get) 'default-value)
			     symbol))
		   ((get symbol 'saved-value)
		    (eval (car (get symbol 'saved-value))))
		   (t
		    (eval value))))))

;; (defun quiet! (&rest args) args)
;; (defvar rtags-rdm-binary-name nil)
;; (defvar rtags-rc-binary-name nil)
;; (defun rtags-executable-find (&rest _))

(when (boundp 'comp-deferred-compilation)
  (setq comp-deferred-compilation t
        comp-async-jobs-number 16
        comp-speed 3
        comp-async-env-modifier-form nil))

;; Prefer a fresh .el file over an outdated .elc counterpart
(setq load-prefer-newer t)

;; Do not take my C-c p prefix
(defvar persp-keymap-prefix "P")

;; Use the packages provided by nixpkgs
(cl-loop for package in '("lispy" "annot" "flx") do
         (setq load-path
               (remove (expand-file-name (concat "straight/build/" package)
                                         doom-local-dir)
                       load-path)))

;; Void variable error
(defvar coq-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `coq-mode'.")
(defvar safe-persp-name nil)

;; Banish all typing latency!
(provide 'evil-escape)
(defun evil-escape-mode (&rest _))

(setq +workspaces-main "Main")
(setq debug-on-error t)

(advice-add #'org-heading-components :before
            (defun switch-to-org-mode-a ()
              (unless (derived-mode-p 'org-mode)
                (org-mode))))

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       company                     ; the ultimate code completion backend
       (helm                       ; the *other* search engine for love and life
        +fuzzy)
       ;;ido               ; the other *other* search engine...
       (ivy                             ; a search engine for love and life
        +prescient
        ;; +childframe
        +icons)

       :ui
       deft                             ; notational velocity for Emacs
       doom                             ; what makes DOOM look the way it does
       ;; doom-dashboard                   ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       fill-column            ; a `fill-column' indicator
       hl-todo                ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;;indent-guides     ; highlighted indent columns
       ;;modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ;; ophints           ; highlight the region an operation acts on
       (popup   ; tame sudden yet inevitable temporary windows
        ;;+all             ; catch all popups that start with an asterix
        +defaults)                    ; default popup rules
       ;; (ligatures                   ; replace bits of code with pretty symbols
       ;;  +iosevka)
       ;; tabs              ; an tab bar for Emacs
       treemacs               ; a project drawer, like neotree but cooler
       unicode                ; extended unicode support for various languages
       vc-gutter              ; vcs diff in the fringe
       vi-tilde-fringe        ; fringe tildes to mark beyond EOB
       ;;window-select          ; visually switch windows
       workspaces             ; tab emulation, persistence & separate workspaces
       ;; zen                    ; distraction-free coding or writing

       :editor
       (evil +everywhere)               ; come to the dark side, we have cookies
       file-templates                   ; auto-snippets for empty files
       fold                             ; (nigh) universal code folding
       (format +onsave)                 ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       lispy                       ; vim for lisp, for people who don't like vim
       multiple-cursors            ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text               ; cycle region at point between text candidates
       snippets                  ; my elves. They type so I don't have to
       word-wrap                 ; soft wrapping with language-aware indent

       :emacs
       (dired                           ; making dired pretty [functional]
        ;; +ranger
        ;; +icons
        )
       electric                         ; smarter, keyword-based electric-indent
       (ibuffer                         ; interactive buffer management
        +icons)
       (undo +tree)
       vc                         ; version-control and Emacs, sitting in a tree

       :term
       eshell   ; a consistent, cross-platform shell (WIP)
       shell    ; a terminal REPL for Emacs
       ;; term     ; terminals in Emacs
       ;; vterm                            ; another terminals in Emacs

       :checkers
       (syntax +childframe)          ; tasing you for every semicolon you forget
       spell                         ; tasing you for misspelling mispelling
       grammar                       ; tasing grammar mistake every you make

       :tools
       ;; ansible
       biblio
       debugger              ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)       ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup                  ; helps you navigate your code and documentation
        +docsets                ; ...or in Dash docsets locally
        +offline
        ;; +webkit
        +dictionary)
       lsp
       ;;macos             ; MacOS-specific commands
       magit      ; a git porcelain for Emacs
       make       ; run make tasks from Emacs
       pass       ; password manager for nerds
       pdf        ; pdf enhancements
       prodigy    ; FIXME managing external services & code builders
       rgb        ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       tty

       :lang                            ;TODO Add missing fstar language
       ;; agda                             ; types of types of types of types...
       (cc +lsp)                        ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data                         ; config/data formats
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp                       ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;(go +lsp)                ; the hipster dialect
       (haskell +ghcide)                ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;json              ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex                    ; writing papers in Emacs has never been so fun
        +latexmk
        +cdlatex
        +fold
        +lsp)
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown            ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       nix                 ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org                ; organize your plain life in plain text
        +pretty
        +dragndrop         ; drag & drop files/images into org buffers
        +hugo            ; use Emacs for hugo blogging
        ;;+jupyter        ; ipython/jupyter support for babel
        +noter
        +roam
        +brain
        +gnuplot
        +pandoc                         ; export-with-pandoc support
        +pomodoro                       ; be fruitful with the tomato technique
        +journal
        +present)    ; using org-mode for presentations
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +conda +cython)     ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust   ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       scheme ; a fully conniving family of lisps
       sh     ; she sells {ba,z,fish shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       web                              ; the tubes
       ;;yaml              ; JSON, but readable

       :email
       mu4e
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       irc                              ; how neckbeards socialize
       (rss +org)                       ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings +smartparens))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(emms-mode-line-cycle-additional-space-num 2)
 '(emms-mode-line-cycle-any-width-p t)
 '(emms-mode-line-cycle-current-title-function
   (lambda nil
     (substring-no-properties
      (let
          ((track
            (emms-playlist-current-selected-track)))
        (cl-case
            (emms-track-type track)
          ((streamlist)
           (let
               ((stream-name
                 (emms-stream-name
                  (emms-track-get track 'metadata))))
             (if stream-name stream-name
               (emms-track-description track))))
          ((url)
           (emms-track-description track))
          (t
           (file-name-nondirectory
            (emms-track-description track)))))
      nil -8)))
 '(emms-mode-line-cycle-max-width 30)
 '(emms-mode-line-cycle-use-icon-p nil)
 '(emms-mode-line-cycle-velocity 2)
 '(emms-mode-line-format " [%s]")
 '(emms-mode-line-titlebar-function nil)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (defun doom-byte-compile-private-config nil
             (run-at-time 2 nil
                          (lambda nil
                            (byte-compile-file
                             (expand-file-name "config.el" doom-private-dir)))))
           nil t)
     (eval and
           (fboundp 'org-make-toc-mode)
           (org-make-toc-mode 1))))
 '(wakatime-api-key "70065547-956a-4695-aa04-54ccef6bb206"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
