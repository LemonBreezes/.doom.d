;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! ace-isearch)

;; For things pillaged from amosbird

(disable-packages!
 centered-window-mode
 evil-escape
 ;; dired-k
 ;; evil
 ;; stripe-buffer
 pyenv
 undo-tree
 ;; vi-tilde-fringe
 ;; visual-fill-column
 ;; window-divider-mode
 ;; persp-mode
 )
;; (package! smartparens :disable t)
;; (package! hl-line :disable t)
;; (package! evil :disable t)
(package! undo-tree :disable t)
;; (package! persp-mode :disable t)
;; (package! window-d)
(package! autothemer)
(package! fountain-mode)
(package! lorem-ipsum)
(package! cl-format)
(package! binarytrainer :recipe
  (:host github :repo "elimik31/binarytrainer" :files ("binarytrainer\.el")))
;; (package! flx-isearch)
(package! outshine)
;; (package! elisp-demos)
(package! doom-themes)
;;(package! evil :fetcher file :path "~/.config/nixpkgs/emacs-patches/evil")
;;(package! evil-snipe :fetcher file :path "~/.config/nixpkgs/emacs-patches/evil-snipe")
;; (package! org-gcal :disable t)
(package! org-bullets :disable t)
(package! anzu)
;; (package! shortcuts :recipe
;;   (:host github :repo "tetron/shortcuts.el" :files ("shortcuts.el")))
(package! goto-chg :recipe
  (:host github :repo "Benaiah/goto-chg" :files ("goto-chg\.el")))
(package! annot :recipe (:host github :repo "ghoshi/annot" :files ("src/annot\.el")))
(package! evil-owl :recipe (:host github :repo "mamapanda/evil-owl" :files ("*")))
(package! webkit-katex-render :recipe
  (:host github :repo "fuxialexander/emacs-webkit-katex-render" :files ("*")))
(package! org-clock-budget :recipe
  (:host github :repo "Fuco1/org-clock-budget" :files ("*")))
(package! arduino-mode)
(package! eaf :recipe
  (:host github :repo "MatthewZMD/emacs-application-framework"
            :files ("*")))
(package! howm)
(package! calfw-howm :recipe
  (:host github :repo "kiwanami/emacs-calfw" :files ("*")))
(package! proof-general)
(package! org-variable-pitch)
(package! general)
(package! org-pomodoro)
(package! org-yt)
(package! goto-chg)
(package! highlight-quoted)
(package! expand-region)
(package! company-shell)

(package! stumpwm-mode)
(package! zygospore :recipe
  (:host github :repo "LouisKottmann/zygospore.el" :files ("*\.el")))

;; * New unsorted
(package! el-go :recipe
  (:host github :repo "eschulte/el-go" :files ("*")))
(package! jumblr :recipe
  (:host github :repo "mkmcc/jumblr" :files ("*")))
(package! downplay-mode :recipe
  (:host github :repo "tobias/downplay-mode" :files ("downplay-mode\.el")))
;; * END
(package! faith :recipe
  (:host github :repo "emacsattic/faith" :files ("*\.el")))
(package! malyon :recipe
  (:host github :repo "speedenator/malyon" :files ("malyon\.el")))
(package! cell :recipe
  (:host gitlab :repo "dto/cell.el" :files ("*\.el")))

(package! fireplace)
(package! gnugo)
(package! 2048-game)
(package! minesweeper)
                                        ;(package! parinfer :recipe (:host github :repo "DogLooksGood/parinfer-mode" :files ("*.el")))
;; (package! smartparens)
(package! eval-in-repl :recipe
  (:host github :repo "kaz-yos/eval-in-repl" :files ("*\.el")))
;; (package! cfrs :recipe (:host github :repo "Alexander-Miller/cfrs" :files ("*.el")))
(package! leetcode)
(package! evil-fringe-mark :recipe
  (:host github :repo "Andrew-William-Smith/evil-fringe-mark" :files ("*")))
;; (package! flycheck-inline)
(package! quick-peek :recipe
  (:host github :repo "cpitclaudel/quick-peek" :files ("*\.el")))
(package! company-quickhelp)
(package! keyfreq)
(package! flycheck-haskell)
(package! nix-sandbox)
(package! ox-reveal)
(package! ox-pandoc)
(package! deadgrep)
;; (package! esh-autosuggest :recipe (:host github :repo "dieggsy/esh-autosuggest" :files ("*.el")))
;; (package! fish-completion)
;; (package! exec-path-from-shell)

;; (package! evil-anzu)
(package! company-flx)
(package! theme-changer)
;; (package! ivy-prescient)
(package! dired-hacks :recipe (:host github :repo "Fuco1/dired-hacks"))
;; (package! counsel-dash)
(package! go-playground)
(package! move-text)
(package! page-break-lines)
(package! sunrise-commander
  :recipe (:host github :repo "escherdragon/sunrise-commander" :files ("*\.el")))
(package! rust-playground)
(package! speed-type)
(package! cc-playground
  :recipe (:host github :repo "amosbird/cc-playground" :files ("*\.el" "templates")))
(package! py-playground
  :recipe (:host github :repo "amosbird/py-playground" :files ("*\.el" "templates")))
;; (package! color-moccur :recipe (:host github :repo "myuhe/color-moccur.el" :files ("*.el")))
;; (package! moccur-edit :recipe (:host github :repo "myuhe/moccur-edit.el" :files ("*.el")))
;; (package! stickyfunc-enhance :recipe (:host github :repo "tuhdo/semantic-stickyfunc-enhance" :files ("*.el")))
;; (package! arduino-mode)
(package! exwm :recipe (:host github :repo "ch11ng/exwm" :files ("*")))
(package! xcb :recipe (:host github :repo "ch11ng/xelb" :files ("*")))
(package! secretaria)
(package! tldr)
(package! landmark)
;; (package! bash-completion :recipe (:host github :repo "szermatt/emacs-bash-completion" :files ("*.el")))
;; (package! emms :recipe (:host github :repo "emacsmirror/emms" :files ("lisp/*")))
;; (package! gpastel)
;; (package! volume :recipe (:host github :repo "dbrock/volume.el" :files ("volume.el")))
;; (package! exwmsw :recipe (:host github :repo "Lemonbreezes/exwmsw" :files ("exwmsw.el")))
;; (package! sauron)
(package! lsp-haskell
  :recipe (:host github :repo "emacs-lsp/lsp-haskell" :files ("*")))
;; (package! company-pcomplete :recipe (:host github :repo "Henry/dot-emacs" :files ("my-lisp/company-pcomplete.el")))
;; (package! company-flx-mode :recipe (:host github :repo "PythonNut/company-flx" :files ("*")))
;; (package! org-noter)
;; (package! org-edit-latex)
;; (package! cdlatex)
(package! md4rd)
;; (package! writegood-mode)
(package! forecast :recipe
  (:host github :repo "cadadr/elisp" :files ("forecast\.el")))
(package! mentor)
;; (package! auto-sudoedit :recipe (:host github :repo "ncaq/auto-sudoedit" :files ("auto-sudoedit.el")))
(package! auto-capitalize :recipe
  (:host github :repo "yuutayamada/auto-capitalize-el" :files ("auto-capitalize\.el")))
(package! bbdb)
(package! simpleclip)
(package! gnus-desktop-notify)
(package! frog-jump-buffer :recipe
  (:host github :repo "waymondo/frog-jump-buffer" :files ("frog-jump-buffer\.el")))
;; (package! olivetti)

;; (package! pos-tip :recipe (:host wiki))
;;(package! isearch+ :recipe (:host wiki))
;;(package! isearch-prop :recipe (:host wiki))
;;(package! thingatpt+ :recipe (:host wiki))
;;(package! highlight :recipe (:host wiki))
;;(package! zones :recipe (:host wiki))
;; (package! lex)
;;(package! sunrise-x-modeline :recipe (:host wiki))

(package! popup-pos-tip :recipe (:files ("~/.doom.d/packages/popup-pos-tip\.el")))
;; (package! posframe)
;; (package! sunrise-commander :recipe (:host github :repo "escherdragon/sunrise-commander" :files ("*.el")))
(package! mu4e-alert)
(package! font-lock-studio)
;; (package! string-edit)
;; (package! major-mode-hydra)
(package! narrow-reindent)
(package! speed-type)
(package! page-break-lines)
(package! wiki-summary)
;; (package! gnuserv)
;; (package! undo-propose :recipe (:host github :repo "jackkamm/undo-propose-el"))
;; (package! typo :recipe (:host github :repo "jorgenschaefer/typoel" :files ("typo.el")))
(package! w3m :recipe
  (:host github :repo "emacs-w3m/emacs-w3m" :files ("*")))
(package! mu4e :recipe
  (:host github :repo "djcb/mu" :files ("mu4e/*")))
(package! exwm-mff :recipe (:host github :repo "ieure/exwm-mff"))
;;(package! use-package-chords)
;; (package! org-super-agenda)
(package! org-brain)
(package! spray)
(package! doctor)
(package! key-quiz)
;; (package! poly-org)
(package! polymode)
(package! auto-compile)
(package! edwina)
(package! undo-propose)
;; (package! esh-autosuggest :recipe (:host github :repo "dieggsy/esh-autosuggest" :files ("esh-autosuggest.el")))
(package! aweshell :recipe (:host github :repo "manateelazycat/aweshell"))
;; (package! ivy-exwm :recipe (:host github :repo "pjones/ivy-exwm"))
;; Tonic
;; (package! pinentry)
;; (package! gif-screencast)
;; (package! nov)
;; (package! shr)
;; (package! xwidget)
;; (package! image-mode)
;; (package! gnuplot)
;; (package! em-cmpl)
;; (package! academic-phrases)
;; (package! quick-peek)
;; ;; (package! esup :recipe (:host github :repo "jschaf/esup"))
;; (package! company-pcomplete)
;; (package! org-habit)
;; (package! org-super-agenda)
