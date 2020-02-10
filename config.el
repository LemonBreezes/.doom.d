;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq inhibit-compacting-font-caches t)

(after! font-utils
  (setq font-utils-use-memory-cache t))

(setq doom-font (font-spec :family "Iosevka" :size 20)
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 20)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 19)
      doom-serif-font (font-spec :family "IBM Plex Serif" :size 20)
      )

(setq browse-url-browser-function #'browse-url-generic
      browse-url-generic-program "qutebrowser")

(setq source-directory (expand-file-name "~/src/emacs-libjit/src")
      find-function-C-source-directory source-directory)

(when (display-graphic-p)
  (global-set-key [remap suspend-frame] #'ignore))

(after! solar
  (setq calendar-latitude 40.11060)
  (setq calendar-longitude -88.20730)
  (setq calendar-location-name "Urbana, IL"))

(after! calendar
  (setq calendar-week-start-day 1))

(load (expand-file-name "email-address" doom-private-dir))

(defun +org-is-agenda-file (filename)
  (cl-find (file-truename filename) (bound-and-true-p org-agenda-files)
           :key #'file-truename
           :test #'equal))

(after! recentf
  ;; don't clobber recentf with agenda files
  (push #'+org-is-agenda-file recentf-exclude)
  (push "~/.mail" recentf-exclude)
  (push "\\.git" recentf-exclude)
  (push "/tmp/" recentf-exclude)
  (push "/ssh:" recentf-exclude)
  (push "~/\\.emacs\\.d/.local" recentf-exclude)
  (push "~/mail" recentf-exclude)
  (push "/var" recentf-exclude)
  (push "/usr" recentf-exclude)
  (push "\\.?ido\\.last$" recentf-exclude)
  (push "^/nix/store/" recentf-exclude)
  (push ".+\\.mp3$" recentf-exclude))

(after! lpr (setq printer-name "Brother_HL-L2320D_series"))
(after! ps-print (setq ps-printer-name "Brother_HL-L2320D_series"))

(after! display-line-numbers (setq display-line-numbers-type 'relative))

(when (boundp '+lookup-provider-url-alist)
  (let ((startpage-entry
         (cons "startpage"
               "https://www.startpage.com/do/search?query=%s&?prf=7de10a290cc3cee4fa552d4b43dc3f48")))
    (setq +lookup-provider-url-alist (assoc-delete-all "google" +lookup-provider-url-alist))
    (add-to-list '+lookup-provider-url-alist startpage-entry)))

(setq shift-select-mode nil)

(require 'el-patch)
(el-patch-use-package-mode +1)

(advice-add #'force-mode-line-update :override #'ignore)

(use-package! epa-file
  :defer-incrementally t
  :config
  (epa-file-enable))

(when (and (fboundp #'jit-disassemble)
           (fboundp #'+evil-delete-region-if-mark-a))
  (advice-remove #'evil-delete-backward-char-and-join #'+evil-delete-region-if-mark-a)
  (defadvice evil-delete-backward-char-and-join (around +evil-delete-region-if-mark-a (&rest args) activate)
    (apply #'+evil-delete-region-if-mark-a args)))

(when (and (fboundp #'jit-disassemble)
           (fboundp #'+org-realign-table-maybe-a))
  (advice-add #'+org-enable-auto-reformat-tables-h
              :after
              (lambda (&rest _)
                (advice-remove #'evil-replace #'+org-realign-table-maybe-a)
                (defadvice evil-replace (after +org-realign-table-maybe-a (&rest args) activate)
                  (apply #'+org-realign-table-maybe-a args)))))

(when (fboundp #'jit-disassemble)
  (after! mu4e-conversation
    (add-hook! 'mu4e-conversation-mode-hook
      (defadvice mu4e~headers-update-handler
          (after
           mu4e-conversation--update-handler-extra (&rest args)
           activate)
        (apply #'mu4e-conversation--update-handler-extra args))

      (advice-remove #'mu4e~headers-update-handler #'mu4e-conversation--update-handler-extra))))

(after! amx
  (cl-loop for fun in '(load eval-last-sexp eval-buffer eval-region eval-expression autoload-do-load)
           do (progn (advice-remove fun #'amx-post-eval-force-update)
                     (eval `(defadvice ,fun (after amx-post-activate)
                              (amx-post-eval-force-update))))))

(when (fboundp #'jit-disassemble)
  (after! with-editor
    (advice-remove #'shell-command #'shell-command--shell-command-with-editor-mode)))

(when (fboundp #'jit-disassemble)
  (after! evil
    (advice-remove #'evil-indent #'+evil--dont-move-cursor-a)
    (defadvice evil-indent (around +evil--dont-move-cursor-a (&rest args)
                                   activate)
      (save-excursion ad-do-it))))

(when (fboundp #'jit-disassemble)
  (after! evil-snipe
    (advice-remove #'evil-snipe-s
                   #'+evil/repeat-evil-snipe-s)
    (advice-remove #'evil-snipe-S
                   #'+evil/repeat-evil-snipe-S)
    (advice-remove #'evil-snipe-f
                   #'+evil/repeat-evil-snipe-f)
    (advice-remove #'evil-snipe-F
                   #'+evil/repeat-evil-snipe-F)
    (advice-remove #'evil-snipe-t
                   #'+evil/repeat-evil-snipe-t)
    (advice-remove #'evil-snipe-T
                   #'+evil/repeat-evil-snipe-T)
    (advice-remove #'evil-snipe-x
                   #'+evil/repeat-evil-snipe-x)
    (advice-remove #'evil-snipe-X
                   #'+evil/repeat-evil-snipe-X)
    (defadvice evil-snipe-s
        (after +evil/repeat-evil-snipe-s
               (&rest args)
               activate)
      (when ad-return-value
        (apply #'+evil/repeat-evil-snipe-s args)))
    (defadvice evil-snipe-S
        (after +evil/repeat-evil-snipe-S
               (&rest args)
               activate)
      (when ad-return-value
        (apply #'+evil/repeat-evil-snipe-S args)))
    (defadvice evil-snipe-f
        (after +evil/repeat-evil-snipe-f
               (&rest args)
               activate)
      (when ad-return-value
        (apply #'+evil/repeat-evil-snipe-f args)))
    (defadvice evil-snipe-F
        (after +evil/repeat-evil-snipe-F
               (&rest args)
               activate)
      (when ad-return-value
        (apply #'+evil/repeat-evil-snipe-F args)))
    (defadvice evil-snipe-t
        (after +evil/repeat-evil-snipe-t
               (&rest args)
               activate)
      (when ad-return-value
        (apply #'+evil/repeat-evil-snipe-t args)))
    (defadvice evil-snipe-T
        (after +evil/repeat-evil-snipe-T
               (&rest args)
               activate)
      (when ad-return-value
        (apply #'+evil/repeat-evil-snipe-T args)))
    (defadvice evil-snipe-x
        (after +evil/repeat-evil-snipe-x
               (&rest args)
               activate)
      (when ad-return-value
        (apply #'+evil/repeat-evil-snipe-x args)))
    (defadvice evil-snipe-X
        (after +evil/repeat-evil-snipe-X
               (&rest args)
               activate)
      (when ad-return-value
        (apply #'+evil/repeat-evil-snipe-X args)))))

(defconst alphabet '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z))

(defconst vowels '(?a ?e ?i ?o ?u))
(defconst numbers '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
(defconst consonants (cl-set-difference alphabet vowels))
(defconst common-starting-consonant-bigrams '("th" "tr" "sh" "tw" "sy" "fl"))
(defconst brackets '(?\[ ?\] ?\{ ?\} ?\( ?\)))
(defconst misc-symbols '(?. ?, ?- ?| ?_))
(defconst whitespace-chars '(?\t ?\s ?\n))
(defconst keyboard-layout-translation-alist
  '(("c" . "e")
    ("p" . "r")
    ("z" . "t")
    ("j" . "y")
    ("l" . "u")
    ("u" . "i")
    ("y" . "o")
    ("'" . "p")
    ("r" . "s")
    ("s" . "d")
    ("t" . "f")
    ("m" . "h")
    ("n" . "j")
    ("e" . "k")
    ("i" . "l")
    ("o" . ";")
    (";" . "\\")
    ("\\" . "'")
    ("x" . "z")
    ("v" . "x")
    ("f" . "c")
    ("d" . "v")
    ("k" . "n")
    ("h" . "m")

    ("a" . "a")
    ("q" . "q")
    ("w" . "w")
    ("g" . "g")

    ("C" . "E")
    ("P" . "R")
    ("Z" . "T")
    ("J" . "Y")
    ("L" . "U")
    ("U" . "I")
    ("Y" . "O")
    ("\"" . "P")
    ("R" . "S")
    ("S" . "D")
    ("T" . "F")
    ("M" . "H")
    ("N" . "J")
    ("E" . "K")
    ("I" . "L")
    ("O" . ":")
    (":" . "|")
    ("|" . "\"")
    ("X" . "Z")
    ("V" . "X")
    ("F" . "C")
    ("D" . "V")
    ("K" . "N")
    ("H" . "M")))
(defconst keyboard-layout-prefix-keys '("g" "z" "gz"))
(defconst symbol-bigrams '("t;" ":"
                           "t`" "~"
                           "t7" "&"
                           "t8" "*"
                           "t9" "("
                           "t0" ")"
                           ;; "t\\" "|"
                           ;; "t/" "?"
                           ;; "t." ">"
                           ;; "t," "<"
                           ;; "t'" "\""
                           ;; "t]" "}"
                           ;; "t[" "{"
                           "n`" "~"
                           ":;" "::"
                           ;; "n2" "@"
                           "n1" "!"
                           ;; "n3" "#"
                           "n4" "$"
                           "n5" "%"
                           ;;"n6" "^"
                           "n8" "*"
                           "n=" "+"
                           ",=" "<="
                           ".=" ">="
                           "=." "=>"
                           ;; "n-" "_"
                           "-." ("->" "→")
                           ",-" ("<-" "↽")
                           "`." ("~>" "⤳")
                           ",`" ("<~" "⬿")
                           "~." ("~>" "⤳")
                           ",." "|"
                           "↽." "↔"
                           ".-" "∸"
                           "∸." "÷"
                           ))
(defconst symbol-trigrams '(
                            "bnn" "ℕ"
                            "bnb" "𝔹"
                            "bnr" "ℝ"
                            "bnv" "𝕍"
                            "bna" "𝔸"
                            "bnc" "ℂ"
                            "bnd" "𝔻"
                            "bne" "𝔼"
                            "bnf" "𝔽"
                            "bnw" "𝕎"
                            "bnq" "ℚ"
                            "bno" "𝕆"
                            "bnj" "𝕁"
                            "bnz" "ℤ"
                            "bny" "𝕐"
                            "bnt" "𝕋"
                            "bnl" "𝕃"
                            "bng" "𝔾"
                            "bnx" "𝕏"
                            "bni" "𝕀"
                            "bnm" "𝕄"
                            "bnk" "𝕂"
                            "bnh" "ℍ"
                            "bnp" "ℙ"
                            "bns" "𝕊"
                            "bnu" "𝕌"
                            ))

(defconst kbl-translation-alist
  (mapcar (lambda (l)
            (setq l (cons (cdr l) (car l))))
          keyboard-layout-translation-alist))

(defconst kbl-reverse-translation-alist
  (mapcar (lambda (l)
            (setq l (cons (cdr l) (car l))))
          kbl-translation-alist))

(defconst kbl-admissible-prefixes
  (mapcar (lambda (x) (string-to-char (car x)))
          keyboard-layout-translation-alist))

(defun kbl-print (s &optional control-p meta-p shift-p super-p)
  "The modifiers are in alphabetical order: Control -> Meta -> Shift -> Super"
  (declare (pure t) (side-effect-free t))
  (concat (and control-p "C-")
          (and meta-p "M-")
          (and shift-p "S-")
          (and super-p "s-")
          (alist-get s kbl-translation-alist s nil #'equal)))

(defun kbl-print-reverse (s &optional control-p meta-p shift-p super-p)
  "The modifiers are in alphabetical order: Control -> Meta -> Shift -> Super"
  (declare (pure t) (side-effect-free t))
  (concat (and control-p "C-")
          (and meta-p "M-")
          (and shift-p "S-")
          (and super-p "s-")
          (alist-get s kbl-reverse-translation-alist s nil #'equal)))

(defun kbl-kbd (s &optional control-p meta-p shift-p super-p)
  (kbd (kbl-print s control-p meta-p shift-p super-p)))

(defun make-conditional-key-translation (key-from key-to translate-keys-p)
  "Make a Key Translation such that if the translate-keys-p function returns true,
   key-from translates to key-to, else key-from translates to itself.  translate-keys-p
   takes key-from as an argument. "
  (define-key key-translation-map key-from
    (lambda (prompt)
      (if (funcall translate-keys-p key-from) key-to key-from))))

(defvar evil-colemak-xvcf-enabled t)

(defun toggle-evil-colemak-xvcf ()
  (interactive)
  (if evil-colemak-xvcf-enabled
      (setq evil-colemak-xvcf-enabled nil)
    (setq evil-colemak-xvcf-enabled t)))

(global-set-key (kbd "M-s-SPC") #'toggle-evil-colemak-xvcf)

(defun my-translate-keys-p (key-from)
  (declare (side-effect-free t))
  "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
  (and evil-colemak-xvcf-enabled
       ;; Only allow a non identity translation if we're beginning a Key Sequence.
       ;; (equal key-from (this-command-keys))
       (not isearch-mode)
       (or (memq (aref (this-command-keys) 0) kbl-admissible-prefixes)
           (eq (aref (this-command-keys) 0) 'easymotion)
           (equal key-from (this-command-keys)))
       (and (or (evil-motion-state-p)
                (evil-normal-state-p)
                (evil-visual-state-p)
                (evil-operator-state-p))
            (not (or (bound-and-true-p avy--overlays-back)
                     (bound-and-true-p avy--overlays-lead)
                     (string-prefix-p "evil-snipe-" (symbol-name this-command)))))))

(cl-loop for p in keyboard-layout-translation-alist
         do (make-conditional-key-translation (kbd (car p)) (kbd (cdr p)) #'my-translate-keys-p))

(make-conditional-key-translation (kbl-kbd "v" 'control) (kbd "C-v") #'my-translate-keys-p)
(make-conditional-key-translation (kbd "C-v") (kbl-kbd "v" 'control) #'my-translate-keys-p)

(defun correct-symbol-ngram ()
  (when (not (equal major-mode 'org-mode))
    (let* ((l 0)
           (s (or (and (> (point) 2)
                       ;; Character before is not a letter or bigram has a number
                       (or (not (memq (char-before (- (point) 2))
                                      (cons ?\' alphabet)))
                           (memq (char-before) numbers)
                           (memq (char-before) brackets)
                           (memq (char-before) misc-symbols))
                       (let ((output (lax-plist-get symbol-bigrams (buffer-substring-no-properties
                                                                    (max (point-min) (- (point) 2))
                                                                    (point)))))
                         (when output
                           (setq l 2)
                           output)))
                  (and (> (point) 2)
                       (or (not (memq (char-before (- (point) 3))
                                      (cons ?\' alphabet)))
                           (memq (char-before) numbers)
                           (memq (char-before) brackets)
                           (memq (char-before) misc-symbols)
                           (memq (char-before (- (point) 2)) whitespace-chars))
                       (let ((output  (lax-plist-get symbol-trigrams (buffer-substring-no-properties
                                                                      (max (point-min) (- (point) 3))
                                                                      (point)))))
                         (when output
                           (setq l 3)
                           output))
                       ))))
      (when s
        (when (or (and (s-matches? "~" (or (and (stringp s) s)
                                           (car s)))
                       (eq (char-after) ?`))
                  (and (s-matches? "{" (or (and (stringp s) s)
                                           (car s)))
                       (eq (char-after) ?\])))
          (delete-char 1))
        (when (listp s)
          (if (memq major-mode '(text-mode
                                 agda2-mode
                                 org-mode))
              (setq s (second s))
            (setq s (first s))))
        (delete-char (- l))
        (setq unread-input-method-events (string-to-list s))))))

(add-hook 'post-self-insert-hook #'correct-symbol-ngram)

(defun swap-semicolon-colon ()
  (when (or (and (memq major-mode
                       '(agda2-mode
                         haskell-mode
                         ))
                 (eq (length (this-command-keys-vector))
                     1)))
    (cond ((eq (aref (this-command-keys-vector)
                     0)
               ?\;)
           (setq last-command-event
                 ?:))
          ((eq (aref (this-command-keys-vector)
                     0)
               ?:)
           (setq last-command-event
                 ?\;)))))

(add-hook 'pre-command-hook #'swap-semicolon-colon)

(defun _I_a_->_I_am_ ()
  (when (and (>= (point) (length " I a "))
             (member (buffer-substring-no-properties (max (point-min) (- (point) (length " I a ")))
                                                     (point))
                     '(" I a " "\nI a " "\tI a ")))
    (save-excursion (forward-char -1)
                    (insert-char ?m))))

(add-hook 'post-self-insert-hook #'_I_a_->_I_am_)

(global-set-key (kbd "s-SPC") #'doom/leader)

(use-package! perfect-margin
  :custom
  (perfect-margin-visible-width 128)
  :config
  ;; enable perfect-mode
  (perfect-margin-mode t)

  ;; add additinal bding on margin area
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll)))

  ;; Perfect margin regexps
  (setq perfect-margin-ignore-regexps
        '("^minibuf" "^ "))

  ;; Adjust margins for popup windows
  (add-hook 'doom-switch-window-hook #'perfect-margin-margin-windows)
  (add-hook 'eshell-mode-hook #'perfect-margin-margin-windows)
  (if (fboundp #'jit-disassemble)
      (defadvice +eshell/toggle
          (after +eshell/toggle-advice (&rest args) activate)
        (perfect-margin-margin-windows))
    (advice-add #'org-insert-structure-template
                :after
                (lambda (&rest _) (perfect-margin-margin-windows))))
  (if (fboundp #'jit-disassemble)
      (defadvice +popup/toggle
          (after +popup/toggle-advice (&rest args) activate)
        (perfect-margin-margin-windows))
    (advice-add #'+popup/toggle
                :after
                (lambda (&rest _) (perfect-margin-margin-windows))))

  ;; Do not ignore certain modes
  (defvar perfect-margin-unignored-modes nil)
  (setq perfect-margin-unignored-modes '(mu4e-main-mode))

  (el-patch-defun perfect-margin--auto-margin-ignore-p (win)
    "Conditions for filtering window (WIN) to setup margin."
    (let* ((buffer (window-buffer win))
           (name (buffer-name buffer)))
      (el-patch-swap
        (or (with-current-buffer buffer
              (apply #'derived-mode-p perfect-margin-ignore-modes))
            (cl-some #'identity
                     (nconc (mapcar (lambda (regexp) (string-match-p regexp name)) perfect-margin-ignore-regexps)
                            (mapcar (lambda (func) (funcall func win)) perfect-margin-ignore-filters))))
        (and (not (member (buffer-local-value 'major-mode buffer)
                          perfect-margin-unignored-modes))
             (or (with-current-buffer buffer
                   (apply #'derived-mode-p perfect-margin-ignore-modes))
                 (cl-some #'identity
                          (nconc (mapcar (lambda (regexp) (string-match-p regexp name)) perfect-margin-ignore-regexps)
                                 (mapcar (lambda (func) (funcall func win)) perfect-margin-ignore-filters))))))))
  )

(use-package! hippie-exp
  :config
  (defun my/he-try-expand-flx-regexp (str)
    "Generate regexp for flexible matching of str."
    (concat (rx word-boundary)
            (mapconcat (lambda (x)
                         (concat (rx (zero-or-more word) (zero-or-more "-"))
                                 (list x)))
                       str
                       "")
            (rx (zero-or-more word) word-boundary)))

  (defun my/he-try-expand-flx-collect (str)
    "Find and collect all words that flex-match str, and sort by flx score"
    (let ((coll)
          (regexp (my/he-try-expand-flx-regexp str)))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp regexp nil t)
          (push (thing-at-point 'symbol) coll)))
      (sort coll #'(lambda (a b)
                     (> (car (flx-score a str))
                        (car (flx-score b str)))))))

  (defun my/he-try-expand-flx (old)
    "Try to complete word using flx matching."
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (unless (he-string-member he-search-string he-tried-table)
        (push he-search-string he-tried-table))
      (setq he-expand-list
            (unless (equal he-search-string "")
              (my/he-try-expand-flx-collect he-search-string))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (pop he-expand-list))
    (prog1
        (null he-expand-list)
      (if (null he-expand-list)
          (when old (he-reset-string))
        (he-substitute-string (pop he-expand-list)))))

  (setq hippie-expand-try-functions-list
        '(yas-hippie-try-expand
          try-expand-dabbrev
          try-expand-dabbrev-from-kill
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          ;; my/he-try-expand-flx
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          )))

;; (global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "C-c C-/") #'hippie-expand)

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

(global-set-key (kbd "C-M-v")
                'my-View-scroll-half-page-forward-other-window)
(global-set-key (kbd "C-M-S-v")
                'my-View-scroll-half-page-backward-other-window)

(when (featurep 'evil)
  (global-set-key [remap evil-scroll-down] #'View-scroll-half-page-forward)
  (global-set-key [remap evil-scroll-up] #'View-scroll-half-page-backward))

(defun my-View-scroll-half-page-forward-other-window ()
  (interactive)
  (with-selected-window (next-window)
    (call-interactively 'View-scroll-half-page-forward)))

(defun my-View-scroll-half-page-backward-other-window ()
  (interactive)
  (with-selected-window (next-window)
    (call-interactively 'View-scroll-half-page-backward)))

(setq scroll-preserve-screen-position 'always)

(advice-add #'View-scroll-half-page-forward :around
            #'my-indicate-scroll-forward)

(advice-add #'View-scroll-half-page-backward :around
            #'my-indicate-scroll-backward)

(defun my-indicate-scroll-get-line (pos)
  (save-excursion
    (goto-char pos)
    (string-to-number (format-mode-line "%l"))))

(defun my-indicate-scroll (linep f args)
  (let ((linen (my-indicate-scroll-get-line linep))
        (pulse-delay 0.1))
    (set-transient-map
     `(keymap ,@(if (and (featurep 'evil)
                         (not (evil-emacs-state-p)))
                    (list (cons (string-to-char (if evil-colemak-xvcf-enabled (kbl-print-reverse "v") "d")) #'View-scroll-half-page-forward)
                          (cons (string-to-char (if evil-colemak-xvcf-enabled (kbl-print-reverse "u") "u")) #'View-scroll-half-page-backward))
                  (cons ?v real-this-command))))
    (save-excursion
      (goto-line linen)
      (pulse-momentary-highlight-one-line (point) 'highlight))
    (sit-for 0.1)
    (apply f args)))

(defun my-indicate-scroll-forward (f &rest args)
  (my-indicate-scroll (1- (window-end)) f args))

(defun my-indicate-scroll-backward (f &rest args)
  (my-indicate-scroll (window-start) f args))

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
(global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
(global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

(global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

(global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)
(setq switch-window-background nil
      switch-window-qwerty-shortcuts
      '("a" "r" "s" "t" "n" "e" "i" "o" "g" "m" "q" "w" "c" "p" "z" "j" "l" "u" "'" "x" "v" "f" "d" "b" "k" "h")
      switch-window-shortcut-style 'qwerty
      switch-window-shortcut-appearance 'text
      switch-window-default-window-size 0.6)

(defun indented-copy-for-reddit ()
  "Copy and indent active region or current defun."
  (interactive)
  (when-let* ((bounds (if (region-active-p)
                          (cons (region-beginning) (region-end))
                        (bounds-of-thing-at-point 'defun)))
              (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (setq deactivate-mark t)
    (kill-new (replace-regexp-in-string "^" "    " text))
    (message "Copied!")))

(defun crux-kill-line-backwards ()
  "Kill line backwards and adjust the indentation."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(if (display-graphic-p)
    (global-set-key (kbd "<C-backspace>") #'crux-kill-line-backwards)
  (global-set-key (kbd "C-DEL") #'crux-kill-line-backwards))

(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)

(defvar message-filter-regexp-list '("^Starting new Ispell process \\[.+\\] \\.\\.\\.$"
                                     "^Ispell process killed$")
  "filter formatted message string to remove noisy messages")

(defadvice message (around message-filter-by-regexp activate)
  (if (not (ad-get-arg 0))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (if (and (stringp formatted-string)
               (some (lambda (re) (string-match re formatted-string)) message-filter-regexp-list))
          (save-excursion
            (set-buffer "*Messages*")
            (goto-char (point-max))
            (insert formatted-string "\n"))
        (progn
          (ad-set-args 0 `("%s" ,formatted-string))
          ad-do-it)))))

;; Searched 1/1 files
(add-to-list 'message-filter-regexp-list "^Searched [0-9]/[0-9] files$")

;; Note: file is write-protected
(add-to-list 'message-filter-regexp-list "^Note: file is write protected$")

;; auto-async-byte-compile -file name- completed with warnings.
(add-to-list 'message-filter-regexp-list
             "^auto-async-byte-compile .+completed with warnings.$")

;; End of buffer.
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

;; turn off auto revert messages
(setq auto-revert-verbose nil)

(use-package! eaf
  :defer-incrementally t
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)

  (defvar buffer-url nil)
  (defun eaf-start-process-in-nix-shell ()
    (interactive)
    (if (process-live-p eaf-process)
        (message "EAF process has started.")
      (setq eaf-process
            (apply 'start-process
                   eaf-name
                   eaf-name
                   "nix-shell"
                   "-p"
                   "qutebrowser"
                   "python3Packages.pip"
                   "python3Packages.xlib"
                   "python3Packages.pyqt5_with_qtwebkit"
                   "python3Packages.pyqrcode"
                   "python3Packages.dbus-python"
                   "python3Packages.pydbus"
                   "python3Packages.pyqtwebengine"
                   "python3Packages.pymediainfo"
                   "python3Packages.poppler-qt5"
                   "python3Packages.pymupdf"
                   "python3Packages.feedparser"
                   "python3Packages.grip"
                   "grip"
                   "qt5Full"
                   "--run"
                   (eaf-format-nix-shell-args eaf-python-command
                                              (list eaf-python-file)
                                              (eaf-get-render-size)
                                              (list eaf-proxy-host eaf-proxy-port eaf-proxy-type (concat user-emacs-directory "eaf"))
                                              (list (string-join (cl-loop for (key . value) in eaf-var-list
                                                                          collect (format "%sᛝ%s" key value)) "ᛡ")))))
      (set-process-query-on-exit-flag eaf-process nil)
      (set-process-sentinel
       eaf-process
       (lambda (process event)
         (message (format "%s %s" process event))
         ))
      (message "EAF process starting...")))

  (advice-add #'eaf-start-process :override #'eaf-start-process-in-nix-shell)

  (evil-set-initial-state 'eaf-mode 'emacs)

  (defun eaf-format-nix-shell-args (python-command &rest string-list-list)
    (list (concat python-command
                  " "
                  (mapconcat (lambda (x)
                               (if (string-blank-p x)
                                   "\"\""
                                 x))
                             (-flatten string-list-list)
                             " ")))))

(when (display-graphic-p)
  (use-package! exwm-mff
    :hook (exwm-init . exwm-mff-mode)
    :config
    (defvar exwm-mff-focused-window-before-warp nil)
    (defvar exwm-mff-disabled-p nil)

    (defun exwm-mff-hook-advice (oldfun &rest args)
      (if (eq exwm-mff-focused-window-before-warp (get-buffer-window))
          (progn (setq exwm-mff-focused-window-before-warp (get-buffer-window))
                 (apply oldfun args))
        (setq exwm-mff-focused-window-before-warp (get-buffer-window))))

    (defun exwm-mff-warp-to-advice (oldfun window)
      (if (eq (buffer-local-value 'major-mode (window-buffer window)) 'exwm-mode)
          (funcall oldfun window)
        (set-mouse-position exwm-workspace--current 0 0)))

    (advice-add #'exwm-mff-hook :around #'exwm-mff-hook-advice)
    (advice-add #'exwm-mff-warp-to :around #'exwm-mff-warp-to-advice)

    (add-hook 'doom-switch-window-hook #'exwm-mff-hook)
    (add-hook 'doom-switch-frame-hook #'exwm-mff-hook)))

(require 'exwm-workspace)
(require 'exwm-xim)
(require 'exwm)
(require 'exwm-systemtray)
(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist
      '(2 "HDMI-0" 1 "DP-5" 0 "DP-3")
      exwm-workspace-number 1)
(exwm-xim-enable)
(exwm-randr-enable)
(exwm-systemtray-enable)
(add-hook! 'exwm-init-hook
  (call-interactively #'+doom-dashboard/open))

(add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

;; let emacs handle these keys
(dolist (k '(XF86AudioLowerVolume
             XF86AudioRaiseVolume
             XF86AudioPlay
             XF86AudioStop
             XF86AudioMute
             XF86AudioPrev
             XF86AudioNext
             ?\C-\S-f
             ?\C-\S-p
             ?\C-\S-n
             ?\C-\S-b
             ?\C-\S-l
             ?\C-\S-u
             ?\s-l
             ?\s-u
             ?\M-\S-1
             ?\M-1
             ?\M-2
             ?\M-3
             ?\M-4
             ?\M-5
             ?\M-6
             ?\M-7
             ?\M-8
             ?\M-9
             ?\M-0))
  (push k exwm-input-prefix-keys))

(call-process-shell-command
 (string-join
  '("nvidia-settings -a '[gpu:0]/gpupowermizermode=1'
-a '[gpu:0]/gpufancontrolstate=1'
-a '[fan:0]/gputargetfanspeed=100'
-a '[fan:1]/gputargetfanspeed=100'
-a '[gpu:0]/gpumemorytransferrateoffset[4]=700'
-a '[gpu:0]/gpugraphicsclockoffset[4]=70' & ")
  " ")
 nil 0)

;; Start caldav adapter for etesync
;; (call-process-shell-command
;;  (string-join '("docker" "run" "--name" "etesync-dav" "-d" "-v"
;;                 "etesync-dav:/data" "-p" "37358:37358"
;;                 "-p" "37359:37359" "--restart=always" "etesync/etesync-dav")
;;               " ")
;;  nil "*etesync-dav*")

(defun discord-start ()
  (interactive)
  (defvar discord-process nil)
  (setq discord-process
        (async-start-process "Discord" "Discord" nil))
  (require 'elcord)
  (elcord-mode 1))

(defun steam-start ()
  (interactive)
  (defvar steam-process nil)
  (setq steam-process
        (async-start-process "steam" "steam" nil)))

(defun discord-stop ()
  (interactive)
  (if (and (boundp 'discord-process)
           (processp discord-process))
      (progn (kill-process discord-process)
             (elcord-mode -1))
    (message "Discord is not running")))

(defun steam-stop ()
  (interactive)
  (if (and (boundp 'steam-process)
           (processp steam-process))
      (kill-process steam-process)
    (message "Steam is not running")))

(exwm-input-set-key
 (kbd "s-q")
 (lambda ()
   (interactive)
   (call-process-shell-command
    (concat "taskset 0x6 " browse-url-generic-program)
    nil 0)))

(exwm-input-set-key
 (kbd "s-Q")
 (lambda ()
   (interactive)
   (call-process-shell-command "taskset 0x6 firefox" nil 0)))

(exwm-input-set-key (kbd "M-;") #'eval-expression)

(add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(after! exwm
  (setq exwm-manage-force-tiling t))

(after! exwm
  (exwm-input-set-key
   [XF86MonBrightnessUp]
   (lambda ()
     (interactive)
     (call-process-shell-command "/run/current-system/sw/bin/light -A 10" nil 0)))

  (exwm-input-set-key
   [XF86MonBrightnessDown]
   (lambda ()
     (interactive)
     (call-process-shell-command "/run/current-system/sw/bin/light -U 10" nil 0)))

  (exwm-input-set-key
   (kbd "C-s-SPC")
   (lambda ()
     (interactive)
     (call-process-shell-command "urxvt" nil 0)
     (run-at-time 0.1 nil (lambda ()
                            (call-process-shell-command "xdotool click 1" nil 0)))))


  (exwm-input-set-key
   (kbd "s-L")
   (lambda ()
     (interactive)
     (call-process-shell-command "sudo slock" nil 0)))


  (exwm-input-set-key
   (kbd "s-l")
   (lambda ()
     (interactive)
     (call-process-shell-command "xtrlock-pam -b none" nil 0)))
  )

(require 'show-eol)
(require 'feebleline)
(require 's)

(defun jcs-current-major-mode ()
  "Get current major mode."
  major-mode)

(defun jcs--feebleline--symbol-read-only ()
  "Feebleline read-only symbol."
  (if buffer-read-only
      "R" ""))

(defun jcs--feebleline--project-name ()
  "Feebleline project name."
  (let ((project-root (cdr (project-current))))
    (if (and project-root
             (buffer-file-name))
        (concat " - " (file-name-nondirectory (directory-file-name project-root)))
      "")))

(defun jcs--feebleline--coding-system ()
  "Feebleline coding system."
  buffer-file-coding-system)

(defun jcs--feebleline--time ()
  "Feebleline time."
  (format-time-string "[%Y-%m-%d %H:%M:%S]"))

(defun oof-feebleline-systray-padding ()
  (make-string (max (- (* 3 (length (or (bound-and-true-p exwm-systemtray--list)
                                        ())))
                       2)
                    0)
               ?\s))

(defun oof-objed-modeline-string ()
  (when (featurep 'objed)
    (propertize
     (format " %s(%s) "
             (symbol-name objed--object)
             (char-to-string
              (aref
               (symbol-name objed--obj-state)
               0)))
     'face 'objed-mode-line)))

(defun oof-pdf-position ()
  (and (eq major-mode 'pdf-view-mode)
       (ignore-errors (pdf-view-current-page))
       (concat " P" (number-to-string (ignore-errors (pdf-view-current-page)))
               ;; Avoid errors during redisplay.
               "/"
               (or (ignore-errors
                     (number-to-string (pdf-cache-number-of-pages)))
                   "???"))))

(defun feebleline-buffer-position ()
  (or (oof-pdf-position)
      (format "%5s:%-2s" (feebleline-line-number)
              (feebleline-column-number))))

(defun oof-feebleline-rsync-status ()
  (bound-and-true-p dired-rsync-modeline-status))

(defun oof-emms-feebleline ()
  (bound-and-true-p emms-mode-line-string))

(defun oof-emms-playing-time ()
  (when (and (bound-and-true-p emms-playing-time-string)
             (not (string-empty-p emms-playing-time-string)))
    (s-trim emms-playing-time-string)))

(defun oof-mu4e-alert-unread-emails ()
  (when (and (bound-and-true-p mu4e-alert-mode-line)
             (not (string-empty-p mu4e-alert-mode-line)))
    (s-trim mu4e-alert-mode-line)))

(setq feebleline-msg-functions
      '((feebleline-buffer-position)
        (oof-objed-modeline-string :face objed-mode-line)
        (feebleline-file-directory :face feebleline-dir-face :post "")
        (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
        (feebleline-file-modified-star :face font-lock-warning-face :post "")
        (feebleline-git-branch :face feebleline-git-face :pre " - ")
        (oof-mu4e-alert-unread-emails :align right)
        (oof-emms-feebleline :align right)
        (oof-emms-playing-time :pre "[" :post "] " :align right)
        (jcs--feebleline--time :align right)
        ;; my things
        (jcs--feebleline--coding-system :pre "[" :post "] " :align right)
        (oof-feebleline-systray-padding
         :align right)
        (oof-feebleline-rsync-status)))

(feebleline-mode +1)

(use-package! anzu
  :defer-incrementally t
  :commands (anzu-query-replace
             anzu-query-replace-regexp)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         (:map isearch-mode-map
           ([remap isearch-query-replace] . anzu-isearch-query-replace)
           ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))))

(use-package! aggressive-indent
  :after-call after-find-file
  :config
  (global-aggressive-indent-mode +1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'eshell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'org-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'emacs-lisp-mode)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode 'nix-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)
;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)

(let ((show-paren-off-screen--ov nil)) ; keep track of the overlay
  (defun show-paren--off-screen+ (&rest _args)
    "Display matching line for off-screen paren."
    (when (overlayp show-paren-off-screen--ov)
      (delete-overlay show-paren-off-screen--ov))
    ;; check if it's appropriate to show match info,
    ;; see `blink-paren-post-self-insert-function'
    (when (and (overlay-buffer show-paren--overlay)
               (not (or cursor-in-echo-area
                        executing-kbd-macro
                        noninteractive
                        (minibufferp)
                        this-command))
               (and (not (bobp))
                    (memq (char-syntax (char-before)) '(?\) ?\$)))
               (= 1 (logand 1 (- (point)
                                 (save-excursion
                                   (forward-char -1)
                                   (skip-syntax-backward "/\\")
                                   (point))))))
      ;; rebind `minibuffer-message' called by
      ;; `blink-matching-open' to handle the overlay display
      (cl-letf (((symbol-function #'minibuffer-message)
                 (lambda (msg &rest args)
                   (let ((msg (apply #'format-message msg args)))
                     (setq show-paren-off-screen--ov (display-line-overlay+
                                                      (window-start) msg ))))))
        (blink-matching-open))))
  (defadvice show-paren-function (after show-paren--off-screen+ (&rest _) activate)
    (defvar show-paren-off-screen--ov nil)
    (show-paren--off-screen+)))

(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:inherit default :inherit highlight)))
    ol))

(setq show-paren-style 'paren
      show-paren-delay 0.03
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren nil
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)

(use-package! super-save
  :after-call after-find-file
  :config
  (add-to-list 'super-save-triggers #'ace-window)
  (setq super-save-triggers nil
        super-save-auto-save-when-idle t
        super-save-exclude '("config.org"))
  (super-save-mode +1))

(use-package! page-break-lines
  :defer nil
  :config
  (global-page-break-lines-mode 1))

(setq search-whitespace-regexp ".*?")
(setq isearch-lax-whitespace t)

(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-delete+)

(defun isearch-delete+ ()
  "Delete the failed portion or last char if succesful search.

See also:

  https://emacs.stackexchange.com/a/10360/9198"
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring
           isearch-string 0 (or (isearch-fail-pos) (1- (length isearch-string))))
          isearch-message
          (mapconcat 'isearch-text-char-description isearch-string ""))
    (funcall (or isearch-message-function #'isearch-message) nil t)
    (if isearch-other-end (goto-char isearch-other-end))
    (isearch-search)
    (isearch-push-state)
    (isearch-update)))


(define-key isearch-mode-map (kbd "C-w")
  'isearch-kill-region+)

(defun isearch-kill-region+ ()
  "Kill text until match or pull text into search string.

If search string is empty forward to `isearch-yank-word-or-char'.
Otherwise exit search and kill text from where search was started
until the current match."
  (interactive)
  (if (or (string= "" isearch-string)
          (eq last-command this-command))
      (isearch-yank-word-or-char)
    (isearch-exit)
    (goto-char isearch-other-end)
    (kill-region
     isearch-opoint (point))))

(after! avy
  (setq avy-all-windows t
        avy-timeout-seconds 0.3
        avy-single-candidate-jump t
        avy-keys '(?q ?a ?r ?s ?t ?i
                      ?e ?n ?g ?m
                      ?w ?c ?p
                      ?' ?y ?u ?l
                      ?z ?j ?x ?v
                      ?f ?d ?h ?k
                      ?b ?, ?.
                      ?/ ?0 ?1 ?2 ?3
                      ?4 ?5 ?6 ?7 ?8
                      ?9 ?\; ?= ?-
                      ?\\
                      ?\[ ?\] ?\`
                      ?A ?R ?S ?T
                      ?I ?E ?N ?G ?M
                      ?Q ?W ?C ?P ?\"
                      ?O ?U ?L ?Z ?J
                      ?X ?V ?F ?D ?H
                      ?K ?B ?: ?<
                      ?> ??
                      ?@)))

(after! info
  (map! :map Info-mode-map
        "o" #'link-hint-open-link)
  (evil-set-initial-state 'Info-mode 'emacs))

(after! ace-window
  (setq aw-keys '(97 114 115 116 105 101 110 103 109 113 119 99
                     112 39 121 117 108 122 106 120 118 102 100 104 107 98 44 46 47
                     48 49 50 51 52 53 54 55 56 57 59 61 45 92 91 93 96 65 82 83 84
                     73 69 78 71 77 81 87 67 80 34 79 85 76 90 74 88 86 70 68 72 75
                     66 58 60 62 63 64)
        aw-dispatch-always nil
        aw-background t
        aw-overlays-back nil)
  (add-to-list 'aw-ignored-buffers "*Agda information*"))

(use-package! eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode)
  :config
  (setq eldoc-idle-delay 0.7)
  (when (bound-and-true-p exwm--connection)
    (add-to-list 'eldoc-box-frame-parameters '(parent-frame nil))
    (eldoc-box-hover-at-point-mode -1)
    (eldoc-box-hover-at-point-mode +1)))

(when (and (featurep! :editor lispy)
           (featurep! :editor evil))
  (after! lispyville
    (lispyville-set-key-theme
     '(operators
       c-w
       commentary
       prettify))
    (setq lispyville-motions-put-into-special nil)
    (map! :map lispyville-mode-map
          :nmvie "<S-right>" #'lispyville-forward-atom-end
          :nmvie "<S-left>" #'lispyville-backward-atom-begin)))

(when (featurep! :editor lispy)
  (customize-set-variable 'lispy-key-theme '(special lispy))
  (after! lispy
    (setq lispy-eval-display-style 'overlay
          lispy-no-permanent-semantic t)))

(after! password-cache
  (setq password-cache-expiry nil))
(after! mml2015
  (setq mml-secure-passphrase-cache-expiry most-positive-fixnum))
(after! auth-source
  (setq auth-source-cache-expiry nil))

(use-package! pinentry
  :defer nil
  :config
  (defun pinentry-emacs (desc prompt ok error)
    (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
      str))
  (pinentry-start))

(use-package! abbrev
  :hook ((prog-mode . abbrev-mode)
         (text-mode . abbrev-mode))
  :custom
  (abbrev-file-name (expand-file-name "abbrev_defs" doom-private-dir))
  (save-abbrevs 'silently)
  :config
  (defun save-abbrevs-or-buffer (arg)
    (interactive "p")
    (if (eq major-mode 'edit-abbrevs-mode)
        (progn (abbrev-edit-save-buffer)
               (unless arg (bury-buffer)))
      (save-buffer)))

  (defun abbrev-unignore-case-advice (oldfun &rest args)
    (let ((result (apply oldfun args)))
      (when (string-equal (car result) (cadr result))
        result)))

  (advice-add #'abbrev--before-point :around #'abbrev-unignore-case-advice)

  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

  (map! (:leader :prefix "t"
          :desc "save file" :nmv "r" #'save-abbrevs-or-buffer))
  (map! :leader :prefix "r"
        :desc "edit abbrevs" :nmv "a" #'edit-abbrevs)

  (abbrev-table-put global-abbrev-table :case-fixed t)
  (after! nix-mode
    (abbrev-table-put nix-mode-abbrev-table :case-fixed t)))

(add-hook 'minibuffer-setup-hook
          (lambda (&rest _)
            (when (eq this-command 'eval-expression)
              (abbrev-mode))))

(autoload 'arduino-mode "arduino-mode" "Major mode for editing Arduino code." t)
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))

(defun jens/lispify-eldoc-message (eldoc-msg)
  "Change the format of eldoc messages for functions to `(fn args)'."
  (if (and eldoc-msg
           (member major-mode sp-lisp-modes))
      (let* ((parts (s-split ": " eldoc-msg))
             (sym (car parts))
             (args (cadr parts)))
        (cond
         ((string= args "()") (format "(%s)" sym))
         (t (format "(%s %s)" sym (substring args 1 (- (length args) 1))))))
    eldoc-msg))

(advice-add #' elisp-get-fnsym-args-string :filter-return #'jens/lispify-eldoc-message)

(after! alert
  (setq alert-default-style 'libnotify))

(use-package! texfrag
  :defer-incrementally t
  :config
  (texfrag-global-mode +1)
  (setq texfrag-header-default
        (string-join '("\\documentclass{article}"
                       "\\usepackage{amsmath,amsfonts,amssymb}"
                       "\\usepackage[utf8]{inputenc}"
                       "\\usepackage[T1]{fontenc}"
                       "\\usepackage{mathrsfs}"
                       "\\usepackage{mathtools}"
                       ;; "\\usepackage{tikz,tikz-cd}"
                       "\\usepackage[all]{xy}")
                     "\n"))
  (defun texfrag-clearout-buffer ()
    (interactive)
    (texfrag-clearout-region (point-min) (point-max)))
  (map! :map texfrag-mode-map
        "C-c p d" #'texfrag-document
        "C-c p c" #'texfrag-clearout-buffer
        "C-c p l" #'texfrag-show-log
        "C-c p p" #'preview-at-point))

(add-hook 'eww-mode-hook #'visual-line-mode)

(after! shr
  (setq shr-external-browser #'browse-url-generic))

(after! eww
  (set-popup-rule! "^\\*eww bookmarks\\*$" :size 0.3 :side 'bottom :quit t :focus t)
  (set-popup-rule! "^\\*eww\\*$" :ignore t))

(use-package! helm-eww
  :bind (:map doom-leader-map
          ("ab" . helm-eww))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "helm-eww") . (nil . "Helm EWW"))))
  :config
  (el-patch-defun helm-eww-new-buffer (&optional url)
    "Fetch URL and render the page in a new buffer.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `eww-search-prefix'."
    (let ((b (generate-new-buffer "*eww*"))
          (url-at-point (thing-at-point-url-at-point)))
      (el-patch-swap (save-window-excursion
                       (with-current-buffer b
                         (eww-mode)
                         (eww (or (and url (not (string= "" url)) url)
                                  url-at-point
                                  ""))))
                     (with-current-buffer b
                       (eww-mode)
                       (eww (or (and url (not (string= "" url)) url)
                                url-at-point
                                ""))))
      b)))

(after! eww
  (map! :map eww-mode-map
        :n "q" #'bury-buffer
        :n "P" #'eww-reload))

(after! eww
  (setq eww-bookmarks-directory
        (expand-file-name ".local/eww-bookmarks" doom-private-dir)
        eww-download-directory
        (expand-file-name "~/downloads")))

(after! eww
  (add-hook! 'eww-mode-hook
    (text-scale-set 2)))

(after! eww
  (require 'cl-lib)

  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let ((mode (eww-buffer-auto-detect-mode)))
        (when mode
          (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil))

  (defun eww-buffer-auto-detect-mode ()
    (let* ((map '((ada ada-mode)
                  (awk awk-mode)
                  (c c-mode)
                  (cpp c++-mode)
                  (clojure clojure-mode lisp-mode)
                  (csharp csharp-mode java-mode)
                  (css css-mode)
                  (dart dart-mode)
                  (delphi delphi-mode)
                  (emacslisp emacs-lisp-mode)
                  (erlang erlang-mode)
                  (fortran fortran-mode)
                  (fsharp fsharp-mode)
                  (go go-mode)
                  (groovy groovy-mode)
                  (haskell haskell-mode)
                  (html html-mode)
                  (java java-mode)
                  (javascript javascript-mode)
                  (json json-mode javascript-mode)
                  (latex latex-mode)
                  (lisp lisp-mode)
                  (lua lua-mode)
                  (matlab matlab-mode octave-mode)
                  (objc objc-mode c-mode)
                  (perl perl-mode)
                  (php php-mode)
                  (prolog prolog-mode)
                  (python python-mode)
                  (r r-mode)
                  (ruby ruby-mode)
                  (rust rust-mode)
                  (scala scala-mode)
                  (shell shell-script-mode)
                  (smalltalk smalltalk-mode)
                  (sql sql-mode)
                  (swift swift-mode)
                  (visualbasic visual-basic-mode)
                  (xml sgml-mode)))
           (language (language-detection-string
                      (buffer-substring-no-properties (point-min) (point-max))))
           (modes (cdr (assoc language map)))
           (mode (cl-loop for mode in modes
                          when (fboundp mode)
                          return mode)))
      (message (format "%s" language))
      (when (fboundp mode)
        mode)))

  (setq shr-external-rendering-functions
        '((pre . eww-tag-pre))))

(use-package! keyfreq
  :defer nil
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package! auto-capitalize
  :defer t
  :commands (auto-capitalize-mode
             turn-on-auto-capitalize-mode))

(use-package! deadgrep
  :defer-incrementally t
  :commands (deadgrep)
  :bind ((:map doom-leader-map
           ("f /" . deadgrep)))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "deadgrep") . (nil . "Deadgrep")))))

(use-package! sunrise-commander
  :defer-incrementally t
  :commands (sunrise)
  :bind (:map doom-leader-map
          ("o s" . sunrise))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "sunrise") . (nil . "Sunrise Commander"))))
  :config
  (setq sr-show-file-attributes t
        sr-cursor-follows-mouse nil
        sr-show-hidden-files t)
  (define-key sr-mode-map [mouse-1] nil)
  (define-key sr-mode-map [mouse-movement] nil))

(defvar path-check-font-lock-keywords
  '(("\\(/[[:alpha:]][--/_~[:alnum:]]+\\)"
     1 (if (file-exists-p (match-string 1))
           'diff-refine-added
         'diff-refine-removed)
     prepend)))

(define-minor-mode path-check-mode
  "check if paths in file exists"
  nil nil nil
  (if path-check-mode
      (font-lock-add-keywords nil path-check-font-lock-keywords)
    (font-lock-remove-keywords nil path-check-font-lock-keywords))
  (font-lock-flush))

(use-package! font-lock-studio
  :commands font-lock-studio)

(when (featurep! :checkers spell)
  (after! ispell
    (setq ispell-quietly nil
          ispell-dictionary "en_US"
          ispell-complete-word-dict "~/.doom.d/dict/english-words.txt"
          ispell-personal-dictionary "~/.doom.d/.aspell.en.pws"))
  (after! flyspell
    (setq flyspell-issue-message-flag t
          flyspell-abbrev-p t)))

(after! ispell
  (defun endless/org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("[^\\]$" "[^\\]$"))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
    (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
    (add-to-list 'ispell-skip-region-alist '("[^\\]\\\\begin\{\\([^\\s$\\s-]+\\)\}" . "[^\\]\\\\end\{\\([^\\s$\\s-]+\\)\}")))
  (add-hook 'org-mode-hook #'endless/org-ispell))

(after! ispell
  (define-key ctl-x-map "\C-i"
    #'endless/ispell-word-then-abbrev)
  ;; I should disable this keybinding from anot some other way
  (after! annot
    (define-key ctl-x-map "\C-i"
      #'endless/ispell-word-then-abbrev))

  (defun endless/simple-get-word ()
    (car-safe (save-excursion (ispell-get-word nil))))

  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (endless/simple-get-word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil              ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word)
          (backward-char))
        (setq aft (endless/simple-get-word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point")))))

(use-package! annot
  :commands (annot-edit/add annot-remove annot-load-annotations)
  :load-path "moose/vendor"
  :defer-incrementally t
  :bind (:map doom-leader-map
          ("ia" . annot-edit/add)
          ("ix" . annot-remove)
          ("iA" . annot-add-image))
  :init
  (add-hook 'text-mode-hook '(lambda ()
                               (annot-load-annotations)))
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "annot-edit/add") . (nil . "Add annotation")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "annot-remove") . (nil . "Remove annotation")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "annot-add-image") . (nil . "Add image annotation"))))
  :config
  (defun annot-run-at-end-of-line-advice (oldfun &rest args)
    (save-excursion
      (end-of-line)
      (apply oldfun args)))
  (defun annot-remove-from-current-line-advice (oldfun &rest args)
    (save-mark-and-excursion
      (beginning-of-line)
      (push-mark)
      (end-of-line)
      (apply oldfun args)))
  (cl-loop for fn in '(annot-edit/add annot-add-image)
           do (advice-add fn :around #'annot-run-at-end-of-line-advice))
  (advice-add #'annot-remove :around #'annot-remove-from-current-line-advice)
  (setq annot-enable-fuf-support t))

(use-package! lorem-ipsum
  :defer-incrementally t
  :bind (:map doom-leader-map
          ("ill" . lorem-ipsum-insert-list)
          ("ilp" . lorem-ipsum-insert-paragraphs)
          ("ils" . lorem-ipsum-insert-sentences))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '(("SPC i l" . nil) . (nil . "lorem ipsum")))
    (add-to-list 'which-key-replacement-alist
                 '(("SPC i l l" . nil) . (nil . "insert list")))
    (add-to-list 'which-key-replacement-alist
                 '(("SPC i l p" . nil) . (nil . "insert paragraph")))
    (add-to-list 'which-key-replacement-alist
                 '(("SPC i l s" . nil) . (nil . "insert sentence"))))
  :config
  (setq lorem-ipsum-paragraph-separator "\n\n"
        lorem-ipsum-sentence-separator " "))

(defun unpackaged/lorem-ipsum-overlay (&optional remove-p)
  "Overlay all text in current buffer with \"lorem ipsum\" text.
When REMOVE-P (interactively, with prefix), remove
overlays. Useful for taking screenshots without revealing buffer
contents."
  (interactive "P")
  (dolist (ov (overlays-in (point-min) (point-max)))
    ;; Clear existing overlays created by this function.
    (when (overlay-get ov :lorem-ipsum-overlay)
      (delete-overlay ov)))
  (unless remove-p
    (require 'lorem-ipsum)
    (let ((lorem-ipsum-words
           (cl-loop for paragraph in lorem-ipsum-text
                    append (cl-loop for sentence in paragraph
                                    append (split-string sentence (rx (or space punct))
                                                         'omit-nulls))))
          (case-fold-search nil))
      (cl-labels ((overlay-match ()
                                 (let* ((beg (match-beginning 0))
                                        (end (match-end 0))
                                        (replacement-word (lorem-word (match-string 0)))
                                        (ov (make-overlay beg end)))
                                   (when replacement-word
                                     (overlay-put ov :lorem-ipsum-overlay t)
                                     (overlay-put ov 'display replacement-word))))
                  (lorem-word (word)
                              (let* ((length (length word)))
                                (cl-loop for liw in lorem-ipsum-words
                                         when (= length (length liw))
                                         collect liw into matches
                                         finally return
                                         (when matches
                                           (apply-case word (downcase (seq-random-elt matches)))))))
                  (apply-case (source target)
                              (cl-loop for sc across-ref source
                                       for tc across-ref target
                                       when (not (string-match-p (rx lower) (char-to-string sc)))
                                       do (setf tc (string-to-char (upcase (char-to-string tc)))))
                              target))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (rx (1+ alpha)) nil t)
            (overlay-match)))))))

(map! :leader
      :desc "convert region" "ilr" #'unpackaged/lorem-ipsum-overlay)

(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("\\`SPC a l\\'" . nil) . (nil . "lookup"))))

(use-package! wordnut
  :defer-incrementally t
  :bind (:map doom-leader-map
          ("alw" . wordnut-search))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "wordnut-search") . (nil . "Wordnut search"))))
  :config
  (map! :map wordnut-mode-map
        :nmv "q" #'quit-window))

(use-package! synosaurus
  :defer-incrementally t
  :bind (:map doom-leader-map
          ("alt" . synosaurus-lookup))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "synosaurus-lookup") . (nil . "Thesaurus"))))
  :config
  (map! :map synosaurus-list-mode-map
        :nmv "q" #'quit-window))

(use-package! wiki-summary
  :defer-incrementally t
  :commands (wiki-summary wiki-summary-insert)
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "wiki-summary") . (nil . "Wikipedia lookup"))))
  :bind (:map doom-leader-map
          ("alW" . wiki-summary)))

(use-package! enwc
  :config
  ;; Customize settings
  (setq enwc-default-backend 'nm
        enwc-display-mode-line nil
        enwc-wireless-device "wlo1"
        enwc-wired-device "lo"
        enwc-ask-to-save-interfaces nil
        enwc-warn-if-already-setup nil
        enwc-enable-auto-scan-on-startup t)
  ;; Ensure enwc buffer is delegated to the popup system.
  (defadvice! +popup--enwc-pop-to-buffer ()
    "Use `pop-to-buffer' instead of `switch-to-buffer' to open buffer.'"
    :before #'enwc
    (pop-to-buffer "*ENWC*"))
  ;; Customize popup buffer
  (set-popup-rule! "*ENWC*" :size 0.3 :side 'bottom :select t :autosave t))

(use-package! helm-systemd
  :bind (:map doom-leader-map
          ("a9" . helm-systemd))
  :config
  (require 'hi-lock)
  (require 'helm-bookmark)
  (setq helm-systemd-list-all t
        helm-systemd-list-not-loaded t)
  (defun my-helm-systemd-display (unit-command unit &optional isuser nodisplay)
    (with-current-buffer (get-buffer-create "Helm systemd log")
      (helm-systemd-status-mode)
      (let ((command
             (helm-systemd-systemctl-command (if isuser "--user") unit-command "--" unit)))
        (insert "\n🔜 " command "\n")
        (if (or isuser (string= unit-command "status"))
            (insert  (shell-command-to-string command))
          (with-temp-buffer
            (cd "/sudo::/")
            (setq command (shell-command-to-string (concat "sudo " command))))
          (insert command)
          )
        (insert "\n")
        (end-of-buffer))
      ;;    (propertise-sysd-buffer )
      (unless nodisplay
        (display-buffer (current-buffer)))))

  (advice-add #'helm-systemd-display :override #'my-helm-systemd-display))

(use-package! helm-sys
  :bind (:map doom-leader-map
          ("a8" . helm-top)))

(use-package! emms
  :bind (:map doom-leader-map
          ("a2" . emms))
  :commands (emms
             emms-play-dired)
  :defer-incrementally t
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "emms") . (nil . "Music"))))
  :config
  (require 'emms-setup)
  (emms-all)
  (defun ambrevar/emms-track-description-with-album (track)
    "Simple function to give a user-readable description of a track.
  If it's a file track, just return the file name.  Otherwise,
  return the type and the name with a colon in between.
  Hex-encoded characters in URLs are replaced by the decoded
  character."
    (let ((type (emms-track-type track)))
      (cond ((eq 'file type)
             (cl-flet ((fmt (string &optional suffix prefix)
                            (if string
                                (concat prefix string suffix)
                              "")))
               (concat
                (fmt (emms-track-get track 'info-artist) " - ")
                (fmt (emms-track-get track 'info-album) " - ")
                (fmt (emms-track-get track 'info-discnumber) "/")
                (if (emms-track-get track 'info-tracknumber)
                    (format "%02d. " (string-to-number (emms-track-get track 'info-tracknumber)))
                  "")
                (emms-track-get track 'info-title)
                (fmt (ambrevar/emms-time-for-display track) "]" " ["))))
            ((eq 'url type)
             (emms-format-url-track-name (emms-track-name track)))
            (t (concat (symbol-name type)
                       ": " (emms-track-name track))))))
  
  (defun ambrevar/emms-time-for-display (track)
    "Inspired by `emms-playing-time-display'."
    (let* ((total-playing-time
            (or (emms-track-get
                 track
                 'info-playing-time)
                0))
           (total-min-only (/ total-playing-time 60))
           (total-sec-only (% total-playing-time 60)))
      (format "%02d:%02d" total-min-only total-sec-only)))
  
  (defun ambrevar/emms-play-on-add (old-pos)
    "Play tracks when calling `emms-browser-add-tracks' if nothing
  is currently playing."
    (interactive)
    (when (or (not emms-player-playing-p)
              emms-player-paused-p
              emms-player-stopped-p)
      (with-current-emms-playlist
        (goto-char old-pos)
        ;; if we're sitting on a group name, move forward
        (unless (emms-playlist-track-at (point))
          (emms-playlist-next))
        (emms-playlist-select (point)))
      (emms-stop)
      (emms-start)))
  
  (defun track-description (track)
    "Return a description of the current TRACK."
    (if (and (emms-track-get track 'info-artist)
             (emms-track-get track 'info-title))
        (let ((pmin (emms-track-get track 'info-playing-time-min))
              (psec (emms-track-get track 'info-playing-time-sec))
              (ptot (emms-track-get track 'info-playing-time))
              (art  (emms-track-get track 'info-artist))
              (tit  (emms-track-get track 'info-title))
              (alb  (emms-track-get track 'info-album)))
          (cond ((and pmin psec) (format "%s - %s - %s" art alb tit))
                (ptot (format  "%s - %s - %s" art alb tit ))
                (t (emms-track-simple-description track))))
      (emms-track-simple-description track)))
  
  (defun ambrevar/emms-browser-track-artist-and-title-format (bdata fmt)
    (concat
     "%i"
     (let ((disc (emms-browser-format-elem fmt "D")))
       (if (and disc (not (string= disc "")))
           "%D/"))
     (let ((track (emms-browser-format-elem fmt "T")))
       (if (and track (not (string= track "0")))
           "%T. "
         ""))
     "%n"))
  (setq emms-directory "~/.doom.d/emms")
  (setq emms-browser-info-title-format 'ambrevar/emms-browser-track-artist-and-title-format)
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
  
  (setq later-do-interval 0.01
        later-do-batch 1)
  
  (setq emms-source-file-directory-tree-function #'emms-source-file-directory-tree-find)
  (setq emms-source-file-default-directory "~/hdd/unindexed-music")
  (setq emms-player-mpd-music-directory "~/hdd/music")
  
  (setq emms-playlist-buffer-name "Music-EMMS")
  
  (when (executable-find "emms-print-metadata")
    (require 'emms-info-libtag)
    (add-to-list 'emms-info-functions 'emms-info-libtag))
  
  (setq emms-info-asynchronously t)
  
  (setq emms-track-description-function #'ambrevar/emms-track-description-with-album)
  
  (setq emms-repeat-playlist t
        emms-stream-repeat-p t)
  (setq emms-browser-covers 'emms-browser-cache-thumbnail)
  
  (add-hook 'emms-browser-tracks-added-hook #'ambrevar/emms-play-on-add)
  (setq emms-volume-change-function #'emms-volume-pulse-change)
  (setq emms-volume-mode-timeout 0)
  (setq emms-volume-change-amount 2)
  
  (setq emms-browser-make-filter "all-files")
  (setq emms-browser-filter-only-type 'file)
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  
  
  (emms-lyrics 1)
  (emms-score 1)
  (emms-history-load)
  (emms-default-players))

(after! dired-x
  (add-to-list 'dired-guess-shell-alist-user
               (list "\\.\\(flac\\|mp3\\|ogg\\|wav\\|opus\\)\\'"
                     '(if (y-or-n-p "Add to emms playlist?")
                          (progn (emms-add-file (dired-get-filename))
                                 (keyboard-quit))
                        "mpv"))))

(use-package! helm-emms
  :bind ((:map doom-leader-map
           ("a/" . helm-emms)))
  :defer-incrementally t
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "helm-emms") . (nil . "Search music"))))
  :config
  (setq helm-emms-dired-directories (list (expand-file-name "~/hdd/unindexed-music/"))
        helm-emms-use-track-description-function t
        helm-emms-directory-files-recursive-fn #'helm-emms-walk-directory-with-find
        helm-emms-default-sources '(helm-source-emms-dired
                                    helm-source-emms-files
                                    ;;helm-source-emms-streams
                                    )))

(after! emms
  (require 'emms-mode-line-cycle)
  (require 'emms-mode-line-icon)

  (emms-mode-line 1)
  (emms-playing-time 1)

  ;; (emms-lyrics-mode-line)
  (emms-mode-line-cycle 1)

  (custom-set-variables
   '(emms-mode-line-cycle-max-width 30)
   '(emms-mode-line-cycle-additional-space-num 2)
   '(emms-mode-line-cycle-use-icon-p nil)
   '(emms-mode-line-format " [%s]")
   '(emms-mode-line-cycle-any-width-p t)
   '(emms-mode-line-cycle-velocity 2)
   '(emms-mode-line-cycle-current-title-function
     (lambda ()
       (substring-no-properties (let ((track (emms-playlist-current-selected-track)))
                                  (cl-case (emms-track-type track)
                                    ((streamlist)
                                     (let ((stream-name (emms-stream-name
                                                         (emms-track-get track 'metadata))))
                                       (if stream-name stream-name (emms-track-description track))))
                                    ((url) (emms-track-description track))
                                    (t (file-name-nondirectory
                                        (emms-track-description track)))))
                                nil -8)
       ))
   '(emms-mode-line-titlebar-function nil)))

(when (featurep! :editor evil)
  (map! :map emms-playlist-mode-map
        :n "q" #'bury-buffer))

(use-package! somafm
  :bind (:map doom-leader-map
          ("a@" . somafm))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "somafm") . (nil . "Somafm"))))
  :config
  (evil-set-initial-state 'somafm-mode 'emacs)
  (map! :map somafm-mode-map
        :e "e" #'previous-line
        :e "p" #'somafm--refresh-channels
        :e "i" #'somafm--play))

(after! ivy
  (ivy-set-actions
   'counsel-virtualbox
   `(("r" counsel-virtualbox-action-run "run")
     ("s" counsel-virtualbox-action-save "save")
     ("p" counsel-virtualbox-action-power-off "power off")
     ("n" ,(lambda (x) (kill-new (second x))) "copy name")
     ("g" ,(lambda (x) (kill-new (third x))) "copy guest os")))

  (defface counsel-virtualbox-name
    '((t :inherit font-lock-variable-name-face))
    "Face used by `counsel-virtualbox' for names."
    :group 'ivy-faces)

  (defface counsel-virtualbox-guest-os
    '((t :inherit font-lock-comment-face))
    "Face used by `counsel-virtualbox' for guest os."
    :group 'ivy-faces)

  (defface counsel-virtualbox-state-running
    '((t :inherit success))
    "Face used by `counsel-virtualbox' for running state."
    :group 'ivy-faces)

  (defface counsel-virtualbox-state-saved
    '((t :inherit font-lock-constant-face))
    "Face used by `counsel-virtualbox' for saved state."
    :group 'ivy-faces)

  (defface counsel-virtualbox-state-aborted
    '((t :inherit error))
    "Face used by `counsel-virtualbox' for aborted state."
    :group 'ivy-faces)

  (defface counsel-virtualbox-state-powered-off
    '((t :inherit font-lock-comment-face))
    "Face used by `counsel-virtualbox' for powered off state."
    :group 'ivy-faces)

  (defface counsel-virtualbox-state-other
    '((t :inherit warning))
    "Face used by `counsel-virtualbox' for other states."
    :group 'ivy-faces)

  (defun counsel--virtualbox-run (name)
    "Run virtualbox by NAME."
    (message "Run virtualbox %s" (propertize name 'face 'counsel-virtualbox-name))
    (call-process-shell-command (concat "VBoxSDL --startvm '" name "' &") nil 0))

  (defun counsel--virtualbox-save (name)
    "Save virtualbox by NAME."
    (message "Save virtualbox %s" (propertize name 'face 'counsel-virtualbox-name))
    (call-process-shell-command (concat "VBoxManage controlvm '" name "' savestate &") nil 0))

  (defun counsel--virtualbox-power-off (name)
    "Power off virtualbox by NAME."
    (message "Power off virtualbox %s" (propertize name 'face 'counsel-virtualbox-name))
    (call-process-shell-command (concat "VBoxManage controlvm '" name "' poweroff &") nil 0))

  (defun counsel--virtualbox-state-face (state)
    "Get face by STATE."
    (pcase state
      ("running" 'counsel-virtualbox-state-running)
      ("saved" 'counsel-virtualbox-state-saved)
      ("powered off" 'counsel-virtualbox-state-powered-off)
      ("aborted" 'counsel-virtualbox-state-aborted)
      (_ 'counsel-virtualbox-state-other)))

  (defun counsel-virtualbox-action (x)
    "Action on candidate X."
    (let ((name (second x))
          (state (fourth x)))
      (pcase state
        ("running" (counsel--virtualbox-save name))
        ("saved" (counsel--virtualbox-run name))
        ("powered off" (counsel--virtualbox-run name))
        ("aborted" (counsel--virtualbox-run name))
        (_ (message "No action taken on %s virtualbox %s"
                    (propertize state 'face (counsel--virtualbox-state-face state))
                    (propertize name 'face 'counsel-virtualbox-name))))))

  (defun counsel-virtualbox-action-run (x)
    "Run on candidate X."
    (counsel--virtualbox-run (second x)))

  (defun counsel-virtualbox-action-save (x)
    "Save on candidate X."
    (counsel--virtualbox-save (second x)))

  (defun counsel-virtualbox-action-power-off (x)
    "Power off on candidate X."
    (counsel--virtualbox-power-off (second x)))

  (defun counsel--virtualbox-candidates ()
    "Return list of `counsel-virtualbox' candidates."
    (with-temp-buffer
      (insert (shell-command-to-string "VBoxManage list -l vms"))
      (let ((case-fold-search t)
            candidates
            state
            guest-os
            name)
        (while (re-search-backward "^State:\s*\\(.*\\)\s*(.*" nil t)
          (setq state (string-trim (match-string 1)))
          (if (re-search-backward "^Guest OS:\s*\\(.*\\)" nil t)
              (setq guest-os (string-trim (match-string 1)))
            (signal 'error (list "Parsing virtualbox from output"
                                 "No Guest OS found"
                                 (buffer-string))))
          (if (re-search-backward "^Name:\s*\\(.*\\)" nil t)
              (setq name (string-trim (match-string 1)))
            (signal 'error (list "Parsing virtualbox from output"
                                 "No Name found"
                                 (buffer-string))))
          (push (list
                 (format "%-30s %-40s %s"
                         (propertize name 'face 'counsel-virtualbox-name)
                         (propertize guest-os 'face 'counsel-virtualbox-guest-os)
                         (propertize state 'face (counsel--virtualbox-state-face state)))
                 name
                 guest-os
                 state)
                candidates))
        candidates)))
  (counsel--virtualbox-candidates)

  (defun counsel-virtualbox ()
    "Complete VirtualBox with Ivy."
    (interactive)
    (ivy-read "virtualbox: " (counsel--virtualbox-candidates)
              :history 'counsel-virtualbox-history
              :action #'counsel-virtualbox-action
              :caller 'counsel-virtualbox
              :require-match t)))

(map! :after ivy
      :leader
      :desc "Virtual Box" "av" #'counsel-virtualbox)

(use-package! w3m
  :defer-incrementally t
  :bind (:map doom-leader-map
          ("a3" . +w3m))
  :init
  :config
  (load (expand-file-name "w3m-type-ahead" doom-private-dir))
  (add-hook 'w3m-mode-hook #'w3m-type-ahead-mode))

(defvar +w3m-workspace-name "*w3m*")
(defvar +w3m--old-wconf nil)

(add-hook 'w3m-mode-hook #'+w3m-init-h)

(defun +w3m ()
  (interactive)
  (if (featurep! :ui workspaces)
      (+workspace-switch +w3m-workspace-name t)
    (setq +w3m--old-wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer)))
  (if (buffer-live-p (get-buffer "*w3m*"))
      (w3m)
    (call-interactively #'w3m-goto-url)))

(defun +w3m-init-h ()
  (add-hook 'kill-buffer-hook #'+w3m-kill-w3m-h nil t))

(defun +w3m-kill-w3m-h ()
  (cond
   ((and (featurep! :ui workspaces) (+workspace-exists-p +w3m-workspace-name))
    (+workspace/delete +w3m-workspace-name))

   (+w3m--old-wconf
    (set-window-configuration +w3m--old-wconf)
    (setq +w3m--old-wconf nil))))

(when (featurep! :editor evil)
  (add-hook 'w3m-mode-hook #'evil-emacs-state))

(after! w3m
  (define-key w3m-mode-map (kbl-kbd "q") #'+workspace/other))

(use-package! ivy-youtube
  :bind (:map doom-leader-map
          ("ay" . ivy-youtube-music)
          ;; ("sy" . ivy-youtube)
          )
  :defer-incrementally t
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "ivy-youtube") . (nil . "Search youtube"))))
  :config
  (defvar ivy-youtube-dl-process nil)
  (defvar ivy-youtube-music-only? nil)

  (defsubst ivy-youtube-dl-music-command (video-url)
    (list (executable-find "youtube-dl")
          "--extract-audio"
          "-f"
          "bestaudio"
          "-k"
          "-o"
          "/home/tony/hdd/youtube/%(title)s.%(ext)s"
          video-url))

  (defun ivy-youtube-play-on-process (video-url)
    "Start a process based on ivy-youtube-play-at variable passing VIDEO-URL."
    (message (format "Starting a process with: [%s %s]" ivy-youtube-play-at video-url))
    (setq ivy-youtube-dl-process
          (make-process :name "Ivy Youtube"
                        :buffer "*Ivy Youtube Output*"
                        :sentinel (lambda (process event)
                                    (message
                                     (format "Ivy Youtube: Process %s (Check buffer *Ivy Youtube Output*)" event)))
                        :command (if ivy-youtube-music-only?
                                     (ivy-youtube-dl-music-command video-url)
                                   (list ivy-youtube-play-at video-url))))
    (when ivy-youtube-music-only?
      (set-process-sentinel ivy-youtube-dl-process
                            #'ivy-youtube-dl-msg)))

  (defun ivy-youtube-dl-msg (&rest args)
    (with-current-buffer "*Ivy Youtube Output*"
      (require 'emms)
      (emms-play-file
       (progn (end-of-buffer)
              (buffer-substring-no-properties
               (1- (point))
               (progn (forward-char -1)
                      (beginning-of-line)
                      (search-forward "/home/" nil t)
                      (forward-char (- (length "/home/")))
                      (point))))))
    (setq ivy-youtube-music-only? nil))

  (defun ivy-youtube-music ()
    (interactive)
    (setq ivy-youtube-music-only? t)
    (when (and (processp emms-player-mpv-proc)
               (process-live-p emms-player-mpv-proc))
      (kill-process emms-player-mpv-proc))
    (call-interactively #'ivy-youtube))

  ;; Set Youtube API key
  (load (expand-file-name "ivy-youtube" doom-private-dir))

  (setq ivy-youtube-play-at (executable-find "mpv")))

(use-package! mentor
  :defer-incrementally t
  :bind (:map doom-leader-map
          ("at" . mentor))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist '((nil . "mentor") . (nil . "rTorrent"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'mentor-mode 'emacs))
  (setq mentor-rtorrent-download-directory (expand-file-name "~/torrents")))

(use-package! forecast
  :defer-incrementally t
  :commands forecast
  :bind (:map doom-leader-map
          ("aw" . forecast))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist '((nil . "forecast") . (nil . "Weather"))))
  :config
  (setq forecast-api-key "3952024acf85777d62f39869da12f853")
  (setq forecast-units 'us)
  (setq forecast-language 'en)
  (map! :map forecast-mode-map
        :n "q" #'forecast-quit))

(use-package! tldr
  :commands (tldr)
  :bind (:map doom-leader-map
          ("alc" . tldr))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "tldr") . (nil . "Community-driven manpages"))))
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/")))

(use-package! speed-type
  :defer-incrementally t
  :commands (speed-type-text)
  :bind (:map doom-leader-map
          ("aT" . speed-type-text))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "speed-type-text") . (nil . "Typing practice"))))
  (when (featurep! :editor evil)
    (defun +amos*evil-insert (&rest _)
      (evil-insert-state))
    (advice-add #'speed-type--setup :after #'+amos*evil-insert))
  (map!
   :map speed-type--completed-keymap
   :ni "q" #'kill-this-buffer
   :ni "r" #'speed-type--replay
   :ni "n" #'speed-type--play-next))

(defun kisaragi/qr-encode (str &optional buf)
  "Encode STR as a QR code.
Return a new buffer or BUF with the code in it."
  (interactive "MString to encode: ")
  (let ((buffer (get-buffer-create (or buf "*QR Code*")))
        (format (if (display-graphic-p) "PNG" "UTF8"))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))
    (make-process
     :name "qrencode" :buffer buffer
     :command `("qrencode" ,str "-t" ,format "-o" "-")
     :coding 'no-conversion
     ;; seems only the filter function is able to move point to top
     :filter (lambda (process string)
               (with-current-buffer (process-buffer process)
                 (insert string)
                 (goto-char (point-min))
                 (set-marker (process-mark process) (point))))
     :sentinel (lambda (process change)
                 (when (string= change "finished\n")
                   (with-current-buffer (process-buffer process)
                     (cond ((string= format "PNG")
                            (image-mode)
                            (image-transform-fit-to-height))
                           (t           ;(string= format "UTF8")
                            (text-mode)
                            (decode-coding-region (point-min) (point-max) 'utf-8)))))))
    (when (called-interactively-p 'interactive)
      (display-buffer buffer))
    buffer))
(after! which-key
  (add-to-list 'which-key-replacement-alist
               '((nil . "kisaragi/qr-encode") . (nil . "QR encode"))))
(map! :leader
      "aQ" #'kisaragi/qr-encode)

(use-package! disk-usage
  :commands disk-usage
  :bind (:map doom-leader-map
          ("ad" . disk-usage))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "disk-usage") . (nil . "Disk usage")))))

(use-package! spray
  :defer-incrementally t
  :commands spray-mode
  :bind (:map doom-leader-map
          ("aS" . spray-mode))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "spray-mode") . (nil . "Speed read"))))
  :config
  (map! :map spray-mode-map
        "i" #'spray-forward-word
        "m" #'spray-backward-word
        "n" #'spray-faster
        "e" #'spray-slower)
  (when (featurep! :editor evil)
    (add-hook 'spray-mode-hook #'evil-emacs-state)
    (advice-add #'spray-quit :after
                (lambda (&rest _)
                  (evil-normal-state)))))

(use-package! md4rd
  :defer-incrementally t
  :commands md4rd
  :bind (:map doom-leader-map
          ("ar" . md4rd))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "md4rd") . (nil . "Reddit"))))
  :config
  (map! :map md4rd-mode-map
        :n "l" #'tree-mode-goto-parent
        :n "y" #'md4rd-open
        :n "d" #'md4rd-visit
        :n "c" #'tree-mode-toggle-expand
        :n "TAB" #'tree-mode-toggle-expand
        :n "c" #'md4rd-widget-expand-all
        :n "f" #'md4rd-widget-collapse-all
        :n "k" #'widget-forward
        :n "n" #'widget-forward
        :n "m" #'backward-button
        :n "'" #'widget-backward
        :n "e" #'widget-backward
        :n "i" #'forward-button
        :n "q" #'kill-current-buffer
        :n "p" #'md4rd-reply
        :n "l" #'md4rd-upvote
        :n "s" #'md4rd-downvote
        :n "z" #'md4rd-widget-toggle-line
        :n "=" #'md4rd-indent-all-the-lines)
  (add-hook! 'md4rd-mode-hook #'md4rd-indent-all-the-lines)
  (setq md4rd-subs-active '(emacs orgmode))
  (load! "md4rd"))

(use-package! sx
  :defer-incrementally t
  :init
  (defun +sx ())
  :config
  (map! :leader
        (:prefix-map ("as" . "Stack Exchange")
          :desc "All questions" "q" #'sx-tab-all-questions
          :desc "Inbox" "i" #'sx-inbox
          :desc "Open link" "o" #'sx-open-link
          :desc "Unanswered-My-Tags questions" "u" #'sx-tab-unanswered-my-tags
          :desc "Ask" "a" #'sx-ask
          :desc "Search" "s" #'sx-search))
  (setq sx-default-site "mathoverflow.net"
        sx-question-mode-display-buffer-function 'display-buffer)
  (set-popup-rule! "^\\*sx-question\\*$" :size 0.95 :side 'bottom :quit t :focus t)
  (load! "sx"))

(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("\\`SPC a g\\'" . nil) . (nil . "games"))))

(use-package! tetris
  :commands tetris
  :bind (:map doom-leader-map
          ("agt" . tetris))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "tetris") . (nil . "Tetris")))))

(use-package! doctor
  :commands doctor
  :bind (:map doom-leader-map
          ("agd" . doctor))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "doctor") . (nil . "Doctor"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'doctor-mode 'insert)
    (map! :map doctor-mode-map
          :n "q" #'bury-buffer)))

(use-package! jumblr
  :commands jumblr
  :bind (:map doom-leader-map
          ("agj" . jumblr))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "jumblr") . (nil . "Jumblr"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'jumblr-mode 'insert)))

(use-package! pong
  :commands pong
  :bind (:map doom-leader-map
          ("agp" . pong))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "pong") . (nil . "Pong")))))

(use-package! snake
  :commands snake
  :bind (:map doom-leader-map
          ("ags" . snake))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "snake") . (nil . "Snake"))))
  :config
  (map! :map snake-mode-map
        :nmvie "n" #'snake-move-down
        :nmvie "e" #'snake-move-up
        :nmvie "m" #'snake-move-left
        :nmvie "i" #'snake-move-right
        :nmvie "<space>" #'snake-pause-game
        :nmvie "SPC" #'snake-pause-game
        :nmvie "<backspace>" #'snake-start-game))

(use-package! dunnet
  :commands dunnet
  :bind (:map doom-leader-map
          ("agd" . dunnet))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "dunnet") . (nil . "Dunnet")))))

(use-package! 2048-game
  :commands 2048-game
  :bind (:map doom-leader-map
          ("ag2" . 2048-game))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "2048-game") . (nil . "2048")))))

(use-package! gomoku
  :commands gomoku
  :bind (:map doom-leader-map
          ("ag%" . gomoku))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "gomoku") . (nil . "5-in-a-row")))))

(use-package! 5x5
  :commands 5x5
  :bind (:map doom-leader-map
          ("ag5" . 5x5)))

(use-package! minesweeper
  :commands minesweeper
  :bind (:map doom-leader-map
          ("agm" . minesweeper))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "minesweeper") . (nil . "Minesweeper")))))

(use-package! gnugo
  :commands gnugo
  :bind (:map doom-leader-map
          ("agg" . gnugo))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "gnugo") . (nil . "go")))))

(use-package! mpuz
  :commands mpuz
  :bind (:map doom-leader-map
          ("agx" . mpuz))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "mpuz") . (nil . "Multiplication puzzle")))))

(use-package! bubbles
  :commands bubbles
  :bind (:map doom-leader-map
          ("agb" . bubbles))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "bubbles") . (nil . "Bubbles")))))

(use-package! key-quiz
  :commands key-quiz
  :bind (:map doom-leader-map
          ("agk" . key-quiz))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "key-quiz") . (nil . "Keybinding quiz"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'key-quiz-mode 'emacs)))

(use-package! malyon
  :commands malyon
  :bind (:map doom-leader-map
          ("agz" . malyon))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "malyon") . (nil . "Z-machine"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'malyon-mode 'emacs)))

(use-package! binarytrainer
  :commands (play-binary play-hex)
  :bind (:map doom-leader-map
          ("ag0" . play-binary)
          ("ag1" . play-hex))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "play-binary") . (nil . "Binary conversion quiz")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "play-hex") . (nil . "Hex conversion quiz"))))
  :config
  (require 'cl-format))

(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("\\`SPC a q\\'" . nil) . (nil . "quotes"))))

(use-package! cookie1
  :commands (cookie)
  :bind (:map doom-leader-map
          ("aqc" . cookie))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "cookie") . (nil . "Fortune cookie"))))
  :config
  (setq cookie-file "~/docs/ascii/misc/fortunes/fortunes"))

(use-package! faith
  :commands (faith
             faith-quote
             faith-insert
             faith-correct-buffer
             faith-correct-region
             faith-correct-string)
  :bind (:map doom-leader-map
          ("aqf" . faith-quote))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "faith-quote") . (nil . "Chuch of Emacs")))))

(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("\\`SPC a e\\'" . nil) . (nil . "eyecandy"))))

(use-package! hanoi
  :commands (hanoi hanoi-unix hanoi-unix-64)
  :bind (:map doom-leader-map
          ("aehh" . hanoi)
          ("aehu" . hanoi-unix)
          ("aeh6" . hanoi-unix-64))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '(("\\`SPC a e h\\'" . nil) . (nil . "Hanoi")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "hanoi") . (nil . "Hanoi")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "hanoi-unix") . (nil . "Hanoi Unix")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "hanoi-unix-64") . (nil . "Hanoi Unix 64")))))

(use-package! dissociate
  :commands dissociated-press
  :bind (:map doom-leader-map
          ("aed" . dissociated-press))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "dissociated-press") . (nil . "Dissociated press")))))

(use-package! life
  :commands life
  :bind (:map doom-leader-map
          ("aeL" . life))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "life") . (nil . "Life"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'life-mode 'emacs)))

(use-package! zone
  :commands zone
  :bind (:map doom-leader-map
          ("aez" . zone))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "zone") . (nil . "Zone")))))

(use-package! fireplace
  :commands fireplace
  :bind (:map doom-leader-map
          ("aef" . fireplace))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "fireplace") . (nil . "Fireplace"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'fireplace-mode 'emacs)))

(after! smartparens
  (map! :eig [C-S-backspace] #'sp-backward-delete-symbol))

(after! doom-themes
  (add-to-list 'doom-themes-base-faces
               '(hl-fill-column-face :inherit 'shadow)))

(after! popup
  (set-popup-rules! '(("^\\*Agda information" :size 0.3 :focus nil)
                      ("\\*intero:global-project::repl" :size 0.3 :focus t)
                      ("\\*haskell-process-log" :size 0.3 :focus nil :quit t)
                      ("\\*test\\*" :size 0.3 :focus t)
                      ("\\*Compile-Log\\*" :size 0.3 :focus nil :quit t)
                      ("^\\*Org Agenda\\*" :size 0.5 :side 'bottom)
                      ("^\\*eww\\*" :ignore t)
                      ("^\\*cfw:details\\*" :size 0.35)
                      ("URxvt" :size 0.35 :side 'bottom :focus t :quit nil)
                      ("Helm systemd" :ignore t))))

(when (featurep! :ui workspaces)
  (map! "C-c C-1" #'+workspace/switch-to-0
        "C-c C-2" #'+workspace/switch-to-1
        "C-c C-3" #'+workspace/switch-to-2
        "C-c C-4" #'+workspace/switch-to-3
        "C-c C-5" #'+workspace/switch-to-4
        "C-c C-6" #'+workspace/switch-to-5
        "C-c C-7" #'+workspace/switch-to-6
        "C-c C-8" #'+workspace/switch-to-7
        "C-c C-9" #'+workspace/switch-to-8
        "C-c C-0" #'+workspace/switch-to-final))

(add-hook! eshell-mode
  (eldoc-mode -1))

(when (featurep! :editor evil)
  (after! evil-snipe
    (setq evil-snipe-scope 'line
          evil-snipe-spillover-scope 'visible)))

(add-hook 'evil-insert-state-exit-hook #'expand-abbrev)

(after! view
  (map! :map view-mode-map
        :n "0" nil))

(when (featurep! :editor evil)
  (when (display-graphic-p)
    (after! evil-org
      (map! :map evil-org-mode-map
            :nv "TAB" nil
            :nv "<tab>" #'org-cycle)))
  (after! lispy
    (map! :map lispy-mode-map
          [remap pop-tag-mark] #'better-jumper-jump-backward)))

(after! evil
  (defun fix-miss-drag (&rest _x)
    (when (region-active-p)
      (cl-destructuring-bind (beg . end) (car (region-bounds))
        (when (> 4 (- end beg))
          (evil-normal-state)))))

  (if (fboundp #'jit-disassemble)
      (defadvice evil-mouse-drag-region
          (after evil-mouse-drag-region-advice (&rest args) activate)
        (fix-miss-drag))
    (advice-add #'evil-mouse-drag-region :after
                #'fix-miss-drag))

  (if (fboundp #'jit-disassemble)
      (defadvice mouse-set-region
          (after mouse-set-region-advice (&rest args) activate)
        (deactivate-mark))
    (advice-add #'mouse-set-region :after #'deactivate-mark))
  
  (if (fboundp #'jit-disassemble)
      (defadvice mouse-drag-region
          (after mouse-drag-region-advice (&rest args) activate)
        (deactivate-mark))
    (advice-add #'mouse-drag-region :after #'deactivate-mark)))

(after! company
  (setq company-idle-delay 0.8
        company-minimum-prefix-length 1))

(use-package! company-quickhelp
  :after company
  :custom
  (company-quickhelp-margin 15)
  (company-quickhelp-delay nil)
  :hook (company-mode . company-quickhelp-local-mode))

(after! company
  (map! :map company-active-map
        "C-c C-1" (lambda! (company-complete-number 1))
        "C-c C-2" (lambda! (company-complete-number 2))
        "C-c C-3" (lambda! (company-complete-number 3))
        "C-c C-4" (lambda! (company-complete-number 4))
        "C-c C-5" (lambda! (company-complete-number 5))
        "C-c C-6" (lambda! (company-complete-number 6))
        "C-c C-7" (lambda! (company-complete-number 7))
        "C-c C-8" (lambda! (company-complete-number 8))
        "C-c C-9" (lambda! (company-complete-number 9))
        "C-c C-0" (lambda! (company-complete-number 10))
        ))

(after! texmathp
  (add-to-list 'texmathp-tex-commands '("tikzcd" env-on))
  (add-to-list 'texmathp-tex-commands '("xymatrix" env-on))
  (texmathp-compile))

(after! font-latex
  (add-to-list 'font-latex-math-environments "tikzcd")
  (add-to-list 'font-latex-math-environments "xymatrix"))

(after! cdlatex
  (add-to-list 'cdlatex-math-modify-alist '(?a "\\mathfrak" nil t nil nil))
  (add-to-list 'cdlatex-math-modify-alist '(?l "\\mathscr" nil t nil nil))
  (add-to-list 'cdlatex-math-modify-alist '(?w "\\mathbb" nil t nil nil))
  (add-to-list 'cdlatex-math-modify-alist '(?6 "\\widehat" nil t nil nil))
  (add-to-list 'cdlatex-math-symbol-alist '(?X "\\otimes"))
  (add-to-list 'cdlatex-math-symbol-alist '(?c "\\amalg"))
  (add-to-list 'cdlatex-math-symbol-alist '(?C "\\coprod"))
  (add-to-list 'cdlatex-math-symbol-alist '(?1 "\\colim"))
  (add-to-list 'cdlatex-math-symbol-alist '(?2 "\\varinjlim"))
  (add-to-list 'cdlatex-math-symbol-alist '(?. ("\\cdot" "\\cdots")))
  (set-popup-rule! "\\*CDLaTeX Help\\*" :side 'bottom :focus nil)
  )

(after! (:and org cdlatex)
  (define-abbrev-table 'math-abbrev-table
    '(("geq" "\\geq" nil 0)
      ("sk" "\\Sk" nil 0)
      ("ob" "\\Ob" nil 0)
      ("set" "\\Set" nil 0)
      ("sset" "\\sSet" nil 0)
      ("hom" "\\Hom" nil 0)
      )
    :enable-function #'lattie--math-p
    :regexp "\\(?:[^a-zA-Z0-9\\]+\\)\\([a-zA-Z0-9]+\\)"
    :case-fixed t)

  (setq abbrev-minor-mode-table-alist
        (list (cons 'org-cdlatex-mode math-abbrev-table))))

(when (featurep! :completion ivy)
  (advice-add #'helm-mode :around #'ignore)
  (after! org
    (map! :map org-mode-map
          :localleader
          "." #'counsel-org-goto
          "/" #'counsel-org-goto-all)))

(after! ivy
  (setq +ivy-buffer-preview 'everything))

(after! ivy
  (define-key ivy-minibuffer-map (kbl-kbd "j" 'control nil 'shift nil) #'ivy-immediate-done))

(add-hook! 'exwm-init-hook
  (after! ivy-posframe
    (add-to-list 'ivy-posframe-parameters '(parent-frame . nil))))

(after! ivy
  (defun ivy-magical-space ()
    "If there is a single ivy candidate and point is at the end of the minibuffer,
exit with that candidate, otherwise insert SPACE character as usual."
    (interactive)
    (call-interactively
     (if (and (= 1 (length ivy--old-cands))
              (= (point) (line-end-position)))
         #'ivy-done
       #'self-insert-command)))

  (define-key ivy-minibuffer-map (kbd "SPC") 'ivy-magical-space))

(after! dired
  (define-key dired-mode-map (kbl-kbd "@") #'emms-play-dired)
  (after! ranger
    (define-key ranger-mode-map (kbl-kbd "@") #'emms-play-dired)))

(map! :leader
      :desc "Calendar" "ac" #'=calendar)

(advice-add #'+calendar/quit
            :after
            (lambda (&rest _)
              (doom-kill-matching-buffers "^\\*cfw-calendar")))

(after! calfw
  (evil-set-initial-state 'cfw:calendar-mode 'normal)
  (map! :map cfw:calendar-mode-map
        :m "j" #'cfw:navi-next-week-command
        :m "k" #'cfw:navi-previous-week-command
        :m "h" #'cfw:navi-previous-day-command
        :m "l" #'cfw:navi-next-day-command
        :m "^" #'cfw:navi-goto-week-begin-command
        :m "$" #'cfw:navi-goto-week-end-command
        :m "gg" #'cfw:navi-goto-first-date-command
        :m "G" #'cfw:navi-goto-last-date-command
        :m "C-j" #'cfw:navi-next-week-command
        :m "C-k" #'cfw:navi-previous-week-command
        :n "." #'cfw:navi-goto-today-command
        :m "gd" #'cfw:navi-goto-date
        :nm "TAB" #'cfw:navi-next-item-command
        :n "zm" #'cfw:change-view-month
        :n "zw" #'cfw:change-view-week
        :n "zt" #'cfw:change-view-two-weeks
        :n "zd" #'cfw:change-view-day
        :n "gr" #'cfw:refresh-calendar-buffer
        :n "f" #'cfw:show-details-command
        :n "RET" #'cfw:org-jump-map
        :n "q" #'+calendar/quit
        :map cfw:details-mode-map
        :n "gr" #'cfw:refresh-calendar-buffer
        :n "gd" #'cfw:org-goto-date
        :n "C" #'org-capture
        :n "q" #'cfw:org-clean-exit
        :n "zd" #'cfw:change-view-day
        :n "zw" #'cfw:change-view-week
        :n "zt" #'cfw:change-view-two-weeks
        :n "zm" #'cfw:change-view-month)
  (defun cfw:org-clean-exit-restore-focus-advice (&rest _)
    (when (get-buffer-window "*cfw-calendar*")
      (select-window (get-buffer-window "*cfw-calendar*"))))
  (advice-add #'cfw:org-clean-exit :after #'cfw:org-clean-exit-restore-focus-advice))

(when (featurep! :app calendar)
  (defun my-open-calendar ()
    (interactive)
    (require 'calfw-cal)
    ;; (require 'calfw-ical)
    (require 'calfw-org)
    ;; (require 'calfw-howm)
    (cfw:open-calendar-buffer
     ;; :custom-map cfw:my-cal-map
     :contents-sources
     (list
      (cfw:org-create-source "Green")   ; orgmode source
      )))

  (setq +calendar-open-function #'my-open-calendar))

(after! org
  (add-to-list 'doom-incremental-packages 'ox-icalendar 'append))
(after! ox-icalendar
  (setq org-icalendar-alarm-time 40
        org-icalendar-include-todo t
        org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo event-if-todo)
        org-icalendar-store-UID t
        org-icalendar-combined-agenda-file "~/org/org.ics"))

(use-package! org-make-toc
  :hook (org-mode . org-make-toc-mode)
  :config
  (defun akirak/org-insert-toc-for-top-level ()
    (interactive)
    (unless (derived-mode-p 'org-mode)
      (user-error "Not in org-mode"))
    (org-with-wide-buffer
     (or (re-search-backward (rx bol "* ") nil t)
         (re-search-forward (rx bol "* ") nil t))
     (org-narrow-to-subtree)
     (when (org-find-property "TOC")
       (user-error "Already has a TOC"))
     (let ((heading (concat (make-string (org-get-valid-level 2) ?\*)
                            " ")))
       (if (re-search-forward (concat "^" (regexp-quote heading))
                              nil t)
           (beginning-of-line 1)
         (org-end-of-subtree)
         (unless (= 0 (car (posn-col-row (posn-at-point))))
           (insert "\n")))
       (insert heading "Table of contents\n")
       (beginning-of-line 0)
       (org-set-property "TOC" "siblings")))
    (add-file-local-variable 'before-save-hook 'org-make-toc)
    (save-buffer)
    (revert-buffer)))

(add-hook! 'org-mode-hook
  (setq display-line-numbers nil))

(add-hook 'org-mode-hook #'visual-line-mode)

(add-hook! 'org-mode-hook
  (eldoc-mode -1))

(after! org
  (require 'org-refile))

(use-package! org-ql
  :defer-incrementally t
  :bind (:map org-mode-map
          ("C-c s" . helm-org-ql)))

(after! org-journal
  (setq org-journal-find-file (lambda (&rest args)
                                (when (featurep! :ui workspaces)
                                  (+workspace-switch "Journal" t))
                                (apply #'find-file args))))

(after! org-journal
  (setq org-journal-file-format "%Y-%m-%d.org"
        org-journal-enable-agenda-integration t
        org-journal-file-pattern "/home/tony/org/journal/\\(?1:[0-9]\\{4\\}\\)-\\(?2:[0-9][0-9]\\)-\\(?3:[0-9][0-9]\\)\\.org\\(\\.gpg\\)?\\'")
  (org-journal-update-org-agenda-files))

(after! org-journal
  (add-to-list 'auto-mode-alist (cons org-journal-file-pattern 'org-journal-mode)))

(defvar org-journal-calendar-pre-window-conf nil)
(map! :leader
      :desc "Calendar" "njc" (lambda! (require 'org-journal)
                                      (setq org-journal-calendar-pre-window-conf (current-window-configuration))
                                      (calendar)))
(add-hook! 'doom-escape-hook
  (when (and (featurep 'org-journal)
             org-journal-calendar-pre-window-conf
             (get-buffer-window "*Calendar*"))
    (set-window-configuration org-journal-calendar-pre-window-conf)
    (setq org-journal-calendar-pre-window-conf nil)))

(after! org-journal
  (map! :map calendar-mode-map
        :n "o" #'org-journal-display-entry
        :n "w" #'org-journal-previous-entry
        :n "e" #'org-journal-next-entry
        :n "O" #'org-journal-new-date-entry
        :n "RET" (lambda! (let* ((_arg current-prefix-arg)
                                 (event last-nonmenu-event)
                                 (time (org-journal-calendar-date->time
                                        (calendar-cursor-to-date t event))))
                            (set-window-configuration org-journal-calendar-pre-window-conf)
                            (setq org-journal-calendar-pre-window-conf nil)
                            (org-journal-read-or-display-entry time nil)))
        :n "q" (lambda! (call-interactively #'calendar-exit)
                        (when org-journal-calendar-pre-window-conf
                          (set-window-configuration org-journal-calendar-pre-window-conf)
                          (setq org-journal-calendar-pre-window-conf nil))))
  (map! :map calendar-mode-map
        :localleader
        "w" #'org-journal-search-calendar-week
        "m" #'org-journal-search-calendar-month
        "y" #'org-journal-search-calendar-year))

(after! org-journal
  (el-patch-defun org-journal-read-or-display-entry (time &optional noselect)
    "Read an entry for the TIME and either select the new window when NOSELECT
is nil or avoid switching when NOSELECT is non-nil."
    (let* ((org-journal-file (org-journal-get-entry-path time))
           (buf-exists (get-file-buffer org-journal-file))
           buf point)
      (if (and (when (file-exists-p org-journal-file)
                 (setq buf (find-file-noselect org-journal-file)))
               ;; If daily continoue with body of if condition
               (or (org-journal-daily-p)
                   ;; Search for journal entry
                   (with-current-buffer buf
                     (save-mark-and-excursion
                       (goto-char (point-min))
                       (setq point (re-search-forward
                                    (format-time-string " *:CREATED: *%Y%m%d" time) nil t))))))
          (progn
            ;; Use `find-file-noselect' instead of `view-file' as it does not respect `auto-mode-alist'
            (with-current-buffer buf
              ;; Open file in view-mode if not opened already.
              (el-patch-swap
                (unless buf-exists
                  (view-mode)
                  (setq view-exit-action 'kill-buffer))
                nil)
              (set (make-local-variable 'org-hide-emphasis-markers) t)
              (unless (org-journal-daily-p)
                (goto-char point))
              (org-journal-finalize-view)
              (setq point (point)))
            (el-patch-swap (if noselect
                               (display-buffer buf t)
                             (funcall org-journal-find-file org-journal-file))
                           (if noselect
                               (progn (mapc #'delete-window (cdr (doom-visible-windows)))
                                      (set-window-buffer (car (doom-visible-windows)) buf))
                             (funcall org-journal-find-file org-journal-file)))
            (set-window-point (get-buffer-window (get-file-buffer org-journal-file)) point)
            buf)
        (message "No journal entry for this date.")))))

(after! org-journal
  (set-popup-rule! "^\\*Org-journal search\\*$" :side 'bottom :size 0.3)
  
  (el-patch-defun org-journal-search-by-string (str &optional period-start period-end)
    "Search for a string within a given time interval.

If STR is empty, search for all entries using `org-journal-time-prefix'."
    (when (time-less-p period-end period-start)
      (error "Period end cannot be before the start"))
    (let* ((search-str (if (string= "" str) org-journal-time-prefix str))
           (files (org-journal-search-build-file-list period-start period-end))
           (results (org-journal-search-do-search search-str files))
           (buf (get-buffer-create org-journal-search-buffer))
           (inhibit-read-only t))
      (unless (get-buffer-window buf 0)
        (el-patch-swap (switch-to-buffer buf)
                       (pop-to-buffer buf)))
      (with-current-buffer buf
        (org-journal-search-mode)
        (erase-buffer)
        (org-journal-search-print-results str results period-start period-end)
        (goto-char (point-min))
        (forward-button 1)
        (button-activate (button-at (point))))))

  (map! :map org-journal-search-mode-map
        :n "q" #'kill-this-buffer
        :n "j" #'org-journal-search-next
        :n "k" #'org-journal-search-prev))

(add-hook! 'org-journal-mode-hook
  (mixed-pitch-mode +1)
  (text-scale-set 2))

(after! org
  (use-package! secretaria
    :defer-incrementally t
    :config
    (secretaria-unknown-time-always-remind-me)))

(after! secretaria
  (defvar secretaria-deadline-expiry 30
    "Amount of days necessary to auto-silence an overdue deadline.")

  (defun secretaria-get-overdue-appt ()
    (let* ((files (org-agenda-files))
           (appts)
           (entries (progn (setf org-agenda-buffer
                                 (when (buffer-live-p org-agenda-buffer)
                                   org-agenda-buffer))
                           (-non-nil
                            (-flatten (cl-loop for i from 1 to secretaria-deadline-expiry
                                               collect (cl-loop for file in (org-agenda-files)
                                                                collect (when-let ((entry (org-agenda-get-day-entries file (calendar-current-date (- i)) :scheduled :deadline)))
                                                                          (vector (car entry)
                                                                                  i))))))))
           (regexp (secretaria--leaders-prepare t))
           (org-agenda-skip-function '(secretaria--skip-entry-if-done))
           (org-clock-current-task (or org-clock-current-task "")))
      (dolist (entry entries)
        (when (and (string-match-p regexp (get-text-property 0 'extra (aref entry 0)))
                   (string-empty-p (get-text-property 0 'time (aref entry 0)))
                   (not (string-equal org-clock-current-task (substring-no-properties (get-text-property 0 'txt (aref entry 0))))))
          (push (vector (substring-no-properties (get-text-property 0 'txt (aref entry 0)))
                        (aref entry 1)) appts)))
      appts))

  (defun secretaria-filter-finished-appts (appts)
    "It should be better if this checked for the CLOSED property."
    (--remove (string-prefix-p "[X] " (or (and (stringp it)
                                               it)
                                          (and (vectorp it)
                                               (stringp (aref it 0))
                                               (aref it 0))))
              appts))

  (defun secretaria-remove-checkboxes-from-title (s)
    (string-remove-prefix "[ ] " s))

  (defun secretaria-alert-overdue-unknown-time-appt ()
    (let ((appts (secretaria-filter-finished-appts (secretaria-get-overdue-appt))))
      (dolist (entry appts)
        (alert (concat "(Task overdue by "
                       (number-to-string (aref entry 1))
                       " days)"
                       ;; ", time unspecified"
                       )
               :title (secretaria-remove-checkboxes-from-title (or (aref entry 0) "(no title)"))
               :severity 'high
               :mode 'org-mode))))

  (el-patch-defun secretaria-alert-unknown-time-appt ()
    "Tell the user about tasks scheduled for today.

Those tasks have no time of the day specified"
    (let ((appts (secretaria-filter-finished-appts (secretaria-get-appt 'unknown))))
      (dolist (entry appts)
        (el-patch-swap (alert "(Task for today, time unspecified"
                              :title (or entry "(no title)")
                              :severity (secretaria--conditional-severity)
                              :mode 'org-mode)
                       (alert ""
                              :title (secretaria-remove-checkboxes-from-title (or entry "(no title)"))
                              :severity (secretaria--conditional-severity)
                              :mode 'org-mode)))))

  ;; (secretaria-alert-overdue-unknown-time-appt)
  ;; (secretaria-alert-unknown-time-appt)

  (defvar secretaria-overdue-unknown-time-reminder-timer nil)
  (setf secretaria-overdue-unknown-time-reminder-timer
        (run-at-time (format "%s min" (or secretaria-unknown-time-remind-time 30))
                     (* (or secretaria-unknown-time-remind-time 30) 60) 'secretaria-alert-overdue-unknown-time-appt)))

(after! secretaria
  (defun my-org-agenda-to-appt ()
    (interactive)
    (setq appt-time-msg-list nil)
    (let ((org-deadline-warning-days 0))    ;; will be automatic in org 5.23
      (org-agenda-to-appt)))

  (defun aj/appt-notify (until time msg)
    "Use `alert' to for appointment notifications."
    (if (listp msg)
        (dolist (i (number-sequence 0 (1- (length until))))
          (alert (nth i msg) :title "Appointment Reminder" :category 'calendar))
      (alert msg :title "Appointment Reminder" :category 'calendar)))

  ;; Advice the agenda refresh to update appts.
  (defadvice org-agenda-redo (after update-appts activate)
    "Update `appt' lists from the agenda."
    (message "Updating appointments...")
    (my-org-agenda-to-appt))

  (my-org-agenda-to-appt)
  (appt-activate +1)
  (setq appt-message-warning-time 20
        appt-display-interval 3
        appt-display-mode-line nil
        appt-disp-window-function #'aj/appt-notify
        appt-delete-window-function #'ignore))

(after! org-agenda
  (setq org-agenda-sticky t
        org-agenda-start-with-clockreport-mode t))

(after! org-capture
  (add-to-list 'org-capture-templates
               `("a" "Appointment" entry
                 ,(list 'file
                        (concat org-directory "appointments.org"))
                 "* %?\nSCHEDULED: %^T\n%a\n"))
  ;; (setq org-capture-templates
  ;;       (--remove (equal (car it) "n")
  ;;                 org-capture-templates))
  ;; (require 'doct)
  ;; (add-to-list 'org-capture-templates
  ;;              (doct '(("Note"
  ;;                       :keys "n"
  ;;                       :file (lambda () (let ((file (concat default-directory "notes.org")))
  ;;                                     ;;create if doesn't exist
  ;;                                     (set-buffer (find-file-noselect file t t))
  ;;                                     (write-file file) file))
  ;;                       :template (lambda () (if (y-or-n-p "link?")
  ;;                                           "* %doct:todo-state %A"
  ;;                                         "* %doct:todo-state %?"))
  ;;                       :todo-state "TODO"
  ;;                       :children (("bug" :keys "b"
  ;;                                   :headline "Bugs :bug:")
  ;;                                  ("enhancement" :keys "e"
  ;;                                   :headline "Ehnancements :enhancement:"
  ;;                                   :todo-state "IDEA")
  ;;                                  ("feature" :keys "f"
  ;;                                   :headline "Features :feature:"
  ;;                                   :todo-state "IDEA")
  ;;                                  ("optimization" :keys "o"
  ;;                                   :headline "Optimizations :optimization:")
  ;;                                  ("security" :keys "s"
  ;;                                   :headline "Security :security:"))))))
  )

(after! org-clock
  (setq org-clock-history-length 23
        org-clock-out-remove-zero-time-clocks t)
  (org-clock-persistence-insinuate))

(after! org
  (add-to-list 'org-modules 'org-crypt)
  (add-to-list 'org-tags-exclude-from-inheritance "crypt")
  (after! org-crypt
    (setq org-crypt-key user-mail-address)
    (org-crypt-use-before-save-magic)
    (setq org-crypt-disable-auto-save t)
    (map! :map org-mode-map
          "C-c d" #'org-decrypt-entry)))

(after! org
  (setq org-log-done t)
  (setq org-log-done-with-time t))

(use-package! org-graph-view
  :commands org-graph-view
  :config
  (set-popup-rule! "^\\*org-graph-view\\*$" :size 0.4 :side 'bottom :quit t)
  (map! :map org-graph-view-map
        :nie "q" #'bury-buffer))

(after! org-keys
  (setq org-use-speed-commands t))

(defvar +org-exit-src-code-hook nil
  "Hook run just before exiting a org source block buffer.")

(defun +org|run-exit-src-code-hooks (&rest _)
  "Runs all hooks in `+org-exit-src-code-hook`."
  (run-hooks '+org-exit-src-code-hook))

(advice-add #'org-edit-src-exit :before #'+org|run-exit-src-code-hooks)

(add-hook '+org-exit-src-code-hook #'ws-butler-trim-eob-lines)

(use-package! company-org-block
  :when (featurep! :completion company)
  :functions (company-org-block)
  :init
  (after! org
    (set-company-backend! 'org-mode 'company-org-block)))

(after! org
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (C . t)
                                   (R . t)
                                   (shell . t)
                                   (python . t)
                                   (lilypond . t)))
  (add-to-list 'doom-incremental-packages 'ob-lilypond 'append)
  (after! ob-lilypond
    (require 'lilypond-mode)
    (setq org-babel-lilypond-arrange-mode t)))

(after! org
  (add-to-list 'org-structure-template-alist
               '("l" . "src emacs-lisp"))
  (setq org-structure-template-alist (remove '("l" . "export latex") org-structure-template-alist)))

(after! org
  (defun org-insert-structure-template--pretty-advice (&rest _)
    (let (beg end)
      (save-excursion
        (re-search-backward "\n[\s\t]*#\\+begin_" nil t)
        (goto-char (match-beginning 0))
        (setq beg (point))
        (upcase-word 1)
        (when (looking-at "_[a-z]+")
          (upcase-word 1))
        (re-search-forward "\n[\s\t]*#\\+end_" nil t)
        (goto-char (match-beginning 0))
        (upcase-word 2)
        (end-of-line)
        (setq end (point)))
      (indent-region beg end))
    (when (looking-at "#\\+END_")
      (forward-char -1)
      (newline-and-indent)))

  (if (fboundp #'jit-disassemble)
      (defadvice org-insert-structure-template
          (after org-insert-structure-template--pretty-advice (&rest args) activate)
        (apply #'org-insert-structure-template--pretty-advice args))
    (advice-add #'org-insert-structure-template
                :after
                #'org-insert-structure-template--pretty-advice)))

;; Slightly different from the original for compatibility with my advice on
;; org-insert-structure-template
(after! org
  (defun akirak/org-yank-into-new-block ()
    (interactive)
    (let ((begin (point))
          done)
      (unwind-protect
          (progn
            (end-of-line)
            (yank)
            (push-mark begin)
            (setq mark-active t)
            (call-interactively #'org-insert-structure-template)
            (setq done t)
            (deactivate-mark)
            (let ((case-fold-search t))
              (re-search-forward (rx bol "#+END_")))
            (forward-line 1))
        (unless done
          (deactivate-mark)
          (delete-region begin (point))))))

  (map! :map org-mode-map
        "C-c C-'" #'akirak/org-yank-into-new-block))

(after! org
  (defun modi/org-in-any-block-p ()
    "Return non-nil if the point is in any Org block.
The Org block can be *any*: src, example, verse, etc., even any
Org Special block.
This function is heavily adapted from `org-between-regexps-p'."
    (save-match-data
      (let ((pos (point))
            (case-fold-search t)
            (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
            (limit-up (save-excursion (outline-previous-heading)))
            (limit-down (save-excursion (outline-next-heading)))
            beg end)
        (save-excursion
          ;; Point is on a block when on BLOCK-BEGIN-RE or if
          ;; BLOCK-BEGIN-RE can be found before it...
          (and (or (org-in-regexp block-begin-re)
                   (re-search-backward block-begin-re limit-up :noerror))
               (setq beg (match-beginning 0))
               ;; ... and BLOCK-END-RE after it...
               (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                           (match-string-no-properties 1)
                                           "\\( .*\\)*$")))
                 (goto-char (match-end 0))
                 (re-search-forward block-end-re limit-down :noerror))
               (> (setq end (match-end 0)) pos)
               ;; ... without another BLOCK-BEGIN-RE in-between.
               (goto-char (match-beginning 0))
               (not (re-search-backward block-begin-re (1+ beg) :noerror))
               ;; Return value.
               (cons beg end))))))

  (defun modi/org-split-block ()
    "Sensibly split the current Org block at point."
    (interactive)
    (if (modi/org-in-any-block-p)
        (save-match-data
          (save-restriction
            (widen)
            (let ((case-fold-search t)
                  (at-bol (bolp))
                  block-start
                  block-end)
              (save-excursion
                (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
                (setq block-start (match-string-no-properties 0))
                (setq block-end (replace-regexp-in-string
                                 "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                                 (match-string-no-properties 1))))
              ;; Go to the end of current line, if not at the BOL
              (unless at-bol
                (end-of-line 1))
              (insert (concat (if at-bol "" "\n")
                              block-end
                              "\n\n"
                              block-start
                              (if at-bol "\n" "")))
              ;; Go to the line before the inserted "#+begin_ .." line
              (beginning-of-line (if at-bol -1 0)))))
      (message "Point is not in an Org block")))

  (defun modi/org-meta-return (&optional arg)
    "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item',
`org-table-wrap-region', or `modi/org-split-block' depending on
context.  When called with an argument, unconditionally call
`org-insert-heading'."
    (interactive "P")
    (org-check-before-invisible-edit 'insert)
    (or (run-hook-with-args-until-success 'org-metareturn-hook)
        (call-interactively (cond (arg #'org-insert-heading)
                                  ((org-at-table-p) #'org-table-wrap-region)
                                  ((org-in-item-p) #'org-insert-item)
                                  ((modi/org-in-any-block-p) #'modi/org-split-block)
                                  (t #'org-insert-heading)))))

  (advice-add 'org-meta-return :override #'modi/org-meta-return)
  )

(after! org-id
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(after! org
  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto))
        org-pretty-entities t
        ;; Based on https://lepisma.xyz/2017/10/28/ricing-org-mode/
        ;; org-ellipsis " ⌄ "
        )

  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  (setq org-cycle-separator-lines 0
        org-catch-invisible-edits 'show-and-error))

(add-hook 'org-mode-hook #'rainbow-delimiters-mode-disable)

(after! org
  (defun org-fold-get-fold-info-file-name ()
    (concat (buffer-file-name) ".fold"))

  (defun org-fold-save ()
    (save-excursion
      (goto-char (point-min))
      (let (foldstates)
        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))
        (while (not (eobp))
          (push (if (some (lambda (o) (overlay-get o 'invisible))
                          (overlays-at (line-end-position)))
                    t)
                foldstates)
          (outline-next-visible-heading 1))
        (with-temp-file (org-fold-get-fold-info-file-name)
            (prin1 (nreverse foldstates) (current-buffer))))))

  (defun org-fold-restore ()
    (save-excursion
      (goto-char (point-min))
      (let* ((foldfile (org-fold-get-fold-info-file-name))
               (foldstates
                (if (file-readable-p foldfile)
                      (with-temp-buffer
                        (insert-file-contents foldfile)
                        (read (current-buffer))))))
        (when foldstates
            (show-all)
          (goto-char (point-min))
          (unless (looking-at outline-regexp)
            (outline-next-visible-heading 1))
          (while (and foldstates (not (eobp)))
            (if (pop foldstates)
                  (hide-subtree))
            (outline-next-visible-heading 1))
          (message "Restored saved folding state")))))

  (add-hook 'org-mode-hook 'org-fold-activate 'append)

  (defun org-fold-activate ()
    (org-fold-restore)
    (add-hook 'kill-buffer-hook 'org-fold-kill-buffer nil t))

  (defun org-fold-kill-buffer ()
    ;; don't save folding info for unsaved buffers
    ;; (unless (buffer-modified-p)
    ;;   (org-fold-save))
    (org-fold-save)
    )
  )

(after! org
  (use-package! org-sidebar
    :bind (:map org-mode-map
            ("<f6>" . org-sidebar-toggle)
            ("<f7>" . org-sidebar-tree-toggle))
    :custom
    (org-sidebar-side 'left)
    (org-sidebar-tree-side 'left)))

(after! org
  (use-package! orly
    :defer-incrementally t))

(use-package! org-autolist
  :hook (org-mode . org-autolist-mode))

(after! org
  (defun org-add-completion-at-point ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point
              nil t))
  (add-hook 'org-mode-hook #'org-add-completion-at-point))

(after! org
  (defun akirak/org-set-created-timestamp (&rest args)
    "Add a creation timestamp to the current Org entry.
If the current command is run with a prefix argument, prevent
from running."
    (unless current-prefix-arg
      (org-set-property "CREATED_TIME"
                        (org-timestamp-format
                         (org-timestamp-from-time (current-time) t t)
                         (org-time-stamp-format t t)))))

  (advice-add #'org-insert-heading
              :after #'akirak/org-set-created-timestamp)
  (if (fboundp #'jit-disassemble)
      (defadvice org-journal-new-entry
          (after akirak/org-set-created-timestamp (&rest args) activate)
        (akirak/org-set-created-timestamp))
    (advice-add #'org-journal-new-entry
                :after
                #'akirak/org-set-created-timestamp))
  (add-hook 'org-capture-mode-hook #'akirak/org-set-created-timestamp))

(add-hook 'org-mode-hook #'turn-on-auto-capitalize-mode)

(after! org
  (use-package! org-noter
    :commands org-noter
    :config
    (setq org-noter-property-doc-file "INTERLEAVE_PDF"
          org-noter-property-note-location "INTERLEAVE_PAGE_NOTE")
    (setq org-noter-notes-window-location 'horizontal-split
          org-noter-always-create-frame nil
          org-noter-kill-frame-at-session-end nil
          org-noter-auto-save-last-location t))
  ;; (setq org-noter-default-notes-file-names
  ;;       '("elements.org" "Conceptual.org" "comprehension.org"))
  )

(after! org
  (use-package! lozenge
    :defer-incrementally t
    :config
    (lozenge-org-export-enable)
    (global-set-key (kbl-kbd "d" 'control 'meta nil 'super) #'lozenge-insert-lozenge)))

(after! org
  (defun my-company-auctex-prefix (regexp)
    "Returns the prefix for matching given REGEXP."
    (and (or (derived-mode-p 'latex-mode)
             (and (derived-mode-p 'org-mode)
                  (lattie--math-p)))
         (when (looking-back regexp)
           (match-string-no-properties 1))))

  (advice-add #'company-auctex-prefix :override #'my-company-auctex-prefix)

  (set-company-backend! 'org-mode 'company-auctex-symbols))

(after! org
  (add-to-list 'org-latex-packages-alist
               '("" "amsmath"))
  (add-to-list 'org-latex-packages-alist
               '("" "amssymb"))
  (add-to-list 'org-latex-packages-alist
               '("" "mathrsfs"))
  ;; Has \coloneqq
  (add-to-list 'org-latex-packages-alist
               '("" "mathtools"))
  (add-to-list 'org-latex-packages-alist
               '("" "tikz"))
  (add-to-list 'org-latex-packages-alist
               '("" "tikz-cd"))
  (add-to-list 'org-latex-packages-alist
               '("all" "xy")))

(after! org
  (setq org-pretty-entities-include-sub-superscripts nil))

(after! org
  (setq org-latex-pdf-process '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")
        org-preview-latex-default-process 'dvisvgm))

(add-hook 'org-mode-hook #'org-cdlatex-mode)

(add-transient-hook! 'org-cdlatex-mode-hook
  (load (expand-file-name "lattie" doom-private-dir))
  (map! :map org-cdlatex-mode-map
        "]" #'lattie-close-bracket
        "[" #'lattie-open-bracket
        "(" #'lattie-open-paren
        "n" #'special-lattie-down
        "-" #'special-lattie-punctuation
        "SPC" #'special-lattie-space
        "RET" #'special-lattie-newline-and-indent
        "^" #'special-lattie-underscore-caret
        "_" #'special-lattie-underscore-caret
        "<return>" #'special-lattie-newline-and-indent
        "." #'special-lattie-space
        "," #'special-lattie-space
        "+" #'lattie-insert-dollar
        "e" #'special-lattie-up
        "m" #'special-lattie-backward
        "i" #'special-lattie-forward
        "t" #'special-lattie-flow
        "c" #'special-lattie-toggle-latex-fragment
        "$" #'special-lattie-dollar
        "{" #'special-lattie-open-brace
        "}" #'special-lattie-close-brace
        "f" #'special-lattie-compile
        "`" #'special-lattie-grave
        "'" #'special-lattie-grave
        "0" #'special-lattie-digit-or-bol
        "1" #'special-lattie-digit
        ;; "^" #'special-lattie-back-to-heading
        "^" #'org-cdlatex-underscore-caret
        "2" #'special-lattie-digit
        "3" #'special-lattie-digit
        "4" #'special-lattie-digit
        "5" #'special-lattie-digit
        "6" #'special-lattie-digit
        "7" #'special-lattie-digit
        "8" #'special-lattie-digit
        "9" #'special-lattie-digit
        ;; "=" #'special-lattie-equals
        ;; "/" #'special-lattie-slash
        "DEL" #'lattie-delete-backward
        [remap backward-kill-word] #'backward-kill-word
        [remap org-self-insert-command] #'lattie-self-insert-command
        [remap self-insert-command] #'lattie-self-insert-command
        ";" #'lattie-self-insert-command
        "?" #'lattie-self-insert-command
        "=" #'lattie-self-insert-command
        ")" #'lattie-self-insert-command
        [tab] #'special-lattie-tab)
  (if (bound-and-true-p evil-org-mode)
      (map! :map evil-org-mode-map
            :i "<return>" #'special-lattie-newline-and-indent
            :i "RET" #'special-lattie-newline-and-indent)
    (add-transient-hook! 'evil-org-mode-hook
      (map! :map evil-org-mode-map
            :i "<return>" #'special-lattie-newline-and-indent
            :i "RET" #'special-lattie-newline-and-indent)))
  )

(after! org
  (defvar org-format-latex-header-old org-format-latex-header)
  (setq org-format-latex-header (string-join `(,org-format-latex-header-old
                                               "\\DeclareMathOperator{\\Hom}{Hom}"
                                               "\\DeclareMathOperator{\\sSet}{sSet}"
                                               "\\DeclareMathOperator{\\Ob}{Ob}"
                                               "\\DeclareMathOperator{\\Set}{Set}"
                                               "\\DeclareMathOperator{\\Sk}{Sk}"
                                               "\\newcommand{\\plus}{+}"
                                               ;; Colimit & Limit commands from
                                               ;; https://tex.stackexchange.com/questions/284059/new-command-for-filtered-colimits-and-limits
                                               "\\makeatletter"
                                               "\\newcommand{\\colim@}[2]{%"
                                               "  \\vtop{\\m@th\\ialign{##\\cr"
                                               "    \\hfil$#1\\operator@font colim$\\hfil\\cr"
                                               "    \\noalign{\\nointerlineskip\\kern1.5\\ex@}#2\\cr"
                                               "    \\noalign{\\nointerlineskip\\kern-\\ex@}\\cr}}%"
                                               "}"
                                               "\\newcommand{\\colim}{%"
                                               "  \\mathop{\\mathpalette\\colim@{\\rightarrowfill@\\scriptscriptstyle}}\\nmlimits@"
                                               "}"
                                               "\\renewcommand{\\varprojlim}{%"
                                               "  \\mathop{\\mathpalette\\varlim@{\\leftarrowfill@\\scriptscriptstyle}}\\nmlimits@"
                                               "}"
                                               "\\renewcommand{\\varinjlim}{%"
                                               "  \\mathop{\\mathpalette\\varlim@{\\rightarrowfill@\\scriptscriptstyle}}\\nmlimits@"
                                               "}"
                                               "\\makeatother"
                                               )
                                             "\n")))

(after! org
   (setq org-highlight-latex-and-related '(native script entities)))

(after! org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))

(use-package! counsel-org-capture-string
  :commands (counsel-org-capture-string)
  :config
  ;; (ivy-add-actions
  ;;  'counsel-org-capture-string
  ;;  '(("sd" akirak/web-search-firefox "Default search with Firefox")
  ;;    ("sg" akirak/surfraw/google "Google")
  ;;    ("sl" akiraksearch/lucky "I'm Feeling Lucky")
  ;;    ("ss" akirak/helm-search "Choose a search engine")))
  ;; (defun akirak/counsel-org-capture ()
  ;;   (require 'org-capture)
  ;;   (require 'counsel-org-capture-string)
  ;;   (ivy-read "Capture template: "
  ;;             #'counsel-org-capture-string--template-list
  ;;             :action (lambda (x)
  ;;                       (org-capture nil (car (split-string x))))))
  ;; (advice-add #'counsel-org-capture :override #'akirak/counsel-org-capture)
  )

(after! smtpmail
  (setq message-send-mail-function #'smtpmail-send-it
        send-mail-function #'smtpmail-send-it
        smtpmail-queue-dir "~/.mail/queued-mail/"
        smtpmail-debug-info t)

  (setq smtpmail-stream-type 'starttls
        smtpmail-starttls-credentials `((,user-mail-address 25 nil nil))
        smtpmail-default-smtp-server "smtp.office365.com"
        smtpmail-smtp-server "smtp.office365.com"
        smtpmail-smtp-service 25))

(set-email-account! "outlook"
                    `((mu4e-sent-folder . "/outlook/Sent Items")
                      (mu4e-drafts-folder . "/outlook/Drafts")
                      (mu4e-trash-folder . "/outlook/Trash")
                      (mu4e-refile-folder . "/outlook/Archive")
                      (smtpmail-smtp-user . ,user-mail-address)
                      (user-mail-address . ,user-mail-address))
                    t)

(map! :leader
      :desc "Mu4e" "am" #'=mu4e)

(use-package! mu4e-alert
  :defer-incrementally t
  :config
  (defun my-mu4e-alert-mode-line-formatter (mail-count)
    "default formatter used to get the string to be displayed in the mode-line.
mail-count is the count of mails for which the string is to displayed"
    (when (not (zerop mail-count))
      (if (zerop mail-count)
          ""
        (format " [%d] " mail-count))))

  (mu4e-alert-set-default-style 'libnotify)

  (setq mu4e-alert-modeline-formatter #'my-mu4e-alert-mode-line-formatter
        mu4e-alert-notify-repeated-mails t
        mu4e-alert-email-notification-types '(subjects)
        mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND maildir:/outlook/Inbox")

  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

(after! mu4e
  (setq mu4e-update-interval 60
        mu4e-get-mail-command "true")) ;; Do not fetch mail.

(after! mu4e
  (require 'mu4e-conversation)
  (setq mu4e-conversation-print-function 'mu4e-conversation-print-tree)
  (global-mu4e-conversation-mode +1))

(after! mu4e
  (setq mu4e-save-multiple-attachments-without-asking t
        mu4e-compose-dont-reply-to-self t
        mu4e-headers-fields '(;; (:account . 12)
                              (:human-date . 12)
                              (:flags . 4)
                              ;; (:size . 6)
                              (:mailing-list . 16)
                              (:from . 28)
                              (:subject)))


  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (setq mu4e-bookmarks
        `(,(make-mu4e-bookmark
            :name "Unread messages"
            :query "flag:unread AND NOT flag:trashed AND maildir:/outlook/Inbox"
            :key ?u)
          ,(make-mu4e-bookmark
            :name "Today's messages"
            :query "date:today..now AND NOT flag:trashed AND maildir:/outlook/Inbox"
            :key ?t)
          ,(make-mu4e-bookmark
            :name "Last 7 days"
            :query "date:7d..now AND NOT flag:trashed AND maildir:/outlook/Inbox"
            :key ?w)
          ,(make-mu4e-bookmark
            :name "Flagged"
            :query "maildir:/outlook/Inbox and flag:flagged"
            :key ?f)
          ,(make-mu4e-bookmark
            :name "Inbox"
            :query "maildir:/outlook/Inbox AND NOT flag:trashed"
            :key ?i)
          ,(make-mu4e-bookmark
            :name "Drafts"
            :query "flag:draft AND NOT flag:trashed"
            :key ?d))))

(after! mu4e
  (set-face-foreground 'mu4e-unread-face "yellow")
  (set-face-attribute 'mu4e-flagged-face nil :inherit 'font-lock-warning-face))

(after! mu4e
  (defun ambrevar/mu4e-mark-execute-all-no-confirm ()
    (interactive)
    (mu4e-mark-execute-all t)
    ;; (+mu4e--refresh-current-view-a)
    )
  (define-key mu4e-headers-mode-map "x" 'ambrevar/mu4e-mark-execute-all-no-confirm)

  (when (featurep! :editor evil)
    (map! :map mu4e-headers-mode-map
          :n "x" #'ambrevar/mu4e-mark-execute-all-no-confirm)))

(after! mu4e
  (when (require 'org-mu4e nil t)
    (dolist (map (list mu4e-view-mode-map mu4e-headers-mode-map))
      ;; Org mode has "C-c C-t" for 'org-todo.
      (define-key map (kbd "C-c C-t") 'org-mu4e-store-and-capture))
    (setq org-mu4e-link-query-in-headers-mode nil)

    (defun ambrevar/org-mail-date (&optional msg)
      (with-current-buffer (mu4e-get-headers-buffer)
        (mu4e-message-field (or msg (mu4e-message-at-point)) :date)))

    (add-to-list 'org-capture-templates
                 `("e" "Mark e-mail in agenda" entry ,(list 'file
                                                            (concat org-directory "email.org"))
                   "* TODO %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"++1d\" nil (ambrevar/org-mail-date)))\n%a\n"))))

(after! mu4e
  (defun mu4e-action-save-to-pdf (msg)
    (let* ((date (mu4e-message-field msg :date))
           (infile (mu4e~write-body-to-html msg))
           (outfile (format-time-string "%Y-%m-%d%H%M%S.pdf" date)))
      (with-temp-buffer
        (shell-command
         (format "wkhtmltopdf %s ~/downloads/%s" infile outfile) t))))

  (add-to-list 'mu4e-view-actions '("Save to PDF" . mu4e-action-save-to-pdf) t)
  (setq mu4e-view-actions (remove '("view as pdf" . mu4e-action-view-as-pdf)
                                  mu4e-view-actions)))

(after! dired
  (require 'gnus-dired)
  (after! gnus-dired
    ;; make the `gnus-dired-mail-buffers' function also work on
    ;; message-mode derived modes, such as mu4e-compose-mode
    (defun gnus-dired-mail-buffers ()
      "return a list of active message buffers."
      (let (buffers)
        (save-current-buffer
          (dolist (buffer (buffer-list t))
            (set-buffer buffer)
            (when (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
              (push (buffer-name buffer) buffers))))
        (nreverse buffers)))
    (setq gnus-dired-mail-mode #'mu4e-user-agent)
    (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)))

(use-package! bbdb
  :defer-incrementally t
  :config
  (require 'bbdb-message)
  (after! gnus
    (require 'ebdb-gnus))
  ;; (after! mu4e
  ;;   (require 'ebdb-mu4e))
  (bbdb-initialize 'gnus 'message))

(use-package! bbdb-vcard
  :commands (bbdb-vcard-import-file bbdb-vcard-export))

(use-package! org-preview-html
  :commands (org-preview-html/preview
             org-preview-html-mode))

(after! mu4e
  (require 'helm-mu nil t)
  (add-hook! (mu4e-headers-mode
              mu4e-main-mode
              mu4e-view-mode)
    (map! :map (mu4e-headers-mode-map mu4e-main-mode-map mu4e-view-mode-map)
          :ne "s" #'helm-mu)))

(after! vterm
  (setq vterm-shell "/run/current-system/sw/bin/zsh"))

(after! esh-module
  (setq eshell-module-list
        (delq 'eshell-banner eshell-modules-list)))

(after! eshell
  (require 'alert)

  (defun eshell-command-alert (process status)
    "Send `alert' with severity based on STATUS when PROCESS finished."
    (let* ((cmd (process-command process))
           (buffer (process-buffer process))
           (msg (format "%s: %s" (mapconcat 'identity cmd " ")  status)))
      (if (string-prefix-p "finished" status)
          (alert msg :buffer buffer :severity  'normal)
        (alert msg :buffer buffer :severity 'urgent))))

  (add-hook 'eshell-kill-hook #'eshell-command-alert)

  (alert-add-rule :status   '(buried)   ;only send alert when buffer not visible
                  :mode     'eshell-mode
                  :style 'notifications))

(use-package! pdf-tools
  :defer-incrementally t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq pdf-misc-print-programm "/run/current-system/sw/bin/lpr")
  (setq pdf-misc-print-programm-args '("-P" "Brother_HL-L2320D_series"
                                       "-o" "media=letter"))
  (delq 'pdf-misc-size-indication-minor-mode pdf-tools-enabled-modes)
  (delq 'pdf-misc-context-menu-minor-mode pdf-tools-enabled-modes)
  (delq 'pdf-misc-menu-bar-minor-mode pdf-tools-enabled-modes)

  (when (featurep! :editor evil)
    (map! :map pdf-view-mode-map
          :n "J" #'pdf-view-next-page
          :n "K" #'pdf-view-previous-page)))

(after! proof-splash
  (setq proof-splash-enable nil))
(after! proof-useropts
  (setq proof-toolbar-enable nil))

(after! pg-custom
  (setq coq-use-holes nil))
(after! coq-db
  (setq coq-holes-minor-mode nil))
(advice-add #'coq-build-abbrev-table-from-db :override #'ignore)

(after! company-coq
  (setq company-coq-live-on-the-edge t
        company-coq-disabled-features '(hello)))

(when (featurep! :editor lispy)
  (autoload 'cider-start-map "cider" "CIDER jack-in and connect keymap." t 'keymap))
