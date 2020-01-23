;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq inhibit-compacting-font-caches t)

(after! font-utils
  (setq font-utils-use-memory-cache t))

(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 17)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 19)
      doom-serif-font (font-spec :family "DejaVu Serif" :size 19))

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

(require 'epa-file)
(epa-file-enable)

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
  (after! direnv
    (advice-remove #'shell-command #'+direnv-update-async-shell-command-a)
    (defadvice shell-command (before +direnv-update-async-shell-command-a (command &optional output-buffer _error-buffer)
                                     activate)
      (+direnv-update-async-shell-command-a command output-buffer _error-buffer))))

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
            (setq l (cons (car l) (cdr l))))
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

(defun my-translate-keys-p (key-from)
  "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
  (and
   ;; Only allow a non identity translation if we're beginning a Key Sequence.
   (equal key-from (this-command-keys))
   (not isearch-mode)
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
      (setq unread-input-method-events (string-to-list s)))))

(add-hook 'post-self-insert-hook #'correct-symbol-ngram)

(defun swap-semicolon-colon ()
  (when (or (and (memq major-mode
                       '(agda2-mode
                         haskell-mode
                         ))
                 (eq (length (this-command-keys-vector))
                     1))
            (and (looking-back "https?" (point-at-bol) nil)
                 (eq (length (this-command-keys-vector))
                     1))
            )
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

(defun correct-shifted-char ()
  (let (start end)
    (let ((s (save-excursion
               (buffer-substring-no-properties
                (progn (search-backward-regexp "[^a-zA-Z]" nil t)
                       (forward-char 1)
                       (setq start (point)))
                (progn (if (search-forward-regexp "[^a-zA-Z]" nil t)
                           (forward-char -1)
                         (end-of-line))
                       (setq end (point)))))))
      ;; (message "%s %s %s %s %s" s
      ;;          (and (> (length s) 3) (memq (aref s 1) consonants))
      ;;          start end
      ;;          (and (> (length s) 3)
      ;;               (concat (upcase (substring-no-properties s 1 2))
      ;;                       (substring-no-properties s 2 nil))))
      (when (and (> (length s) 3)
                 (eq (aref s 0) ?n)
                 (memq (aref s 1) consonants)
                 (not (string-prefix-p "ngin" s))
                 (not (string-prefix-p "nlab" s))
                 (or (memq (aref s 2) vowels)
                     (member (substring-no-properties s 1 3)
                             common-starting-consonant-bigrams)))
        (ignore-errors (delete-region start end)
                       (insert (concat (upcase (substring-no-properties s 1 2))
                                       (substring-no-properties s 2 nil))))))))

(add-hook 'post-self-insert-hook #'correct-shifted-char)

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
          my/he-try-expand-flx
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(global-set-key (kbd "M-/") #'hippie-expand)

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
                    (list (cons (string-to-char (kbl-print-reverse "v")) #'View-scroll-half-page-forward)
                          (cons (string-to-char (kbl-print-reverse "u")) #'View-scroll-half-page-backward))
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

(defun crux-kill-line-backwards ()
  "Kill line backwards and adjust the indentation."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(if (display-graphic-p)
    (global-set-key (kbd "<C-backspace>") #'crux-kill-line-backwards)
  (global-set-key (kbd "C-DEL") #'crux-kill-line-backwards))

(global-set-key (kbl-kbd "u" nil nil nil 'super) #'previous-buffer)
(global-set-key (kbl-kbd "i" nil nil nil 'super) #'next-buffer)

(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)

(global-set-key (kbl-kbd "b" 'control 'meta nil 'super) #'bookmark-jump)

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

(when (display-graphic-p)
  (require 'exwm-workspace)
  (require 'exwm-xim)
  (exwm-xim-enable)
  (require 'exwm)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist
        '(2 "HDMI-0" 1 "DP-5"  0 "DP-3")
        exwm-workspace-number 3)
  (exwm-randr-enable)
  (exwm-enable)

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
               ?\M-\S-1))
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
  (call-process-shell-command
   (string-join '("docker" "run" "--name" "etesync-dav" "-d" "-v"
                  "etesync-dav:/data" "-p" "37358:37358"
                  "-p" "37359:37359" "--restart=always" "etesync/etesync-dav")
                " ")
   nil "*etesync-dav*")

  (defun discord-start ()
    (interactive)
    (defvar discord-process nil)
    (setq discord-process
          (async-start-process "Discord" "Discord" nil))
    (require 'elcord)
    (elcord-mode +1))

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
     (call-process-shell-command "taskset 0x6 qutebrowser" nil 0)))

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
                (exwm-workspace-rename-buffer exwm-class-name)))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

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
  (make-string (max (- (* 3 (length (or (bound-and-true-p exwm-systemtray--list) ())))
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

(require 'aggressive-indent)
(global-aggressive-indent-mode +1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode 'nix-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line)))))

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
  ;; (add-to-list 'super-save-triggers #'ace-window)
  (setq super-save-triggers nil
        super-save-auto-save-when-idle t)
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
  (setq eldoc-idle-delay 1)
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

(when (featurep! :tools flyspell +aspell)
  (after! ispell
    (setq ispell-quietly nil
          ispell-dictionary "en_us"
          ispell-complete-word-dict "~/.doom.d/dict/english-words.txt"))
  (after! flyspell
    (setq flyspell-issue-message-flag t
          flyspell-abbrev-p t)))

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
  (emms-all))

(after! emms
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
     "%n")))

(after! emms
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
  (evil-set-initial-state 'w3m-mode 'emacs))

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

(use-package! gnus
  :bind (:map doom-leader-map
          ("an" . gnus))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "gnus") . (nil . "News"))))
  :config
  (require 'smtpmail)
  (require 'gnus)
  (require 'gnus-msg)
  (require 'gnus-score)
  (require 'gnus-start)
  (require 'gnus-async))

(after! gnus
  (setq gnus-asynchronous t
        gnus-plugged nil)
  (setq gnus-use-full-window nil
        gnus-inhibit-startup-message t
        gnus-add-to-list t
        gnus-always-read-dribble-file t
        gnus-interactive-exit nil
        gnus-save-newsrc-file nil
        gnus-inhibit-user-auto-expire t
        gnus-use-scoring t
                                        ; gnus-use-trees nil
        gnus-summary-default-score 0
        gnus-summary-expunge-below -256
        gnus-summary-make-false-root 'dummy
        gnus-suppress-duplicates t
        gnus-score-expiry-days nil
        gnus-fetch-old-headers t
        gnus-home-score-file "~/.mail/gnus.score"
        gnus-agent-directory "~/.mail/agent/"
        gnus-directory "~/.mail/news"
        gnus-article-save-directory "~/.mail/news"
        gnus-cache-dictory "~/.mail/news/cache"
        gnus-cache-active-file "~/.mail/news/cache/active"
        gnus-kill-files-directory "~/.mail/news"
        nndraft-directory "~/.mail/drafts/"
        gnus-default-article-saver 'gnus-summary-save-in-mail
        gnus-save-killed-list nil
                                        ;gnus-auto-expirable-newsgroups "gmane.*"
        gnus-ignored-mime-types '("text/x-gnus")
        ;; vcard-ignored-from-addresses
        gnus-show-all-headers nil
        gnus-treat-capitalize-sentences nil
        gnus-treat-display-picons nil ;not bound?
        gnus-treat-display-smileys nil
        gnus-treat-display-x-face t
        gnus-treat-emphasize nil
        gnus-treat-fill-long-lines nil
        gnus-treat-hide-signature nil
                                        ;qgnus-treat-hide-citation t
        gnus-treat-overstrike nil
        gnus-treat-play-sounds nil ;not bound?
        gnus-treat-strip-banner nil
        gnus-treat-strip-cr t
        gnus-treat-strip-leading-blank-lines nil
        gnus-treat-strip-multiple-blank-lines nil
        gnus-treat-strip-pem nil
        gnus-treat-strip-trailing-blank-lines nil
        gnus-treat-translate nil ;not bound?
        )
  (setq gnus-inhibit-images nil
        ;; mm-discouraged-alternatives '("text/html" "text/richtext")
        )

  ;; Show the article headers in this order.
  (setq gnus-sorted-header-list
        '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
          "^Subject:" "^Date:" "^Gnus"))

  (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")

  (setq ;gnus-sorted-header-list gnus-visible-headers
   gnus-extra-headers
   '(To Cc Keywords Gcc Newsgroups X-Spam-Flag)
   gnus-extra-headers
   nnmail-extra-headers)
  (setq message-generate-headers-first t
        message-insert-canlock nil
        message-wash-forwarded-subjects t
        message-make-forward-subject-function #'message-forward-subject-fwd
        message-use-mail-followup-to 'use
        message-subscribed-address-functions '(gnus-find-subscribed-addresses))
  ;; (setq mail-source-delete-incoming t)

  (setq nnmail-split-methods 'nnmail-split-fancy
        nnmail-split-header-length-limit 4096
        nnmail-use-long-file-names t
        nnmail-crosspost nil)

  ;; (setq gnus-select-method '(nntp "news.gwene.org")) ;; Read feeds/atom through gwene
  (setq gnus-select-method
        '(nntp "news.gmane.org"
               (nntp-open-connection-function nntp-open-plain-stream)))
  (setq gnus-secondary-select-methods '((nntp "nntp.aioe.org")
                                        (nnmaildir "outlook"
                                                   (directory "~/.mail/outlook"))
                                        (nntp "news.eternal-september.org")
                                        (nntp "news.gwene.org")
                                        ;; (nnreddit "")
                                        ;; (nnml "")
                                        ))

  (setq gnus-message-archive-method
        '(nnfolder "archive"
                   (nnfolder-directory    "~/.mail/outlook/Archive")
                   (nnfolder-active-file  "~/.mail/outlook/Archive/active")
                   (nnfolder-get-new-mail nil)))

  ;; Crypt-foo
  (setq gnus-message-replysign t
        gnus-message-replyencrypt t
        mm-verify-option 'always
        mm-decrypt-option 'always)

  ;; (define-key message-minibuffer-local-map [(tab)] 'bbdb-complete-name)

  ;; Buttonize the different parts, please
  (setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))

  ;; But keep buttons for multiple parts
  (setq gnus-inhibit-mime-unbuttonizing t)

  ;; ask encryption password once
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)

  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))

                                        ; NO 'passive
  (setq gnus-use-cache t)

  ;; press "o" to view all groups
  (defun my-gnus-group-list-subscribed-groups ()
    "List all subscribed groups with or without un-read messages"
    (interactive)
    (gnus-group-list-all-groups 5))

  (define-key gnus-group-mode-map
    ;; list all the subscribed groups even they contain zero un-read messages
    (kbd "o") 'my-gnus-group-list-subscribed-groups)

  ;; Fetch only part of the article if we can.
  ;; I saw this in someone's .gnus
  (setq gnus-read-active-file 'some)

  ;; open attachment
  (eval-after-load 'mailcap
    '(progn
       (cond
        ;; on macOS, maybe change mailcap-mime-data?
        ((eq system-type 'darwin))
        ;; on Windows, maybe change mailcap-mime-data?
        ((eq system-type 'windows-nt))
        (t
         ;; Linux, read ~/.mailcap
         (mailcap-parse-mailcaps)))))

  ;; Tree view for groups.
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  ;; Threads!  I hate reading un-threaded email -- especially mailing
  ;; lists.  This helps a ton!
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

  ;; Also, I prefer to see only the top level message.  If a message has
  ;; several replies or is part of a thread, only show the first message.
  ;; `gnus-thread-ignore-subject' will ignore the subject and
  ;; look at 'In-Reply-To:' and 'References:' headers.
  (setq gnus-thread-hide-subtree t)
  (setq gnus-thread-ignore-subject t)

  ;; Read HTML mail:
  ;; You need install the command line web browser 'w3m' and Emacs plugin 'w3m'
  ;; manually. It specify the html render as w3m so my setup works on all versions
  ;; of Emacs.
  ;;
  ;; Since Emacs 24+, a default html rendering engine `shr' is provided:
  ;;   - It works out of box without any cli program dependency or setup
  ;;   - It can render html color
  ;; So below line is optional.
  (setq mm-text-html-renderer 'w3m) ; OPTIONAL

  ;; Send email through SMTP
  (setq message-send-mail-function #'smtpmail-send-it
        send-mail-function #'smtpmail-send-it
        smtpmail-queue-dir "~/.mail/queued-mail/"
        smtpmail-debug-info t)

  (setq smtpmail-stream-type 'starttls
        smtpmail-starttls-credentials `((,user-mail-address 25 nil nil))
        smtpmail-default-smtp-server "smtp.office365.com"
        smtpmail-smtp-server "smtp.office365.com"
        smtpmail-smtp-service 25)

  ;; http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
  (setq gnus-use-correct-string-widths nil)

  ;; Sample on how to organize mail folders.
  ;; It's dependent on `gnus-topic-mode'.
  ;; (eval-after-load 'gnus-topic
  ;;   '(progn
  ;;      (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
  ;;      (setq gnus-server-alist '(("Archive" nnfolder "Archive"
  ;;                                 (nnfolder-directory "~/.mail/Archive")
  ;;                                 (nnfolder-active-file "~/.mail/Archive/active")
  ;;                                 (nnfolder-get-new-mail nil)
  ;;                                 (nnfolder-inhibit-expiry t))))

  ;;      ;; "Gnus" is the root folder, and there are three mail accounts, "misc", "hotmail", "gmail"
  ;;      (setq gnus-topic-topology '(("Gnus" visible)
  ;;                                  ;; (("misc" visible))
  ;;                                  (("outlook" visible nil nil))))

  ;;      ;; each topic corresponds to a public imap folder
  ;;      (setq gnus-topic-alist '(("outlook" ; the key of topic
  ;;                                "nnimap+outlook:Inbox"
  ;;                                "nnimap+outlook:Drafts"
  ;;                                "nnimap+outlook:Sent"
  ;;                                ;;"nnimap+outlook:Junk"
  ;;                                ;;"nnimap+outlook:Deleted"
  ;;                                )
  ;;                               ;; ("misc" ; the key of topic
  ;;                               ;;  "nnfolder+archive:sent.2018"
  ;;                               ;;  "nnfolder+archive:sent.2019"
  ;;                               ;;  "nndraft:drafts")
  ;;                               ("Gnus")))))

  ;; eye candy
  (with-eval-after-load "gnus"
    (copy-face 'font-lock-variable-name-face 'gnus-face-6)
    (setq gnus-face-6 'gnus-face-6)
    (copy-face 'font-lock-constant-face 'gnus-face-7)
    (setq gnus-face-7 'gnus-face-7)
    (copy-face 'gnus-face-7 'gnus-summary-normal-unread)
    (copy-face 'font-lock-constant-face 'gnus-face-8)
    (set-face-foreground 'gnus-face-8 "gray50")
    (setq gnus-face-8 'gnus-face-8)
    (copy-face 'font-lock-constant-face 'gnus-face-9)
    (set-face-foreground 'gnus-face-9 "gray70")
    (setq gnus-face-9 'gnus-face-9)
    (setq gnus-summary-make-false-root 'dummy)
    (setq gnus-summary-make-false-root-always nil)

    (defun oxy-unicode-threads ()
      (interactive)
      (setq gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
            gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %s\n"
            gnus-sum-thread-tree-indent " "
            gnus-sum-thread-tree-root "■ "
            gnus-sum-thread-tree-false-root "□ "
            gnus-sum-thread-tree-single-indent "▣ "
            gnus-sum-thread-tree-leaf-with-other "├─▶ "
            gnus-sum-thread-tree-vertical "│"
            gnus-sum-thread-tree-single-leaf "└─▶ "))

    (defun oxy-unicode-threads-heavy ()
      (interactive)
      (setq gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
            gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
            gnus-sum-thread-tree-indent " "
            gnus-sum-thread-tree-root "┏● "
            gnus-sum-thread-tree-false-root " ○ "
            gnus-sum-thread-tree-single-indent " ● "
            gnus-sum-thread-tree-leaf-with-other "┣━━❯ "
            gnus-sum-thread-tree-vertical "┃"
            gnus-sum-thread-tree-single-leaf "┗━━❯ "))

    (oxy-unicode-threads-heavy)))

(use-package! mentor
  :defer-incrementally t
  :bind (:map doom-leader-map
          ("at" . mentor))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist '((nil . "mentor") . (nil . "rTorrent"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'mentor-mode 'emacs)))

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
  (setq forecast-language 'en))

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
          ("as" . spray-mode))
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
  (add-to-list 'md4rd-subs-active 'nixos :append))

(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("\\`SPC a g\\'" . nil) . (nil . "games"))))

(use-package! tetris
  :defer-incrementally t
  :commands tetris
  :bind (:map doom-leader-map
          ("agt" . tetris))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "tetris") . (nil . "Tetris")))))

(use-package! doctor
  :defer-incrementally t
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
  :defer-incrementally t
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
  :defer-incrementally t
  :commands pong
  :bind (:map doom-leader-map
          ("agp" . pong))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "pong") . (nil . "Pong")))))

(use-package! snake
  :defer-incrementally t
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
  :defer-incrementally t
  :commands dunnet
  :bind (:map doom-leader-map
          ("agd" . dunnet))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "dunnet") . (nil . "Dunnet")))))

(use-package! 2048-game
  :defer-incrementally t
  :commands 2048-game
  :bind (:map doom-leader-map
          ("ag2" . 2048-game))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "2048-game") . (nil . "2048")))))

(use-package! gomoku
  :defer-incrementally t
  :commands gomoku
  :bind (:map doom-leader-map
          ("ag%" . gomoku))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "gomoku") . (nil . "5-in-a-row")))))

(use-package! 5x5
  :defer-incrementally t
  :commands 5x5
  :bind (:map doom-leader-map
          ("ag5" . 5x5)))

(use-package! minesweeper
  :defer-incrementally t
  :commands minesweeper
  :bind (:map doom-leader-map
          ("agm" . minesweeper))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "minesweeper") . (nil . "Minesweeper")))))

(use-package! gnugo
  :defer-incrementally t
  :commands gnugo
  :bind (:map doom-leader-map
          ("agg" . gnugo))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "gnugo") . (nil . "go")))))

(use-package! mpuz
  :defer-incrementally t
  :commands mpuz
  :bind (:map doom-leader-map
          ("agx" . mpuz))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "mpuz") . (nil . "Multiplication puzzle")))))

(use-package! bubbles
  :defer-incrementally t
  :commands bubbles
  :bind (:map doom-leader-map
          ("agb" . bubbles))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "bubbles") . (nil . "Bubbles")))))

(use-package! key-quiz
  :defer-incrementally t
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
  :defer-incrementally t
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
  :defer-incrementally t
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
  :defer-incrementally t
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
  :defer-incrementally t
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
  :defer-incrementally t
  :commands dissociated-press
  :bind (:map doom-leader-map
          ("aed" . dissociated-press))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "dissociated-press") . (nil . "Dissociated press")))))

(use-package! life
  :defer-incrementally t
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
  :defer-incrementally t
  :commands zone
  :bind (:map doom-leader-map
          ("aez" . zone))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "zone") . (nil . "Zone")))))

(use-package! fireplace
  :defer-incrementally t
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

(after! popup
  (set-popup-rules! '(("^\\*Agda information" :size 0.3 :focus nil)
                      ("\\*intero:global-project::repl" :size 0.3 :focus t)
                      ("\\*haskell-process-log" :size 0.3 :focus nil :quit t)
                      ("\\*test\\*" :size 0.3 :focus t)
                      ("\\*Compile-Log\\*" :size 0.3 :focus nil :quit t)
                      ("^\\*Org Agenda\\*" :size 0.5 :side bottom)
                      ("^\\*eww\\*" :ignore t)
                      ("^\\*cfw:details\\*" :size 0.35)
                      ("Helm systemd" :ignore t))))

(when (featurep! :editor evil)
  (after! evil-snipe
    (setq evil-snipe-scope 'line
          evil-snipe-spillover-scope 'visible)))

(add-hook 'evil-insert-state-exit-hook #'expand-abbrev)

(use-package! company-posframe
  :if (posframe-workable-p)
  :hook (company-mode . company-posframe-mode)
  :config
  (setq company-posframe-quickhelp-delay (when (boundp 'company-quickhelp-delay)
                                           company-quickhelp-delay)))

(after! company
  (setq company-idle-delay 1))

(use-package! company-quickhelp
  :after company
  :custom
  (company-quickhelp-margin 15)
  (company-quickhelp-delay nil)
  :hook (company-mode . company-quickhelp-local-mode))

(when (featurep! :completion ivy)
  (advice-add #'helm-mode :around #'ignore)
  (after! org
    (map! :map org-mode-map
          :localleader
          "." #'counsel-org-goto
          "/" #'counsel-org-goto-all)))

(after! ivy
  (setq +ivy-buffer-preview t))

(after! ivy
  (define-key ivy-minibuffer-map (kbl-kbd "j" 'control nil 'shift nil) #'ivy-immediate-done))

(add-hook! 'exwm-init-hook
  (after! ivy-posframe
    (add-to-list 'ivy-posframe-parameters '(parent-frame . nil))))

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
    (require 'calfw-ical)
    (require 'calfw-org)
    (cfw:open-calendar-buffer
     ;; :custom-map cfw:my-cal-map
     :contents-sources
     (list
      (cfw:org-create-source "Green")   ; orgmode source
      ;; (cfw:howm-create-source "Blue")  ; howm source
      ;; (cfw:cal-create-source "Orange")  ; diary source
      ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
      (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/luneth1314%40gmail.com/private-a4a90eade91c03181cd1266949a6ede2/basic.ics" "IndianRed") ; google calendar ICS
      )))

  (setq +calendar-open-function #'my-open-calendar))

(after! org
  (after! ox-icalendar
    (setq org-icalendar-alarm-time 40
          org-icalendar-include-todo t
          org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo event-if-todo)
          org-icalendar-store-UID t))

  (after! org-caldav
    (load (expand-file-name "caldav" doom-private-dir))
    (setq org-caldav-files '("~/org/appointments.org" "~/org/email.org"))
    (when (boundp 'org-caldav-sync-todo)
      (setq org-caldav-sync-todo t)))

  (defvar org-caldav-sync-timer nil
    "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")

  (defun org-caldav-sync-with-delay (secs)
    (when org-caldav-sync-timer
      (cancel-timer org-caldav-sync-timer))
    (setq org-caldav-sync-timer
          (run-with-idle-timer
           (* 1 secs) nil 'org-caldav-sync)))

  (add-hook! 'after-save-hook
    (require 'ox-icalendar)
    (require 'org-caldav)
    (when (and (eq major-mode 'org-mode)
               (member (buffer-file-name)
                       org-caldav-files))
      (org-caldav-sync-with-delay 300)))

  ;; (add-hook 'kill-emacs-hook #'org-caldav-sync)
  )

(defun org-caldav-do-not-display-when-boring-advice (oldfun &rest args)
  (when org-caldav-sync-result
    (apply oldfun args)))

(advice-add #'org-caldav-display-sync-results :around #'org-caldav-do-not-display-when-boring-advice)

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

(after! org
  (require 'secretaria)
  (secretaria-unknown-time-always-remind-me))

(after! org
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
    (aj/org-agenda-to-appt))

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
                 "* %?\nSCHEDULED: %^T\n%a\n")))

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
    (setq org-crypt-disable-auto-save t)))

(after! org
  (setq org-log-done t)
  (setq org-log-done-with-time t))

(defvar +org-exit-src-code-hook nil
  "Hook run just before exiting a org source block buffer.")

(defun +org|run-exit-src-code-hooks (&rest _)
  "Runs all hooks in `+org-exit-src-code-hook`."
  (run-hooks '+org-exit-src-code-hook))

(advice-add #'org-edit-src-exit :before #'+org|run-exit-src-code-hooks)

(add-hook '+org-exit-src-code-hook #'ws-butler-trim-eob-lines)

(use-package! company-org-block
  :when (featurep! :completion company)
  :after org
  :functions (company-org-block)
  :config
  (set-company-backend! 'org-mode 'company-org-block))

(after! org
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (C . t)
                                   (R . t)
                                   (shell . t)
                                   (python . t)
                                   (lilypond . t)))
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

(after! org-bullets
  (setq org-bullets-bullet-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ"))
  (remove-hook 'org-mode-hook #'org-bullets-mode))

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

(use-package! org-sidebar :after org
              :custom
              (org-sidebar-side 'left)
              (org-sidebar-tree-side 'left)
              :config
              (map! :map org-mode-map
                    "<f6>" #'org-sidebar-toggle
                    "<f7>" #'org-sidebar-tree-toggle))

(use-package! orly :after org)

(use-package! org-autolist :after org
              :init
              (add-hook 'org-mode-hook #'org-autolist-mode))

(use-package! org-bookmark-heading :after org
  :custom
  (org-bookmark-heading-filename-fn
   (defun akirak/org-bookmark-heading-filename (path)
     (let* ((path (expand-file-name path))
            (project (project-current))
            (dir (abbreviate-file-name (file-name-directory path)))
            (filename (file-name-nondirectory path))
            (root (car-safe (project-roots project))))
       (if root
           (f-relative path (f-parent root))
         path))))
  (org-bookmark-heading-name-fn
   (defun akirak/org-bookmark-heading (path heading)
     (let ((ancestors (org-get-outline-path)))
       (format "\"%s\" in %s%s"
               (substring-no-properties
                (org-link-display-format heading))
               (akirak/org-bookmark-heading-filename path)
               (if ancestors
                   (substring-no-properties
                    (concat ":" (org-format-outline-path
                                 (mapcar #'org-link-display-format ancestors)
                                 nil nil "/")))
                 ""))))))

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
  (add-hook 'org-capture-mode-hook #'akirak/org-set-created-timestamp))

(after! org
  (use-package! org-noter
    :commands org-noter
    :config
    (setq org-noter-property-doc-file "INTERLEAVE_PDF"
          org-noter-property-note-location "INTERLEAVE_PAGE_NOTE")
    (setq org-noter-notes-window-location 'horizontal-split
          org-noter-always-create-frame nil
          org-noter-kill-frame-at-session-end nil))
  ;; (setq org-noter-default-notes-file-names
  ;;       '("elements.org" "Conceptual.org" "comprehension.org"))
  )

(use-package! lozenge
  :after org
  :config
  (lozenge-org-export-enable)
  (global-set-key (kbl-kbd "d" 'control 'meta nil 'super) #'lozenge-insert-lozenge))

;; http://kitchingroup.cheme.cmu.edu/blog/2015/10/09/Automatic-latex-image-toggling-when-cursor-is-on-a-fragment/
;; when the point is on a latex image, toggle it to latex code
;; else display the corresponding latex image

;;; bokwoon additions
;; According to the comments, as of the latest org-mode you have to replace all 'org-latex-fragment-image-overlays' with (org--list-latex-overlays)
;; However the function (org--list-latex-overlays) seems to have been removed (at least in my version of emacs) so I found a webpage online mentioning this function declaration (https://code.orgmode.org/bzg/org-mode/commit/cadfbbe8af9b978a02f48ee70bf6c855fdd3e19d) and copied it into this file

;; 2 alternative scripts
;; http://slumpy.org/blog/2017-02-01-automatic-latex-preview-in-org-mode/
;; https://gist.github.com/cvcore/760008a4dfb2eadf42afdc9cf01ef979

;; This function definition was obtained from https://code.orgmode.org/bzg/org-mode/commit/cadfbbe8af9b978a02f48ee70bf6c855fdd3e19d
;; The script needs this function to work
(after! org
  (defun org--list-latex-overlays (&optional beg end)
    "List all Org LaTeX overlays in current buffer.
Limit to overlays between BEG and END when those are provided."
    (org-remove-if-not
     (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
     (overlays-in (or beg (point-min)) (or end (point-max)))))

  (defvar org-latex-fragment-last nil
    "Holds last fragment/environment you were on.")

  (defun org-latex-fragment-toggle ()
    "Toggle a latex fragment image "
    (interactive)
    (and (eq 'org-mode major-mode)
         (let* ((el (org-element-context))
                (el-type (car el)))
           (cond
            ;; were on a fragment and now on a new fragment
            ((and
              ;; fragment we were on
              org-latex-fragment-last
              ;; and are on a fragment now
              (or
               (eq 'latex-fragment el-type)
               (eq 'latex-environment el-type))
              ;; but not on the last one this is a little tricky. as you edit the
              ;; fragment, it is not equal to the last one. We use the begin
              ;; property which is less likely to change for the comparison.
              (not (= (org-element-property :begin el)
                      (org-element-property :begin org-latex-fragment-last))))
             ;; go back to last one and put image back
             (save-excursion
               (goto-char (org-element-property :begin org-latex-fragment-last))
               (org-preview-latex-fragment))
             ;; now remove current image
             (goto-char (org-element-property :begin el))
             (let ((ov (cl-loop for ov in (org--list-latex-overlays)
                                if
                                (and
                                 (<= (overlay-start ov) (point))
                                 (>= (overlay-end ov) (point)))
                                return ov)))
               (when ov
                 (delete-overlay ov)))
             ;; and save new fragment
             (setq org-latex-fragment-last el))

            ;; were on a fragment and now are not on a fragment
            ((and
              ;; not on a fragment now
              (not (or
                    (eq 'latex-fragment el-type)
                    (eq 'latex-environment el-type)))
              ;; but we were on one
              org-latex-fragment-last)
             ;; put image back on
             (save-excursion
               (goto-char (org-element-property :begin org-latex-fragment-last))
               (org-preview-latex-fragment))
             ;; unset last fragment
             (setq org-latex-fragment-last nil))

            ;; were not on a fragment, and now are
            ((and
              ;; we were not one one
              (not org-latex-fragment-last)
              ;; but now we are
              (or
               (eq 'latex-fragment el-type)
               (eq 'latex-environment el-type)))
             (goto-char (org-element-property :begin el))
             ;; remove image
             (let ((ov (cl-loop for ov in (org--list-latex-overlays)
                                if
                                (and
                                 (<= (overlay-start ov) (point))
                                 (>= (overlay-end ov) (point)))
                                return ov)))
               (when ov
                 (delete-overlay ov)))
             (setq org-latex-fragment-last el))))))


  (add-hook 'post-command-hook 'org-latex-fragment-toggle)

  ;; (add-hook 'org-mode-hook
  ;; 	  (add-hook 'post-command-hook 'org-latex-fragment-toggle))

  (defun org-latex-auto-on ()
    (interactive)
    (add-hook 'org-mode-hook
	            (add-hook 'post-command-hook 'org-latex-fragment-toggle)))

  (defun org-latex-auto-off ()
    (interactive)
    (add-hook 'org-mode-hook
	            (remove-hook 'post-command-hook 'org-latex-fragment-toggle))))

(after! org
  (require 'org-num)
  (add-hook 'org-mode-hook #'org-num-mode))

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
        mu4e-alert-email-notification-types '(subjects))

  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

(after! mu4e
  (require 'mu4e-conversation)
  (setq mu4e-conversation-print-function 'mu4e-conversation-print-tree)
  (global-mu4e-conversation-mode +1))

(after! mu4e
  (setq mu4e-save-multiple-attachments-without-asking t
        mu4e-compose-dont-reply-to-self t
        mu4e-get-mail-command "true" ;; rely on mbsync timer instead
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
            :query "flag:unread AND NOT flag:trashed"
            :key ?u)
          ,(make-mu4e-bookmark
            :name "Today's messages"
            :query "date:today..now"
            :key ?t)
          ,(make-mu4e-bookmark
            :name "Last 7 days"
            :query "date:7d..now"
            :key ?w)
          ,(make-mu4e-bookmark
            :name "Flagged"
            :query "maildir:/outlook/Inbox and flag:flagged"
            :key ?f)
          ,(make-mu4e-bookmark
            :name "Inbox"
            :query "maildir:/outlook/Inbox"
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

(after! org
  (add-to-list 'org-modules 'org-bbdb))

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

(alert-add-rule :status   '(buried)     ;only send alert when buffer not visible
                :mode     'eshell-mode
                :style 'notifications)

(use-package! pdf-tools
  :defer-incrementally t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq pdf-misc-print-programm "/run/current-system/sw/bin/lpr")
  (setq pdf-misc-print-programm-args '("-P" "Brother_HL-L2320D_series"
                                       "-o" "media=letter"))
  (delq 'pdf-misc-size-indication-minor-mode pdf-tools-enabled-modes)
  (delq 'pdf-misc-context-menu-minor-mode pdf-tools-enabled-modes)
  (delq 'pdf-misc-menu-bar-minor-mode pdf-tools-enabled-modes))

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
