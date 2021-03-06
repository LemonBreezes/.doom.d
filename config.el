;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Iosevka" :size 22)
      ;; doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 22)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 19)
      doom-themes-enable-bold t)

(setq doom-theme 'doom-dark+)

(setq browse-url-browser-function #'browse-url-generic
      browse-url-generic-program "firefox")

(setq custom-file (expand-file-name "custom.el" doom-private-dir))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setq source-directory (expand-file-name "~/src/emacs-libjit/src")
      find-function-C-source-directory source-directory)

(after! bookmark
  (setq bookmark-default-file
        (expand-file-name ".local/bookmarks" doom-private-dir)))

(setq kill-ring-max 120)

(when (display-graphic-p)
  (global-set-key [remap suspend-frame] #'ignore))

(use-package! geoclue
  ;; :after (:all solar s)
  :defer 2
  :config
  (advice-add #'geoclue--location*
              :override
              (defun my-geoclue--location* ()
                "Start GeoClue2 and wait for it to provide the location of the host."
                (geoclue-start)
                (let ((times 0))
                  (while (or (not geoclue--location)
                             (> times 5))
                    (cl-incf times)
                    (sleep-for 0 250))
                (or geoclue--location
                    (error "Unable to determine current location")))))

  (let* ((whereami (geoclue-location))
         (lat (cdr (assoc 'latitude whereami)))
         (long (cdr (assoc 'longitude whereami))))
    ;; calendar-location-name
    (setq calendar-latitude lat
          calendar-longitude long))

  (defvar timezone-detect-process)
  (setq timezone-detect-process
        (start-process "ZoneDetect" "*ZoneDetect*" "~/src/ZoneDetect/demo"
                       (expand-file-name "~/src/ZoneDetect/database/builder/out_v1/timezone21.bin")
                       (number-to-string calendar-latitude)
                       (number-to-string calendar-longitude)))

  (set-process-sentinel timezone-detect-process
                        (lambda (process event)
                          (when (equal event "finished\n")
                            (setq calendar-location-name
                                  (with-current-buffer "*ZoneDetect*"
                                    (goto-char (point-min))
                                    (search-forward "The simple string is [" nil t)
                                    (buffer-substring-no-properties (point)
                                                                    (1- (point-at-eol)))))))))
;; (after! solar
;;   (setq calendar-latitude 40.11060)
;;   (setq calendar-longitude -88.20730)
;;   (setq calendar-location-name "Urbana, IL"))

(after! calendar
  (setq calendar-week-start-day 1))

(load (expand-file-name "email-address" doom-private-dir) t t)

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

(after! display-line-numbers (setq display-line-numbers-type 'relative))

(setq shift-select-mode nil)

(require 'el-patch)
(el-patch-use-package-mode +1)

(use-package! epa-file
  :defer-incrementally t
  :config
  (epa-file-enable))

(defun byte-compile-literate-config (&rest _)
  ;; Race condition: Will it never take more than 3 seconds to tangle config.org?
  (run-at-time 2.5 nil
               (lambda (&rest _)
                 (byte-compile-file (expand-file-name "config.el" doom-private-dir)))))

(global-set-key (kbd "s-SPC") #'doom/leader)
(map! :n "DEL" #'doom/leader)

(after! which-key
  (setq which-key-idle-delay 1.5))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

(setq window-combination-resize t)

(global-subword-mode +1)

(defun emacs-notify (message &optional title noflash)
  (unless title
    (setq title "ding"))
  (unless noflash
    (let ((visible-bell))
      (setq visible-bell t)
      (ding)(sit-for 0.5)(ding)))
  (message "%s - %s" title message))

(defun ping-status ()
  (interactive)
  (let ((buffer (generate-new-buffer "*internet*")))
    (make-process
     :name "internet"
     :connection-type 'pipe
     :buffer buffer
     :command (list "ping" "-q" "-c" "1" "8.8.8.8")
     :sentinel `(lambda (p e)
                  (with-current-buffer ',buffer
                    (goto-char (point-min))
                    (if (or (search-forward "unreachable" nil t)
                            (search-forward "errors" nil t))
                        (progn (unless (string-match "ping-status" (format "%s" timer-list))
                                 (run-with-timer 5 5 'ping-status)
                                 (emacs-notify "no internet")))
                      (when (string-match "ping-status" (format "%s" timer-list))
                        (cancel-function-timers 'ping-status))
                      (emacs-notify "internet working"))
                    (kill-buffer))))))

(after! s
  (defun my-split-string (string &optional separators omit-nulls keep-sep)
    "Split STRING into substrings bounded by matches for SEPARATORS."
    (let* ((keep-nulls (not (if separators omit-nulls t)))
           (rexp (or separators split-string-default-separators))
           (start 0)
           this-start this-end
           notfirst
           (list nil)
           (push-one
            (lambda ()
              (when (or keep-nulls (< this-start this-end))
                (let ((this (substring string this-start this-end)))
                  (when (or keep-nulls (> (length this) 0))
                    (push this list)))))))
      (while (and (string-match
                   rexp string
                   (if (and notfirst
                            (= start (match-beginning 0))
                            (< start (length string)))
                       (1+ start) start))
                  (< start (length string)))
        (setq notfirst t)
        (setq this-start start this-end (match-beginning 0)
              start (match-end 0))
        (funcall push-one)
        (when keep-sep
          (push (match-string 0 string) list)))
      (setq this-start start this-end (length string))
      (funcall push-one)
      (nreverse list))))

(require 'schrute)
(setf schrute-shortcuts-commands
      '((evil-snipe-F . (evil-backward-char left-char backward-char))
        (evil-snipe-f . (evil-forward-char right-char forward-char))
        (evilem-motion-next-line . (next-line evil-next-line))
        (evilem-motion-previous-line . (previous-line evil-previous-line))
        (evil-avy-goto-char-timer
         . (forward-word right-word backward-word left-word))))
(schrute-mode)

(advice-add #'looking-at :before-while
            (defun looking-at--type-check-args (s)
              (stringp s)))

(defconst alphabet '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z))
(defconst capitalized-alphabet '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R
                                    ?S ?T ?U ?V ?W ?X ?Y ?Z))
(defconst vowels '(?a ?e ?i ?o ?u))
(defconst numbers '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
(after! cl-seq (defconst consonants (cl-set-difference alphabet vowels)))
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
       (or (eq (length (this-command-keys)) 0)
           (memq (aref (this-command-keys) 0) kbl-admissible-prefixes)
           (eq (aref (this-command-keys) 0) 'easymotion)
           (equal key-from (this-command-keys)))
       (and (or (evil-motion-state-p)
                (evil-normal-state-p)
                (evil-visual-state-p)
                (evil-operator-state-p)
                (eq (bound-and-true-p evil-state) 'treemacs)
                (cl-equalp (and (fboundp #'exwm--app-name) (exwm--app-name))
                           "Firefox"))
            (not (or (bound-and-true-p avy--overlays-back)
                     (bound-and-true-p avy--overlays-lead)
                     (eq cursor-type evil-replace-state-cursor)
                     (minibufferp))))))

(cl-loop for p in keyboard-layout-translation-alist
         do (make-conditional-key-translation (kbd (car p)) (kbd (cdr p)) #'my-translate-keys-p))

(advice-add #'read-key-sequence
            :around
            (defun kbl--do-not-translate-keys-a (oldfun &rest args)
              (let ((evil-colemak-xvcf-enabled
                     (memq this-command '(describe-key
                                          describe-key-briefly
                                          Info-goto-emacs-key-command-node
                                          helpful-key
                                          lispyville-delete
                                          evil-delete
                                          evil-org-delete))))
                (apply oldfun args))))
(advice-add #'read-key-sequence-vector
            :around #'kbl--do-not-translate-keys-a)

(make-conditional-key-translation (kbl-kbd "v" 'control) (kbd "C-v") #'my-translate-keys-p)
(make-conditional-key-translation (kbd "C-v") (kbl-kbd "v" 'control) #'my-translate-keys-p)
(define-key key-translation-map (kbd "C-c C-e") (kbd "C-c C-p"))
(define-key key-translation-map (kbd "C-c C-p") (kbd "C-c C-e"))

(defun swap-semicolon-colon ()
  (when (and (memq major-mode
                   '(agda2-mode
                     haskell-mode))
             (eq (length (this-command-keys-vector))
                 1))
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

(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (let ((end (point)))
           (and (if (called-interactively-p 'any)
                    (skip-syntax-backward "w")
                  (= -3 (skip-syntax-backward "w")))
                (let (case-fold-search)
                  (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
                (capitalize-region (point) end))))))
;; (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(add-hook 'text-mode-hook (defun dubcaps-mode-enable (&rest _)
                            (dubcaps-mode 1)))

(use-package! perfect-margin
  :after-call post-command-hook
  :custom
  (perfect-margin-visible-width 100)
  (perfect-margin-ignore-modes '(exwm-mode
                                 doc-view-mode
                                 pdf-view-mode
                                 nov-mode
                                 vterm-mode
                                 mu4e-headers-mode
                                 html-mode
                                 ;; For log files
                                 fundamental-mode))
  (perfect-margin-ignore-regexps `("^minibuf"
                                   "^ "))
  :config
  ;; enable perfect-mode
  (perfect-margin-mode t)

  ;; add additinal key binding on margin area
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) #'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) #'ignore)
    (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll)))

  (defun perfect-margin--disable-a (&rest _)
    perfect-margin-mode)
  (cl-loop for func in '(perfect-margin-margin-windows
                         perfect-margin-margin-frame
                         perfect-margin-set-header-margin)
           do (eval `(advice-add ',func :before-while #'perfect-margin--disable-a)))

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
  (setq perfect-margin-unignored-modes '(mu4e-main-mode fundamental-mode))

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

  (defun perfect-margin-set-header-margin (&rest _)
    (unless (perfect-margin--auto-margin-ignore-p (selected-window))
      (cond ((and (stringp header-line-format)
                  (> (length header-line-format) 0))
             (setq header-line-format
                   (if (>= (frame-width)
                           (window-total-width))
                       (s-trim-left header-line-format)
                     (concat (make-string (+ (car (perfect-margin--init-window-margins))
                                             (if (bound-and-true-p display-line-numbers-mode)
                                                 display-line-numbers-width
                                               0)
                                             (if (eq major-mode 'paperless-mode)
                                                 4
                                               0))
                                          ?\s)
                             (s-trim-left header-line-format)))))
            ((and (listp header-line-format)
                  (> (length header-line-format) 1)
                  (stringp (cadr header-line-format)))
             (setf (cadr header-line-format)
                   (concat (make-string (car (perfect-margin--init-window-margins))
                                        ?\s)
                           (s-trim-left (cadr header-line-format)))))
            ((and (listp header-line-format)
                  (stringp (car header-line-format)))
             (setf (car header-line-format)
                   (concat (make-string (car (perfect-margin--init-window-margins))
                                        ?\s)
                           (s-trim-left (car header-line-format)))))))

    ;; Center Helm header line
    (when (bound-and-true-p helm-header-line-space-before-prompt)
      (setq helm-header-line-space-before-prompt (car (perfect-margin--init-window-margins)))))

  ;; (defun pad-string-to-center (s)
  ;;   (concat (make-string (max 0 (- (/ (frame-width) 2)
  ;;                                  (/ (length (s-trim s)) 2)))
  ;;                        ?\s)
  ;;           (s-trim s)))

  (advice-add #'read-string
              :around
              (defun perfect-margin--read-string-with-margins-a (oldfun &rest args)
                (when (and perfect-margin-mode
                           (not (s-prefix-p "helm-" (symbol-name this-command))))
                  (setcar args (concat (make-string (car (perfect-margin--init-window-margins))
                                                    ?\s)
                                       (s-trim-left (car args)))))
                (apply oldfun args)))

  (defun pp-fit-eval-output-buffer-a (&rest _)
    (with-current-buffer "*Pp Eval Output*"
      (visual-line-mode 1)
      (doom/delete-trailing-newlines))
    (fit-window-to-buffer
     (get-buffer-window "*Pp Eval Output*")
     (floor (* (frame-height) (/ helm-autoresize-max-height 100.0)))
     1 nil nil t))

  (advice-add #'pp-display-expression :after #'pp-fit-eval-output-buffer-a)

  (defun pp--center-minibuffer-output-a (oldfun &rest args)
    (if (or (= (length args) 1)
            (eq (second args) standard-output))
        (let ((s (prin1-to-string (car args))))
          (if (with-temp-buffer
                (insert s)
                (delay-mode-hooks (emacs-lisp-mode))
                (font-lock-default-function 'emacs-lisp-mode)
                (font-lock-default-fontify-region (point-min)
                                                  (point-max)
                                                  nil)
                (doom/delete-trailing-newlines)
                (setq s (buffer-string))
                (>= (- (point-max) (point-min)) (frame-width)))
              (apply oldfun args)
            (princ s)))
      (apply oldfun args)))

  (defun message-filter-center (args)
    "Center message string.
This is a :filter-args advice for `message`."
    (if (car args)
        (let ((str (apply #'format-message args)))
          (list "%s"
                (propertize str 'line-prefix
                            (list 'space :align-to
                                  (+ (car (perfect-margin--init-window-margins))
                                     (length "Eval: "))))))
      args))

  (advice-add #'message :filter-args #'message-filter-center)

  (defun eval-expression--center-printed-output (oldfun &rest args)
    (advice-add #'pp :around #'pp--center-minibuffer-output-a)
    (apply oldfun args)
    (advice-remove #'pp #'pp--center-minibuffer-output-a))

  (advice-add #'eval-expression :override #'pp-eval-expression)
  (advice-add #'pp-eval-expression :around #'eval-expression--center-printed-output)

  (advice-add #'read--expression
              :around #'perfect-margin--read-string-with-margins-a)
  (advice-add #'read-from-minibuffer
              :around #'perfect-margin--read-string-with-margins-a)

  (defun sh/current-time-microseconds ()
    "Return the current time formatted to include microseconds."
    (let* ((nowtime (current-time))
           (now-ms (nth 2 nowtime)))
      (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

  (defvar point-at-end-of-last-message nil)

;;   (defun sh/ad-timestamp-message (format-string &rest args)
;;     "Advice to run before `message' that prepends a timestamp to each message.

;; Activate this advice with:
;; (advice-add 'message :before 'sh/ad-timestamp-message)"
;;     (unless (string-equal format-string "%s%s")
;;       (let ((deactivate-mark nil)
;;             (inhibit-read-only t))
;;         (with-current-buffer "*Messages*"
;;           (goto-char (point-max))
;;           (if (not (bolp))
;;               (newline))
;;           (insert (sh/current-time-microseconds) " ")
;;           (setq point-at-end-of-last-message (point))))))

;;   (advice-add #'message :before #'sh/ad-timestamp-message)

  (advice-add #'perfect-margin-margin-windows
              :after #'perfect-margin-set-header-margin)
  (add-hook 'doom-switch-window-hook
            #'perfect-margin-set-header-margin)
  (add-hook 'window-size-change-functions
            #'perfect-margin-set-header-margin)
  (add-hook 'doom-switch-buffer-hook
            #'perfect-margin-margin-windows)
  (add-hook 'kill-buffer-hook
            #'perfect-margin-margin-windows)
  (after! mu4e
    (advice-add #'mu4e~main-view
                :after
                (defun perfect-margin-margin-windows-a (&rest _)
                  (perfect-margin-margin-windows))))

  (when (featurep! :ui popup)
    (setq +popup-buffer-mode-hook
          (cl-set-difference +popup-buffer-mode-hook
                             '(+popup-adjust-fringes-h
                               +popup-adjust-margins-h))))

  (after! org-capture
    (add-hook 'org-capture-mode-hook
              #'perfect-margin-set-header-margin
              'append)
    (add-hook 'org-capture-mode-hook
              #'perfect-margin-margin-windows
              'append)))

(use-package! circadian
  :config
  (setq circadian-themes '(("7:15" . doom-one-light)
                           ("19:30" . doom-dark+)))
  (circadian-setup)
  (after! exwm-randr
    (add-hook 'doom-load-theme-hook #'exwm-randr-refresh)))

(after! pdf-view
  (add-hook 'pdf-view-mode-hook
            (defun pdf-view--circadian-midnight-mode ()
              (if (eq doom-theme (cdar (last (circadian-themes-parse))))
                  (pdf-view-midnight-minor-mode +1)
                (pdf-view-midnight-minor-mode -1))))
  (add-hook 'doom-load-theme-hook
            (defun pdf-view--load-midnight-mode-on-theme-change ()
              (mapc (lambda (buf)
                        (with-current-buffer buf
                          (pdf-view--circadian-midnight-mode)))
                      (--filter (eq (buffer-local-value 'major-mode it)
                                    'pdf-view-mode)
                                (buffer-list))))))

(after! helm-icons (add-hook 'doom-load-theme-hook #'helm-icons-enable))

(use-package! hippie-exp
  :defer-incrementally t
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

  (defun google-suggest--request (query)
    (with-current-buffer
        (url-retrieve-synchronously
         (format "http://suggestqueries.google.com/complete/search?client=firefox&q=%s" query) t t 1)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point)(point-min))(buffer-string)))

  (defun google-suggest--list (result)
    (let* ((q (progn
                (string-match ",\\[\\(.*?\\)\\]" result)
                (match-string 1 result)))
           (r (replace-regexp-in-string "\\\"" "" q))
           (l (split-string r "," t)))
      (when (> (length (car (cdr l))) 0)
        (remove
         (car l)
         (cdr l)))))

  (defun try-expand-google-completion (old)
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (setq he-expand-list (sort
                            (all-completions
                             he-search-string
                             (lambda (s y n) (google-suggest--list (google-suggest--request s))))
                            'string-lessp)))
    (if (null he-expand-list)
        (progn
          (when old (he-reset-string))
          ())
      (he-substitute-string (car he-expand-list) t)
      (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list (cdr he-expand-list))
      t))

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
          try-expand-google-completion
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
      (forward-line (-  linen (count-lines (point-min) (point))))
      (pulse-momentary-highlight-one-line (point) 'highlight))
    (sit-for 0.1)
    (apply f args)))

(defun my-indicate-scroll-forward (f &rest args)
  (my-indicate-scroll (1- (window-end)) f args))

(defun my-indicate-scroll-backward (f &rest args)
  (my-indicate-scroll (window-start) f args))

(after! counsel
  (map! "C-c u" #'counsel-unicode-char))

(with-eval-after-load 'shr ; lazy load is very important, it can save you a lot of boot up time
  (require 'shrface)
  (shrface-basic) ; enable shrfaces, must be called before loading eww/dash-docs/nov.el
  (shrface-trial) ; enable shrface experimental face(s), must be called before loading eww/dash-docs/nov.el
  (setq shrface-href-versatile t)     ; enable versatile URL faces support
                                        ; (http/https/ftp/file/mailto/other), if
                                        ; `shrface-href-versatile' is nil, default
                                        ; face `shrface-href-face' would be used.

  (when (featurep! :editor fold)
    (advice-add #'+fold--hideshow-fold-p
                :around
                (defun +fold--ignore-if-shrface-enabled-a (oldfun &rest args)
                  (unless (or (bound-and-true-p shrface-mode)
                              (memq major-mode '(w3m-mode)))
                    (apply oldfun args))))

    (advice-add #'+fold/open
                :around
                (defun +fold--use-outline-folds-in-shrface-a (oldfun &rest args)
                  (if (bound-and-true-p shrface-mode)
                      (outline-show-entry)
                    (apply oldfun args)))))

  (advice-add #'+fold/open-all
              :around
              (defun +fold--use-outline-folds-in-shrface-a (oldfun &optional level)
                (if (bound-and-true-p shrface-mode)
                    (progn (when (featurep 'vimish-fold)
                             (vimish-fold-unfold-all))
                           (save-excursion
                             (if (integerp level)
                                 (progn
                                   (outline-hide-sublevels (max 1 (1- level))))
                               (when (fboundp 'outline-show-all)
                                 (outline-show-all)))))
                  (apply oldfun args))))

  ;; eww support
  (with-eval-after-load 'eww
    (add-hook 'eww-after-render-hook 'shrface-mode)
    (map! :map eww-mode-map
          :g "C-c C-n" #'org-next-visible-heading
          :g "C-c C-p" #'org-previous-visible-heading
          :g "C-c <C-backspace>" #'org-up-element
          :g "C-c C-^" #'org-up-element
          :g "C-c C-f" #'org-forward-heading-same-level
          :g "C-c C-b" #'org-backward-heading-same-level
          :g "C-c C-u" #'outline-up-heading))

  (after! helpful
    (map! :map helpful-mode-map
          :g "C-c C-n" #'org-next-visible-heading
          :g "C-c C-p" #'org-previous-visible-heading
          :g "C-c <C-backspace>" #'org-up-element
          :g "C-c C-^" #'org-up-element
          :g "C-c C-f" #'org-forward-heading-same-level
          :g "C-c C-b" #'org-backward-heading-same-level
          :g "C-c C-u" #'outline-up-heading))

  ;; nov support
  (with-eval-after-load 'nov
    (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))) ; reset nov-shr-rendering-functions, in case of the list get bigger and bigger
    (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
    (add-hook 'nov-mode-hook 'shrface-mode))

  ;; mu4e support
  (with-eval-after-load 'mu4e
    (add-hook 'mu4e-view-mode-hook 'shrface-mode)))

(use-package! shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions)))
  (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight))
  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   (setq-local fill-column 120)
                   (indent-rigidly (point-min) (point-max) 2)
                   ;; (fill-region (point-min) (point-max) nil nil nil)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (insert "  ")                     ; indentation
      ;; (insert (propertize (concat "#+begin_src" lang) 'face 'org-block-begin-line)) ; delete "lang" of this line, if you found the wrong detected langugage is annoying
      (shr-ensure-newline)
      (insert
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code))
      (shr-ensure-newline)
      (insert "  ")                     ; indentation
      ;; (insert (propertize "#+end_src" 'face 'org-block-end-line ) )
      (shr-ensure-newline))))

(use-package! emojify
  :defer-incrementally t
  :config
  (global-emojify-mode +1)
  (add-to-list 'emojify-inhibit-major-modes 'helpful-mode))

(use-package! sublimity
  :defer-incrementally t
  :config
  (require 'sublimity-scroll)
  (setq sublimity-scroll-weight 5
        sublimity-scroll-drift-length 10
        sublimity-ignored-scroll-commands '(scroll-bar-drag
                                            scroll-bar-toolkit-scroll
                                            scroll-bar-scroll-up
                                            scroll-bar-scroll-down
                                            +workspace/switch-to-0
                                            +workspace/switch-to-1
                                            +workspace/switch-to-2
                                            +workspace/switch-to-3
                                            +workspace/switch-to-4
                                            +workspace/switch-to-5
                                            +workspace/switch-to-6
                                            +workspace/switch-to-7
                                            +workspace/switch-to-8
                                            +workspace/switch-to-final))
  (advice-add #'+workspace-switch :before
              (defun sublimity--disable-a (&rest _)
                (when sublimity-mode
                  (sublimity-mode -1)
                  (run-at-time 0.01 nil #'sublimity-mode))))
  (sublimity-mode +1))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(after! nov
  (add-to-list 'doom-large-file-excluded-modes 'nov-mode))

(with-eval-after-load 'org
  (require 'inherit-org)

  (with-eval-after-load 'info
    (add-hook 'Info-mode-hook 'inherit-org-mode))

  ;; (with-eval-after-load 'helpful
  ;;   (add-hook 'helpful-mode-hook 'inherit-org-mode))

  (with-eval-after-load 'w3m
    (add-hook 'w3m-fontify-before-hook 'inherit-org-w3m-headline-fontify) ;only one level is supported
    (add-hook 'w3m-fontify-after-hook 'inherit-org-mode)))

(use-package! helm-tail
  :after helm
  :bind (:map doom-leader-map
         ("a!" . helm-tail))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "helm-tail") . (nil . "Recent errors")))))

(use-package!  switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete)
         ("C-x 4 d" . switch-window-then-dired)
         ("C-x 4 f" . switch-window-then-find-file)
         ("C-x 4 m" . switch-window-then-compose-mail)
         ("C-x 4 r" . switch-window-then-find-file-read-only)
         ("C-x 4 C-f" . switch-window-then-find-file)
         ("C-x 4 C-o" . switch-window-then-display-buffer)
         ("C-x 4 0" . switch-window-then-kill-buffer)
         ;; (:map doom-leader-map
         ;;   ("0" . switch-window-then-delete)
         ;;   ("1" . switch-window-then-maximize)
         ;;   ("2" . switch-window-then-split-below)
         ;;   ("3" . switch-window-then-split-right)
         ;;   ("4" . switch-window))
         )
  :defer-incrementally t
  :config
  (setq switch-window-background nil
        switch-window-qwerty-shortcuts
        '("a" "r" "s" "t" "n" "e" "i" "o" "g" "m" "q" "w" "c" "p" "z" "j" "l" "u" "'" "x" "v" "f" "d" "b" "k" "h")
        switch-window-shortcut-style 'qwerty
        switch-window-shortcut-appearance 'text
        switch-window-default-window-size 0.6
        switch-window-auto-resize-window nil)
  (when (featurep! :ui workspaces)
    (advice-add #'switch-window-then-delete :around
                (defun switch-window--close-workspace-a (oldfun &rest args)
                  (if (eq (length (switch-window--list))
                          1)
                      (+workspace/close-window-or-workspace)
                    (apply oldfun args))))))

(use-package! imenu-list
  :bind ((:map global-map ("C-'" . imenu-list-smart-toggle)))
  :defer-incrementally t
  :config
  (setq imenu-list-focus-after-activation t)
  (set-popup-rule! (concat "^\\" imenu-list-buffer-name "\\*$")
    :size imenu-list-size
    :side imenu-list-position
    :focus imenu-list-focus-after-activation
    :quit t)
  (advice-add #'imenu-list-goto-entry
              :around
              (defun imenu-list-goto-entry-a (oldfun &rest args)
                (let (pdf-window))
                (save-excursion
                  (save-selected-window
                    (apply oldfun args)
                    (setq pdf-window (selected-window))))
                (select-window pdf-window)))
  (defun imenu-list-preview-entry ()
    (interactive)
    (save-excursion
      (save-selected-window
        (imenu-list-goto-entry))))
  (map! :map imenu-list-major-mode-map
        :n ";" #'imenu-list-preview-entry)
  (after! evil-snipe
    (add-to-list 'evil-snipe-disabled-modes
                 'imenu-list-major-mode))
  (after! org
    (map! :map org-mode-map
          "C-'" #'imenu-list-smart-toggle)))

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

(require 'exwm-workspace)
(require 'exwm-xim)
(require 'exwm)
(require 'exwm-systemtray)
(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist
      '(0 "HDMI-0" 1 "VGA-0")
      exwm-workspace-number 2)
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil
;;              (string-join '("xrandr --output VGA-0 --left-of HDMI-0 --auto"
;;                             "xrandr --newmode \"700x480_ati\" 13.849698 700 742 801 867 480 490 496 533 interlace -hsync -vsync"
;;                             "xrandr --addmode VGA-0 \"700x480_ati\""
;;                             "xrandr --output VGA-0 --mode \"700x480_ati\"")
;;                           " && "))))
(exwm-randr-enable)
(exwm-xim-enable)
(exwm-randr-enable)
(exwm-systemtray-enable)
(when (featurep! :ui dashboard)
  (add-hook! 'exwm-init-hook
    (call-interactively #'+doom-dashboard/open)))

(add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

;; let emacs handle these keys
(dolist (k '(XF86AudioLowerVolume
             XF86AudioRaiseVolume
             XF86AudioPlay
             XF86AudioStop
             XF86AudioMute
             XF86AudioPrev
             XF86AudioNext
             ?\C-\^
             ?\C-x
             ?\C-\S-f
             ?\C-\S-p
             ?\C-\S-n
             ?\C-\S-b
             ?\C-\S-l
             ?\C-\S-u
             ?\C-\$
             ?\s-l
             ?\s-u
             ?\s-q
             ?\s-w
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
             ?\M-0
             ?\C-\M-\!
             ?\M-`))
  (cl-pushnew k exwm-input-prefix-keys))

;; (call-process-shell-command
;;  (string-join
;;   '("nvidia-settings -a '[gpu:0]/gpupowermizermode=1'
;; -a '[gpu:0]/gpufancontrolstate=1'
;; -a '[fan:0]/gputargetfanspeed=100'
;; -a '[fan:1]/gputargetfanspeed=100'
;; -a '[gpu:0]/gpumemorytransferrateoffset[4]=700'
;; -a '[gpu:0]/gpugraphicsclockoffset[4]=70' & ")
;;   " ")
;;  nil 0)

(defun exwm-wechat-start ()
  (interactive)
  (defvar exwm-wechat-process nil)
  (setq exwm-wechat-process
        (start-process-shell-command
         "WeChat"
         nil
         (string-join
          '("WINEPREFIX=~/win32"
            "WINEARCH=win32"
            ;; "LC_ALL=\"zh_CN.UTF8\""
            "LC_ALL=en_US.UTF-8"
            "wine"
            "~/win32/dosdevices/c:/Program\\ Files/Tencent/WeChat/WeChat.exe")
          " "))))

(defun exwm-run-or-raise-urxvt ()
  (interactive)
  (defvar exwm-urxvt-process nil)
  (if (and (featurep! :ui workspaces)
           (+workspace-exists-p "URxvt"))
      (+workspace-switch "URxvt")
      (setq exwm-urxvt-process
            (start-process-shell-command
             "URxvt"
             nil
             "urxvt"))))

(defun exwm-byond-start ()
  (interactive)
  (defvar exwm-byond-process nil)
  (setq exwm-byond-process
        (start-process-shell-command
         "BYOND"
         nil
         (string-join
          '("WINEPREFIX=~/win32"
            "WINEARCH=win32"
            "LC_ALL=en_US.UTF-8"
            "wine"
            "~/win32/drive_c/Program\\ Files/BYOND/bin/byond.exe")
          " "))))

(exwm-input-set-key (kbd "<s-return>") #'exwm-run-or-raise-urxvt)

(exwm-input-set-key
 (kbd "s-w")
 (defun exwm-run-or-raise-firefox ()
   (interactive)
   (if (and (featurep! :ui workspaces)
            (+workspace-exists-p "Firefox"))
       (+workspace-switch "Firefox")
     (start-process "Firefox" nil "firefox"))))

(exwm-input-set-key
 (kbd "s-q")
 (defun exwm-run-or-raise-qutebrowser ()
   (interactive)
   (if (and (featurep! :ui workspaces)
            (+workspace-exists-p "Qutebrowser"))
       (+workspace-switch "Qutebrowser")
     (start-process "Qutebrowser" nil "qutebrowser"))))

(exwm-input-set-key
 (kbd "C-$")
 (defun +counsel-linux-app ()
   (interactive)
   (require 'counsel)
   (counsel-linux-app)))

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name)
                        (string= "qutebrowser" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))

(add-hook 'exwm-update-title-hook
          (defun exwm--update-buffer-name-to-title ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(advice-add #'exwm--update-utf8-title
            :before-while
            (defun exwm--update-utf8-title-a (id &optional force)
              (get-buffer-window (exwm--id->buffer id))))

(add-hook 'exwm-update-class-hook
          (defun exwm--init-urxvt ()
            (when (string= exwm-class-name "URxvt")
              (define-key exwm-mode-map [?\C-c ?\C-c]
                (cmd!
                 (exwm-input--fake-key ?\C-c))))))

;; Not working
;; (add-hook 'exwm-update-class-hook
;;           (defun exwm--init-VBoxSDL ()
;;             (when (string= exwm-class-name "VBoxSDL")
;;               (exwm-input-release-keyboard))))

;; (add-hook 'exwm-update-class-hook
;;           (defun exwm--init-qutebrowser ()
;;             (when (string= exwm-class-name "qutebrowser")
;;               (exwm-input-grab-keyboard))))

(after! exwm
  (require 'exwm-firefox-evil)
  (add-to-list 'exwm-firefox-evil-firefox-class-name "Firefox Developer Edition")
  ;; Auto enable exwm-firefox-evil-mode on all firefox buffers
  (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)
  (dolist (k `(escape))
    (cl-pushnew k exwm-input-prefix-keys))

  ;; (defvar exwm-firefox-update-url-hook nil)
  ;; (defvar exwm-firefox--update-url-hook-timer nil)
  ;; (defvar exwm-firefox--last-url nil)
  ;; (defun exwm-firefox--start-update-url-hook-timer ()
  ;;   (when (timerp exwm-firefox--update-url-hook-timer)
  ;;     (cancel-timer exwm-firefox--update-url-hook-timer))
  ;;   (setq exwm-firefox--update-url-hook-timer
  ;;         (run-at-time
  ;;          0.05 0.05
  ;;          (defun exwm-firefox--run-update-url-hook-maybe ()
  ;;            (let ((url (and (exwm-firefox?)
  ;;                            (exwm-firefox--current-tab-url))))
  ;;              (unless (equal exwm-firefox--last-url url)
  ;;                ;; (message "RUNNING URL HOOKS: %s | %s"
  ;;                ;;          exwm-firefox--last-url url)
  ;;                (run-hooks 'exwm-firefox-update-url-hook)
  ;;                (setq exwm-firefox--last-url url)))))))

  ;; (add-transient-hook! 'exwm-firefox-evil-mode-hook
  ;;   (exwm-firefox--start-update-url-hook-timer))

  ;; (add-hook 'exwm-update-title-hook
  ;;           (defun exwm-firefox--enter-normal-state-in-new-pages ()
  ;;             (when (exwm-firefox?)
  ;;               (unless (string-equal exwm-title "Mozilla Firefox")
  ;;                 (exwm-firefox-evil-normal)))))

  (defun exwm-firefox--initialize-temporary-text-input ()
    (setq exwm--link-hint-active-p t)
    (unless (eq evil-state 'insert)
      (exwm-firefox-evil-insert)
      (add-hook 'exwm-update-title-hook #'exwm-firefox-hint-links--transient-state)))

  (defun exwm-firefox-hint-links--transient-state ()
    (when (exwm-firefox?)
      (setq exwm--link-hint-active-p nil)
      (exwm-firefox-evil-normal)
      (remove-hook 'exwm-update-title-hook #'exwm-firefox-hint-links--transient-state)))

  (defvar exwm--link-hint-active-p nil)

  (defun exwm-firefox-core-hint-links ()
    (interactive)
    (exwm-input--fake-key ?\M-x)
    (exwm-firefox--initialize-temporary-text-input))

  (defun exwm-firefox-core-hint-links-new-tab-and-switch ()
    (interactive)
    (exwm-input--fake-key ?\M-l)
    (exwm-firefox--initialize-temporary-text-input))

  ;; (add-hook 'doom-switch-buffer-hook
  ;;           (defun exwm-firefox-evl--use-normal-state-by-default ()
  ;;             (when (exwm-firefox?)
  ;;               (exwm-firefox-evil-normal))))

  (defun exwm-firefox-core-return ()
    (interactive)
    (exwm-input--fake-key 'return)
    (when exwm--link-hint-active-p
      (setq exwm--link-hint-active-p nil)
      (exwm-firefox-evil-normal)))

  (add-transient-hook! 'exwm-firefox-evil-mode-hook
    (advice-remove #'exwm-firefox-core-quick-find #'exwm-firefox-evil-insert)
    (advice-remove #'exwm-firefox-core-focus-search-bar #'exwm-firefox-evil-insert))
  (advice-add #'exwm-firefox-core-quick-find :after
              #'exwm-firefox--initialize-temporary-text-input)
  (advice-add #'exwm-firefox-core-focus-search-bar :after
              #'exwm-firefox--initialize-temporary-text-input)

  (defun exwm-firefox-core-zoom-in ()
    (interactive)
    (exwm-input--fake-key ?\C-+))

  (defun exwm-firefox-core-zoom-out ()
    (interactive)
    (exwm-input--fake-key ?\C--))

  (defun exwm-firefox-core-reset-zoom ()
    (interactive)
    (exwm-input--fake-key ?\C-0))

  (defun exwm-firefox-core-focus-first-input ()
    (interactive)
    (exwm-input--fake-key ?\M-j)
    (exwm-firefox-evil-insert))

  ;; (advice-add #'exwm-firefox-core-tab-new :after
  ;;             (defun exwm-firefox--fix-insert ()
  ;;               (run-at-time 0.05 nil
  ;;                            (lambda ()
  ;;                              (exwm-firefox-evil-insert)))))

  ;; (defvar exwm-firefox--left-click-timer nil)
  ;; (defvar exwm-firefox--right-click-timer nil)

  ;; (defun exwm-firefox--simulate-left-click ()
  ;;   (exwm-firefox-evil-insert)
  ;;   (if (memq exwm-firefox--left-click-timer
  ;;             timer-list)
  ;;       (cancel-timer exwm-firefox--left-click-timer)
  ;;     (setq exwm-firefox--left-click-timer
  ;;           (run-at-time 0.2 nil
  ;;                        (lambda ()
  ;;                          (start-process "xdotool left click"
  ;;                                         nil
  ;;                                         "xdotool"
  ;;                                         "click"
  ;;                                         "1")
  ;;                          (run-at-time 0.5 nil
  ;;                                       (lambda ()
  ;;                                         (exwm-firefox-evil-normal))))))))

  ;; (defun exwm-firefox--simulate-right-click ()
  ;;   (interactive)
  ;;   (exwm-firefox-evil-insert)
  ;;   (if (memq exwm-firefox--right-click-timer
  ;;             timer-list)
  ;;       (cancel-timer exwm-firefox--right-click-timer)
  ;;     (setq exwm-firefox--right-click-timer
  ;;           (run-at-time 0.2 nil
  ;;                        (lambda ()
  ;;                          (start-process "xdotool right click"
  ;;                                         nil
  ;;                                         "xdotool"
  ;;                                         "click"
  ;;                                         "3")
  ;;                          (run-at-time 0.5 nil
  ;;                                       (lambda ()
  ;;                                         (exwm-firefox-evil-normal))))))))

  ;; (defun exwm-firefox--fix-mouse-a (oldfun command &rest args)
  ;;   (if (eq command #'mouse-drag-region)
  ;;       (condition-case err
  ;;           (apply oldfun command args)
  ;;         (error (if (and (equal (second err) "mouse-drag-region must be bound to an event with parameters")
  ;;                         (exwm-firefox?))
  ;;                    (exwm-firefox--simulate-left-click)
  ;;                  (and (stringp (cdr err))
  ;;                       (signal (car err) (cdr err))))))
  ;;     (apply oldfun command args)))

  ;; (add-hook 'post-command-hook
  ;;           (defun exwm-firefox--advise-command-execute ()
  ;;             (if (or (exwm-firefox?)
  ;;                     (equal (+workspace-current-name)
  ;;                            "Firefox"))
  ;;                 (advice-add #'command-execute :around #'exwm-firefox--fix-mouse-a)
  ;;               (advice-remove #'command-execute #'exwm-firefox--fix-mouse-a))))

  (defun exwm-firefox-cancel-a ()
    (exwm-input--fake-key 'tab)
    (exwm-input--fake-key 'tab)
    (exwm-input--fake-key 'tab))

  (el-patch-defun exwm-firefox-core-quick-find ()
    "Quick find."
    (interactive)
    (exwm-input--fake-key (el-patch-swap ?/ ?\C-f)))

  (advice-add #'exwm-firefox-core-cancel :after #'exwm-firefox-cancel-a)

  ;; (defun exwm-simulate-return ()
  ;;   (interactive)
  ;;   (exwm-input--fake-key 'return))
  ;; (define-key exwm-mode-map (kbd "<return>") nil)
  ;; (define-key exwm-mode-map (kbd "RET") nil)

  (el-patch-defun exwm-firefox-evil-insert ()
    "Pass every key to firefox."
    (interactive)
    (setq-local exwm-input-line-mode-passthrough nil)
    (el-patch-swap (evil-insert-state)
                   (call-interactively #'evil-insert)))

  (defun exwm-firefox-core-alternative-quick-find ()
    (interactive)
    (exwm-input--fake-key ?/))

  ;; (defun exwm-firefox-core-undo-close-tab ()
  ;;   (interactive)
  ;;   (exwm-input--fake-key ?\C-\S-t)
  ;;   (exwm-firefox--initialize-temporary-text-input))

  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "g/") #'exwm-firefox-core-alternative-quick-find)
  ;; (define-key exwm-firefox-evil-mode-map (kbd "<down-mouse-3>") #'exwm-firefox--simulate-right-click)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "gi") #'exwm-firefox-core-focus-first-input)

  (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "<return>") #'exwm-firefox-core-return)
  (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "RET") #'exwm-firefox-core-return)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "<return>") #'exwm-firefox-core-return)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "RET") #'exwm-firefox-core-return)
  (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "<return>") #'exwm-firefox-core-return)
  (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "RET") #'exwm-firefox-core-return)

  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "d") #'+lookup/online)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "D") #'+lookup/online-select)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "u") #'exwm-firefox-core-tab-close-undo)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "U") #'exwm-firefox-core-window-close-undo)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "C-/") #'exwm-firefox-core-undo)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "f") #'exwm-firefox-core-hint-links)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "F") #'exwm-firefox-core-hint-links)

  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "+") #'exwm-firefox-core-zoom-in)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "-") #'exwm-firefox-core-zoom-out)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "=") #'exwm-firefox-core-reset-zoom)

  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "<next>") #'exwm-firefox-core-page-down)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "<prior>") #'exwm-firefox-core-page-up)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "o") #'exwm-firefox-core-focus-search-bar)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd ";") #'exwm-firefox-core-focus-search-bar)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd ":") #'exwm-firefox-core-focus-search-bar)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "O") #'exwm-firefox-core-tab-new)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "K") #'exwm-firefox-core-tab-next)
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "J") #'exwm-firefox-core-tab-previous))

(after! exwm
  (defun my-exwm-send-string (string)
    "Send STRING to `exwm-mode' buffer or just insert it."
    (if (eq major-mode 'exwm-mode)
        (mapc #'exwm-input--fake-key (string-to-list string))
      (insert string)))

  (defun my-exwm-insert-path ()
    "Read and insert path."
    (interactive)
    (let ((ivy-inhibit-action #'my-exwm-send-string))
      (counsel-find-file)))

  (defun konix/kill-ring-insert ()
    (interactive)
    (let ((to_insert (completing-read "Yank : "
                                      (delete-duplicates kill-ring :test #'equal))))
      (when (and to_insert (region-active-p))
        ;; the currently highlighted section is to be replaced by the yank
        (delete-region (region-beginning) (region-end)))
      (my-exwm-send-string to_insert)))

  (map! :map exwm-mode-map
        ;; :n "C-c u" #'exwm-counsel-unicode-char
        "C-c f" #'my-exwm-insert-path
        "C-c y" #'konix/kill-ring-insert
        "C-c h" #'counsel-ffdata-firefox-history
        (:when (featurep! :tools lookup)
         "C-c s" #'+lookup/online
         "C-c S" #'+lookup/online-select)
        "C-c b" #'counsel-ffdata-firefox-bookmarks))

(use-package! exwm-firefox
  :after exwm
  :config
  (exwm-firefox-mode)
  (map! :map exwm-firefox-keymap
        "C-c C-f" nil
        "C-c C-b" nil)
  (advice-add #'exwm-firefox-merge
              :after
              #'delete-window))

(after! exwm
  (defun exwm-toggle-plover ()
    (interactive)
    (defvar exwm-plover-process nil)
    (if (process-live-p exwm-plover-process)
        (kill-process exwm-plover-process)
      (setq exwm-plover-process (start-process "plover" nil "plover"))))
  (map! "C-M-!" #'exwm-toggle-plover))

(after! exwm
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

  (exwm-input-set-key
   (kbd "s-r")
   (defun exwm-run-or-raise-retroarch ()
     (interactive)
     (if (and (featurep! :ui workspaces)
              (+workspace-exists-p "retroarch"))
         (+workspace-switch "retroarch")
       (call-process-shell-command "taskset 0x6 retroarch" nil 0)))))

(after! exwm
  (require 'exwm-edit)
  (setq exwm-edit-split "below")
  (add-hook! '(exwm-edit-before-finish-hook
               exwm-edit-before-cancel-hook)
    (defun exwm-edit-clear-last-kill ()
      (setq exwm-edit-last-kill nil)))

  (defvar exwm-edit-activate-appropriate-major-mode--timer)
  (add-hook 'exwm-edit-compose-hook
            (defun exwm-edit-activate-appropriate-major-mode ()
              (setq exwm-edit-activate-appropriate-major-mode--timer
                    (run-at-time 0.01 0.01
                                 (defun exwm-edit-activate-appropriate-major-mode--timer-fn (&rest _)
                                   (unless (string-prefix-p "*exwm-edit "
                                                            (buffer-name))
                                     (cancel-timer exwm-edit-activate-appropriate-major-mode--timer))
                                   (when (buffer-modified-p)
                                     (cancel-timer exwm-edit-activate-appropriate-major-mode--timer)
                                     (let ((header-line-format--old header-line-format))
                                       (when (string-prefix-p "*exwm-edit "
                                                              (buffer-name))
                                         (cl-case (language-detection-buffer)
                                           (emacslisp (emacs-lisp-mode))
                                           (python (python-mode))
                                           (t (org-mode)))
                                         (setq header-line-format header-line-format--old))))))))))

(after! exwm
  (defun true (&rest _) t)
  (defvar exwm--old-default-directory nil)

  (defun exwm-leader-key ()
    (interactive)
    (advice-add #'exwm-input--event-passthrough-p
                :around #'true)
    (setq unread-command-events '((t . 32))
          exwm--old-default-directory default-directory
          default-directory (expand-file-name "~/")
          exwm-leader-key-timer
          (run-at-time 0.05 0.05
                       (lambda (&rest _)
                         (unless (string-match-p " " (this-command-keys))
                           (advice-remove #'exwm-input--event-passthrough-p
                                          #'true)
                           (setq list-buffers-directory nil
                                 default-directory exwm--old-default-directory
                                 exwm--old-default-directory nil)
                           (cancel-timer exwm-leader-key-timer))))))

  (add-hook 'exwm-mode-hook
            (defun exwm--init-leader-bindings ()
              (map! :map exwm-mode-map
                    :n "C-c SPC" #'exwm-leader-key
                    :g "C-SPC" #'exwm-leader-key
                    :g "M-SPC" #'exwm-leader-key))))

(after! (:all exwm evil)
  (advice-add #'evil-repeat-pre-hook
              :around
              (defun exwm--ignore-in-exwm-buffer-a (oldfun &rest args)
                (unless (or (derived-mode-p 'exwm-mode)
                            (eq (point-max) (1- (point-min))))
                  (apply oldfun args)))))

(after! (:all s exwm dash)
  (defun exwm-firefox--opened-tabs ()
    "The web page must have finished loading first. Set
'browser.sessionstore.interval' as low as possible in your 'about:config' page."
    (--map (let ((i (string-match " " it)))
             (cons (substring-no-properties it (1+ i))
                   (substring-no-properties it 0 i)))
           (split-string (shell-command-to-string "firefox-get-tabs")
                         "\n" t)))

  (defun exwm--get-active-firefox-window-title ()
    "Meant to be used from Firefox with EXWM grabbing input."
    (and (stringp exwm-title)
         (->> exwm-title
              (string-remove-suffix " - Firefox")
              (string-remove-suffix " - Mozilla Firefox"))))

  (defun exwm-firefox--current-tab-url ()
    (let ((i (and (stringp exwm-title)
                  (or (string-match " - Firefox" exwm-title)
                      (string-match " - Mozilla Firefox" exwm-title)))))
      (when (wholenump i)
        (let ((url (alist-get (substring-no-properties exwm-title 0 i)
                              (exwm-firefox--opened-tabs) nil nil #'equal)))
          (cond ((and (stringp url)
                      (string-prefix-p "moz-extension://" url)) nil)
                (t url))))))

  (defun exwm--get-active-qutebrowser-window-title ()
    "Meant to be used from Qutebrowser without EXWM grabbing input."
    (car (--map-first (cl-equalp (buffer-local-value 'exwm-class-name it)
                                 "qutebrowser")
                      (->> (buffer-local-value 'exwm-title it)
                           (string-remove-suffix " - qutebrowser"))
                      (doom-visible-buffers))))

  (defvar exwm--org-roam-capture-web-page-history nil)
  ;; (after! savehist
  ;;   (add-to-list 'savehist-additional-variables
  ;;                exwm--org-roam-capture-web-page-history))
  (defun exwm--org-roam-capture-web-page (&optional title url)
    (interactive)
    (unless (and (stringp title)
                 (stringp url))
      (setq title (caar exwm--org-roam-capture-web-page-history)
            url (cdar exwm--org-roam-capture-web-page-history)))
    (when (and (stringp title)
               (stringp url))
      (push (cons title url) exwm--org-roam-capture-web-page-history)
      (setq url (car (s-split "#" url t))
            last-command 'exwm--org-roam-capture-web-page)
      (advice-add #'akirak/org-set-created-timestamp :override #'ignore)
      (let* ((completions (org-roam--get-title-path-completions))
             (org-roam-buffer-position 'bottom)
             ;; (title-with-keys (org-roam-completion--completing-read "File: "
             ;;                                                        completions))
             ;; (title-with-keys title)
             (res (cdr (assoc title completions
                              (lambda (x y)
                                ;; (message "%s || %s"
                                ;;          (if (string-match "^([^()]*) " x)
                                ;;              (substring x (match-end 0) nil)
                                ;;            x)
                                ;;          (if (string-match "^([^()]*) " y)
                                ;;            (substring y (match-end 0) nil)
                                ;;          y))
                                (equal (if (string-match "^([^()]*) " x)
                                           (substring x (match-end 0) nil)
                                         x)
                                       (if (string-match "^([^()]*) " y)
                                           (substring y (match-end 0) nil)
                                         y))))))
             ;; (title (or (plist-get res :title) title-with-keys))
             (file-path (plist-get res :path)))
        (let ((org-roam-capture--info (list (cons 'title title)
                                            (cons 'slug (org-roam--title-to-slug title))
                                            (cons 'file file-path)))
              (org-roam-capture--context 'capture)
              (+org-roam-open-buffer-on-find-file nil))
          (setq org-roam-capture-additional-template-props (list :capture-fn 'org-roam-capture))
          (condition-case err
              (org-roam-capture--capture)
            (error (user-error "%s.  Please adjust `org-roam-capture-templates'"
                               (error-message-string err)))))
        (if file-path
            (goto-char (point-max))
          (insert "#+ROAM_TAGS: "
                  "\n- source :: "
                  (org-link-make-string url title))
          (forward-line -1)
          (end-of-line)))
      (advice-remove #'akirak/org-set-created-timestamp #'ignore)
      (run-at-time 0.1 nil #'exwm-randr-refresh)))

  (defun exwm-firefox-org-roam-capture-web-page ()
    (interactive)
    (exwm--org-roam-capture-web-page (exwm--get-active-firefox-window-title)
                                     (exwm-firefox--current-tab-url)))

  (after! exwm-firefox-evil
    (map! "C-c c" #'exwm-firefox-org-roam-capture-web-page)))

(after! org-capture
  (advice-add #'org-capture-kill :after #'exwm-randr-refresh))

;; monitor the system clipboard and add any changes to the kill ring
(when (fboundp #'clipmon-mode-start)
  (add-to-list 'after-init-hook #'clipmon-mode-start))

(after! clipmon
  (setq clipmon-timer-interval 1)      ; check system clipboard every n secs
  (setq clipmon-autoinsert-sound t)    ; t for included beep, or path or nil
  (setq clipmon-autoinsert-color "red") ; color of cursor when autoinsert is on
  (setq clipmon-autoinsert-timeout 5)  ; stop autoinsert after n mins inactivity
  (setq clipmon-transform-trim t)      ; remove leading whitespace
  (setq clipmon-transform-remove       ; remove text matching this regexp
        "\\[[0-9][0-9]?[0-9]?\\]\\|\\[citation needed\\]\\|\\[by whom?\\]")
  (setq clipmon-transform-prefix "")    ; add to start of text
  (setq clipmon-transform-suffix "\n\n") ; add to end of text
  (setq clipmon-transform-function nil))  ; additional transform function

(after! exwm
  (add-hook 'exwm-update-title-hook
            (defun exwm-urxvt--update-buffer-file-name ()
              (when (string= exwm-class-name "URxvt")
                (setq default-directory
                      (and (string-match " " exwm-title)
                           (concat (substring-no-properties
                                    exwm-title
                                    (match-end 0))
                                   "/")))))))

(use-package! orderless
  :custom (completion-styles '(orderless))
  :defer-incrementally t
  :config
  ;; (after! ivy (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))
  (after! company
    (defun just-one-face (fn &rest args)
      (let ((orderless-match-faces [completions-common-part]))
        (apply fn args)))

    (advice-add 'company-capf--candidates :around #'just-one-face))
  (setq orderless-matching-styles
      '(orderless-flex
        orderless-strict-leading-initialism
        orderless-regexp
        orderless-prefixes
        orderless-literal)))

(require 'show-eol)
(require 'feebleline)
(require 's)
(require 'dash)

(defun jcs-current-major-mode ()
  "Get current major mode."
  major-mode)

(defun jcs--feebleline--symbol-read-only ()
  "Feebleline read-only symbol."
  (when buffer-read-only
    "R"))

(defun jcs--feebleline--project-name ()
  "Feebleline project name."
  (let* ((current-project (project-current))
         (project-root (and (listp current-project)
                            (cdr current-project))))
    (when (and project-root
               (buffer-file-name))
      (concat " - " (file-name-nondirectory (directory-file-name project-root))))))

(defun jcs--feebleline--coding-system ()
  "Feebleline coding system."
  (unless (derived-mode-p 'exwm-mode 'pdf-view-mode)
    buffer-file-coding-system))

(defun jcs--feebleline--time ()
  "Feebleline time."
  (format-time-string "[%Y-%m-%d %H:%M:%S]"))

(defun oof-feebleline-systray-padding ()
  (when (bound-and-true-p exwm-systemtray--list)
    (make-string (max (oof-systray-padding--compute-size (length exwm-systemtray--list))
                      0)
                 ?\s)))

(defun oof-systray-padding--compute-size (n)
  (declare (pure t) (side-effect-free t))
  (pcase n
    (7 (1+ (- (* 3 n) 2)))
    (8 (- (* 3 n) 4))
    (t (- (* 3 n) 2))))
;; (oof-systray-padding--compute-size 7)

(defun oof-objed-modeline-string ()
  (when (featurep 'objed)
    (format " %s(%s) "
            (symbol-name objed--object)
            (char-to-string
             (aref
              (symbol-name objed--obj-state)
              0)))))

(defun oof-pdf-position ()
  (when-let ((page (and (eq major-mode 'pdf-view-mode)
                        (ignore-errors (pdf-view-current-page))))
             (max-page (pdf-cache-number-of-pages)))
    (format "%5s/%-2s"
            (concat " P" (number-to-string page))
            (or (number-to-string max-page) "???"))))

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

(defun oof-pomodoro ()
  (when (and (bound-and-true-p org-pomodoro-mode-line)
             (listp org-pomodoro-mode-line))
    (s-trim (apply #'concat org-pomodoro-mode-line))))

(defun oof-org-clock (&optional refresh)
  (if (bound-and-true-p org-clock-current-task)
      (progn (if org-clock-effort
                 (org-clock-notify-once-if-expired)
               (setq org-clock-task-overrun nil))
             (when refresh (setq org-clock-heading (org-clock--mode-line-heading)))
             (if (and org-clock-task-overrun
                      (stringp org-clock-task-overrun-text))
                 (concat org-clock-task-overrun-text
                         (pomodorofy-org-clock-string (org-clock-get-clock-string))))
             (pomodorofy-org-clock-string (org-clock-get-clock-string)))
    ;; Show break time if on break
    (oof-pomodoro)))

(defun oof-org-noter ()
  (when (featurep 'org-noter)
    (org-noter--mode-line-text)))


(defun pomodorofy-org-clock-string (s)
  (cond ((and (featurep 'org-pomodoro)
              (org-pomodoro-remaining-seconds))
         (replace-regexp-in-string "\\[[0-9]:[0-9][0-9]\\]" (oof-pomodoro) s))
        ((not s)
         (oof-pomodoro))
        (t s)))

(defun oof-appt ()
  (when (bound-and-true-p appt-mode-string)
    (s-trim appt-mode-string)))

(defun oof-whitespace-padding ()
  (when (bound-and-true-p perfect-margin-mode)
    (make-string (car (perfect-margin--init-window-margins))
                 ?\s)))

(defun oof-file-or-buffer-name ()
  "Current file, or just buffer name if not a file."
  (->> (if (buffer-file-name)
           (file-name-nondirectory (buffer-file-name))
         (buffer-name))
       (string-remove-suffix (concat " - Mozilla Firefox"))))

(setq feebleline-msg-functions
      '(;; (oof-whitespace-padding)
        (feebleline-buffer-position)
        (oof-org-noter :face org-noter-notes-exist-face)
        (oof-objed-modeline-string :face objed-mode-line)
        (feebleline-file-directory :face feebleline-dir-face :post "")
        (oof-file-or-buffer-name :face font-lock-keyword-face :post "")
        (feebleline-file-modified-star :face font-lock-warning-face :post "")
        (feebleline-git-branch :face feebleline-git-face :pre " - ")
        (oof-org-clock :align right)
        (oof-appt :align right :pre "( " :post " )" :face warning)
        (oof-mu4e-alert-unread-emails :align right)
        (oof-emms-feebleline :align right)
        (oof-emms-playing-time :pre "[" :post "] " :align right)
        (jcs--feebleline--time :align right)
        (jcs--feebleline--coding-system :pre "[" :post "] " :align right)
        (oof-feebleline-systray-padding :align right)
        (oof-feebleline-rsync-status)))

(when (featurep! :ui popup)
  (setq +popup-buffer-mode-hook
        (cl-set-difference +popup-buffer-mode-hook
                           '(+popup-set-modeline-on-enable-h
                             +popup-unset-modeline-on-disable-h)))
  (add-hook '+popup-buffer-mode-hook #'hide-mode-line-mode))

(feebleline-mode 1)

(use-package! anzu
  :defer-incrementally t
  :commands (anzu-query-replace
             anzu-query-replace-regexp)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         (:map isearch-mode-map
          ([remap isearch-query-replace] . anzu-isearch-query-replace)
          ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))))

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

(use-package! page-break-lines
  :after-call after-find-file
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

(use-package! focus
  :bind (:map doom-leader-map
         ("t3" . focus-mode))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "focus-mode") . (nil . "Focus"))))
  (defun focus/face ())
  (defun focus/hook ()
    (dolist (hook '(c-mode-common-hook
                    ;; clojure-mode-hook
                    ;; cider-repl-mode-hook
                    ;; emacs-lisp-mode-hook
                    ;; lisp-interaction-mode-hook
                    python-mode-hook
                    LaTeX-mode-hook
                    org-mode-hook))
      (add-hook hook (lambda ()
                       (add-hook 'after-change-functions
                                 #'focus-mode-enable
                                 t t))))
    (dolist (target '(switch-to-buffer
                      pop-to-buffer
                      other-window
                      select-frame-set-input-focus
                      other-frame))
      (advice-add target :before #'focus-mode-disable))
    (advice-add 'undo :after
                (lambda (&rest _)
                  (unless (buffer-modified-p)
                    (focus-mode-disable))))
    (eval-after-load "undo-tree"
      '(advice-add 'undo-tree-undo :after
                   (lambda (&rest _)
                     (unless (buffer-modified-p)
                       (focus-mode-disable)))))
    (advice-add 'keyboard-quit :before
                (lambda (&rest _)
                  (focus-mode-disable)))
    (add-hook 'after-save-hook #'focus-mode-disable)
    (run-with-idle-timer 5 t #'focus-mode-disable))
  (defun focus/key-bind ()
    (unbind-key "C-c C-q" focus-mode-map))
  (defun focus/setting ()
    ;; (setq text-scale-mode-step 1.05)
    (add-to-list 'focus-mode-to-thing '(tex-mode . page))
    (add-to-list 'focus-mode-to-thing '(org-mode . org))
    ;; (add-to-list 'focus-mode-to-thing '(clojure-mode . list+))
    ;; (add-to-list 'focus-mode-to-thing '(emacs-lisp-mode . list+))
    (put 'org 'bounds-of-thing-at-point
         (lambda ()
           (save-excursion
             (let ((start (progn
                            (outline-previous-heading)
                            (point)))
                   (end (progn
                          (outline-next-visible-heading 1)
                          (beginning-of-line)
                          (point))))
               (cons start end)))))
    (put 'tex-sentence 'bounds-of-thing-at-point
         (lambda ()
           (let* ((regx (concat "^\\(?:[[:cntrl:]]\\)*$"))
                  (start (save-excursion
                           (backward-char)
                           (re-search-backward regx nil t)
                           (point)))
                  (end (save-excursion
                         (forward-char)
                         (re-search-forward regx nil t)
                         (point))))
             (cons start end))))
    (put 'list+ 'bounds-of-thing-at-point (get 'list 'bounds-of-thing-at-point))
    ;; (eval-after-load "smartparens"
    ;;   '(put 'list+ 'bounds-of-thing-at-point
    ;;         (lambda ()
    ;;           (save-excursion
    ;;             (let ((start (progn
    ;;                            (ignore-errors
    ;;                              (when (sp-point-in-string)
    ;;                                (sp-backward-up-sexp))
    ;;                              (backward-up-list 2))
    ;;                            (point)))
    ;;                   (end (progn
    ;;                          (forward-list)
    ;;                          (point))))
    ;;               (cons start end))))))
    )
  (defvar focus-mode-unsupported-minor-modes '(git-timemachine-mode multiple-cursors-mode))
  (defun focus-mode-possible? ()
    (not (--some (and (boundp it) it) focus-mode-unsupported-minor-modes)))
  (defun focus-mode-enabled? ()
    focus-mode)
  (defun focus-mode-enable (&rest _)
    (when (and (eq (window-buffer)
                   (current-buffer))
               (focus-mode-possible?)
               (not (focus-mode-enabled?)))
      ;; (ignore-errors
      ;;   (text-scale-increase 0)
      ;;   (text-scale-increase 1))
      (focus-mode 1)))
  (defun focus-mode-disable (&rest _)
    (when (focus-mode-enabled?)
      ;; (ignore-errors
      ;;   (text-scale-increase 0))
      (focus-mode 0)))
  :config
  (focus/face)
  ;; (focus/hook)
  (focus/key-bind)
  (focus/setting))

(after! avy
  (setq avy-all-windows t
        avy-timeout-seconds 0.3
        avy-single-candidate-jump t
        avy-keys '(?a ?r ?s ?t ?i
                      ?e ?n ?g ?m
                      ?q ?w ?c ?p
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

(after! eldoc
  (global-eldoc-mode -1)
  (after! org
    (remove-hook 'org-mode-local-vars-hook #'eldoc-mode)))

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
  :defer-incrementally t
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
               (unless arg (kill-current-buffer)))
      (save-buffer)))

  (defun abbrev-unignore-case-advice (oldfun &rest args)
    (let ((result (apply oldfun args)))
      (when (string-equal (car result) (cadr result))
        result)))

  (advice-add #'abbrev--before-point :around #'abbrev-unignore-case-advice)

  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

  (map! :leader :prefix "f"
        :desc "save file" :nmv "s" #'save-abbrevs-or-buffer)
  (map! :leader
        :desc "edit abbrevs" :nmv "A" #'edit-abbrevs)

  (map! :map edit-abbrevs-mode-map
        "C-c C-k" #'kill-current-buffer
        "C-c C-c" (cmd! (abbrev-edit-save-buffer)
                           (kill-current-buffer)))

  (abbrev-table-put global-abbrev-table :case-fixed t)
  (after! nix-mode
    (abbrev-table-put nix-mode-abbrev-table :case-fixed t)))

(add-hook 'minibuffer-setup-hook
          (lambda (&rest _)
            (when (eq this-command 'eval-expression)
              (abbrev-mode))))

;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
(after! abbrev
  (defcustom abbrev-additional-chars
    '((t ?-)
      (org-mode ?> ?<))
    "Alist that maps major mode symbols to lists of characters that may appear in abbreviations.
The chars of the special major mode symbol `t' are active in all modes."
    :group 'abbrev
    :type '(repeat :tag "List of modes"
                   (cons :tag "Map major mode symbols to lists of additional chars in abbrevs"
                         (symbol :tag "Mode symbol (`t' stands for all modes)")
                         (repeat :tag "List of additional word-consistent characters" character))))

  (defvar-local T-abbrev-syntax-table nil
    "List of additional characters in abbreviations.")

  (defun T-abbrev-mode-hook-fun ()
    "Populate T-abbrev-syntax-table with the local syntax table modfied by
the characters in `abbrev-additional-chars'."
    (when abbrev-mode
      (let ((char-list (append (cdr (assoc major-mode abbrev-additional-chars))
                               (cdr (assoc 't abbrev-additional-chars)))))
        (setq T-abbrev-syntax-table (make-syntax-table (syntax-table)))
        (mapcar (lambda (char)
                  (modify-syntax-entry char "w" T-abbrev-syntax-table))
                char-list))))

  ;; Wrapping functions of the `abbrev` package with the local syntax table.
  ;; I'm not sure I captured all fun's that need to run with the local syntax-table.
  ;; Adding further functions is easy.
  ;; Just add them to the list at the end of the next form.
  (mapcar
   (lambda (fun)
     (let ((newfun (intern (concat "T-ad-" (symbol-name fun)))))
       (eval
        `(progn
           (defun ,newfun (oldfun &rest args)
             ,(concat "This function evaluates `" (symbol-name fun) "' with `T-abbrev-syntax-table' as active syntax table.
It is used for the advicing `" (symbol-name fun) "'.")
             (if (syntax-table-p T-abbrev-syntax-table)
                 (with-syntax-table T-abbrev-syntax-table
                   (apply oldfun args))
               (apply oldfun args)))
           (advice-add (quote ,fun) :around (quote ,newfun))))))
   '(define-mode-abbrev abbrev--before-point))

  (add-hook 'abbrev-mode-hook #'T-abbrev-mode-hook-fun))

(el-patch-defun abbrev--before-point ()
  "Try and find an abbrev before point.  Return it if found, nil otherwise."
  (unless (eq abbrev-start-location-buffer (current-buffer))
    (setq abbrev-start-location nil))

  (let ((tables (abbrev--active-tables))
        (pos (point))
        start end name res)

    (if abbrev-start-location
        (progn
          (setq start abbrev-start-location)
          (setq abbrev-start-location nil)
          ;; Remove the hyphen inserted by `abbrev-prefix-mark'.
          (when (and (< start (point-max))
                     (eq (char-after start) ?-))
            (delete-region start (1+ start))
            (setq pos (1- pos)))
          (skip-syntax-backward " ")
          (setq end (point))
          (when (> end start)
            (setq name (buffer-substring start end))
            (goto-char pos)               ; Restore point.
            (list (abbrev-symbol name tables) name start end)))

      (while (and tables (not (car res)))
        (let* ((table (pop tables))
               (enable-fun (abbrev-table-get table :enable-function)))
          (setq tables (append (abbrev-table-get table :parents) tables))
          (setq res
                (and (or (not enable-fun) (funcall enable-fun))
                     (let ((re (abbrev-table-get table :regexp)))
                       (if (null re)
                           ;; We used to default `re' to "\\<\\(\\w+\\)\\W*"
                           ;; but when words-include-escapes is set, that
                           ;; is not right and fixing it is boring.
                           (let ((lim (point)))
                             (backward-word 1)
                             (setq start (point))
                             (forward-word 1)
                             (setq end (min (point) lim)))
                         (when (el-patch-swap (looking-back re (line-beginning-position))
                                              (looking-back re (max (1- (line-beginning-position))
                                                                    (point-min))))
                           (setq start (match-beginning 1))
                           (setq end   (match-end 1)))))
                     (setq name  (buffer-substring start end))
                     (let ((abbrev (abbrev--symbol name table)))
                       (when abbrev
                         (setq enable-fun (abbrev-get abbrev :enable-function))
                         (and (or (not enable-fun) (funcall enable-fun))
                              ;; This will also look it up in parent tables.
                              ;; This is not on purpose, but it seems harmless.
                              (list abbrev name start end))))))
          ;; Restore point.
          (goto-char pos)))
      res)))

(autoload 'arduino-mode "arduino-mode" "Major mode for editing Arduino code." t)
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))

(after! alert
  (setq alert-default-style 'libnotify))

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
        :n "P" #'eww-reload
        :n ";" #'ace-link))

(after! eww
  (setq eww-bookmarks-directory
        (expand-file-name ".local/eww-bookmarks" doom-private-dir)
        eww-download-directory
        (expand-file-name "~/downloads")))

(use-package! keyfreq
  :after-call post-command-hook
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package! auto-capitalize
  :hook (org-mode . turn-on-auto-capitalize-mode)
  :config
  (setq auto-capitalize-aspell-file ispell-personal-dictionary))

(use-package! sunrise-commander
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

(use-package! font-lock-studio
  :commands font-lock-studio)

(when (featurep! :checkers spell)
  (after! ispell
    (setq ispell-quietly t
          ispell-dictionary "en_US"
          ispell-complete-word-dict "~/.doom.d/dict/english-words.txt"
          ispell-personal-dictionary "~/.doom.d/.aspell.en.pws")
    (advice-add #'ispell-init-process :around #'doom-shut-up-a))
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
    (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src"))
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
  :bind (:map doom-leader-map
         ("alW" . wordnut-search))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "wordnut-search") . (nil . "Wordnut search"))))
  :config
  (map! :map wordnut-mode-map
        :nmv "q" #'quit-window))

(use-package! synosaurus
  :bind (:map doom-leader-map
         ("alt" . synosaurus-lookup))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "synosaurus-lookup") . (nil . "Thesaurus"))))
  :config
  (map! :map synosaurus-list-mode-map
        :nmv "q" #'quit-window))

(use-package! helm-wikipedia
  :commands (helm-wikipedia-suggest)
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "helm-wikipedia") . (nil . "Wikipedia lookup"))))
  :bind (:map doom-leader-map
         ("alw" . helm-wikipedia-suggest)))

(when (featurep! :tools lookup)
  (defun +lookup--online-backend-github-code-search (query)
    (funcall +lookup-open-url-fn
             (let* ((name "GitHub code search")
                    (language (cl-case major-mode
                                ((emacs-lisp-mode inferior-emacs-lisp-mode) "Emacs Lisp")
                                (nix-mode "Nix")
                                (python-mode "Python")
                                ;; more here ...
                                (t (completing-read (concat name " language: ")
                                                    ;; List of programming languages
                                                    '("Emacs Lisp"
                                                      "Python"
                                                      "Nix")))))
                    (symbol (completing-read
                             (concat name ": ")
                             (pcase language
                               ("Emacs Lisp" obarray)
                               (t nil))
                             nil
                             nil
                             query)))
               (cond (language (concat "https://github.com/search?l="
                                       (replace-regexp-in-string "[[:space:]]+" "+" language)
                                       "&q=" symbol "&type=Code"))
                     (t (format "https://github.com/search?l=%s&type=Code&q="
                                symbol)))))
    ;; HACK Do not ask for further input
    "")

  (add-to-list '+lookup-provider-url-alist
               (list "GitHub code search" #'+lookup--online-backend-github-code-search)))

(after! exwm
  (defun use-active-browser-a (oldfun &rest args)
    (let ((browse-url-generic-program
           (or (car (cl-member (exwm--app-name) '("qutebrowser" "firefox")
                               :test #'cl-equalp))
               browse-url-generic-program))
          (browse-url-browser-function (cond ((and exwm-class-name
                                                   browse-url-generic-program)
                                              #'browse-url-generic)
                                             ((eq major-mode 'w3m-mode)
                                              #'w3m-browse-url)
                                             ((eq major-mode 'eww-mode)
                                              #'eww-browse-url)
                                             (t browse-url-browser-function))))
      (apply oldfun args)))

  (advice-add #'webjump :around #'use-active-browser-a)
  (defadvice +lookup/online (around use-active-browser-a activate)
    (let ((browse-url-generic-program
           (or (car (cl-member (exwm--app-name) '("qutebrowser" "firefox")
                               :test #'cl-equalp))
               browse-url-generic-program))
          (browse-url-browser-function (cond ((and exwm-class-name
                                                   browse-url-generic-program)
                                              #'browse-url-generic)
                                             ((eq major-mode 'w3m-mode)
                                              #'w3m-browse-url)
                                             ((eq major-mode 'eww-mode)
                                              #'eww-browse-url)
                                             (t browse-url-browser-function))))
      ad-do-it)))

(advice-add #'+lookup/online :around
            (defun +lookup--use-completing-read-a (oldfun &rest args)
              (advice-add #'read-string :override #'completing-read)
              (apply oldfun args)
              (advice-remove #'read-string #'completing-read)))

(use-package! ellocate
  :bind (:map doom-leader-map
         ("sf" . ellocate)
         ("sF" . ellocate-all))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "ellocate") . (nil . "Locate file in current directory")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "ellocate-all") . (nil . "Locate file in database"))))
  :config
  (setq ellocate-scan-dirs
        `(("~/" ,(expand-file-name ".local/ellocate-home-db" doom-private-dir))
          ("/mnt/" nil))))

(use-package! helm-systemd
  :bind (:map doom-leader-map
         ("a9" . helm-systemd))
  :config
  (require 'hi-lock)
  (require 'helm-bookmark)
  (setq helm-systemd-list-all t
        helm-systemd-list-not-loaded t))

(use-package! helm-sys
  :bind (:map doom-leader-map
         ("a8" . helm-top)))

(use-package! steam
  :bind (:map doom-leader-map
         ("a4" . steam-launch))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "steam-launch") . (nil . "Steam"))))
  :config
  (el-patch-defun steam-get-xml ()
    "Downloads the user's games as XML."
    (with-current-buffer
        (url-retrieve-synchronously (el-patch-swap (format "http://steamcommunity.com/id/%s/games?tab=all&xml=1"
                                                           (url-hexify-string steam-username))
                                                   "http://steamcommunity.com/profiles/76561198052660778/games?tab=all&xml=1"))
      (goto-char url-http-end-of-headers)
      (let*
            ((response (car (xml-parse-region (point) (point-max))))
             (error-detected (steam-check-xml-response response)))
        (if (not error-detected)
              (progn
                (message "Retrieved games successfully")
                (car (xml-get-children response 'games)))
            (message error-detected)
            nil)))))

(use-package! elcord
  :defer-incrementally t
  :config
  (elcord-mode 1)
  (advice-add #'elcord--buffer-boring-p
              :around
              (defun elcord--ignore-some-roam-buffers (oldfun buffer-name)
                (or (funcall oldfun buffer-name)
                    (doom-unreal-buffer-p buffer-name)
                    (let ((buf (get-buffer buffer-name)))
                      (and (eq (buffer-local-value 'major-mode buf) 'org-mode)
                           (stringp (buffer-local-value 'buffer-file-name buf))
                           (f-file-p (buffer-local-value 'buffer-file-name buf))
                           (file-in-directory-p (buffer-local-value 'buffer-file-name buf) org-roam-directory)
                           (with-current-buffer buf
                             (cl-member "Person"
                                        (org-roam--extract-tags)
                                        :test
                                        #'cl-equalp))))))))

(use-package! emms
  :bind ((:map doom-leader-map
          ("a2" . emms)))
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
  ;; (emms-history-load)
  (emms-default-players)
  (after! (:all dired evil)
    (map! :map dired-mode-map
          :n "@" #'emms-play-dired)))

(after! dired-x
  (add-to-list 'dired-guess-shell-alist-user
               (list "\\.\\(flac\\|mp3\\|ogg\\|wav\\|opus\\)\\'"
                     '(if (y-or-n-p "Add to emms playlist?")
                          (progn (emms-add-file (dired-get-filename))
                                 (keyboard-quit))
                        "mpv"))))

(after! (:all persp-mode emms)
  (persp-def-auto-persp
   "EMMS"
   :hooks '(doom-switch-buffer-hook)
   :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
              (persp-add-buffer-on-find-file nil)
              persp-add-buffer-on-after-change-major-mode)
   :switch 'window
   :predicate (lambda (buffer &optional state)
                (and (string-prefix-p "emms-" (symbol-name major-mode))
                     (or state t)))
   :after-match (lambda (buffer &rest _)
                  (let* ((buffer (alist-get 'buffer state)))
                    (+workspace-switch "EMMS")
                    (+workspace/display)
                    (switch-to-buffer buffer)))))

(after! persp-mode
  (setq persp-auto-save-opt 0
        persp-auto-save-persps-to-their-file nil))

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
        helm-emms-default-sources '(helm-source-emms-files
                                    helm-source-emms-streams
                                    helm-source-emms-dired)))

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

(after! emms
  (use-package! emms-get-lyrics
    :commands (emms-get-lyrics)))

(use-package! picpocket
  :defer-incrementally t
  :config
  (evil-set-initial-state 'picpocket-mode 'emacs))

(use-package! epaint
  :config
  (setq epaint-plug-in-default-directory
        (expand-file-name "straight/repos/epaint/plug-ins" doom-local-dir))
  (when (featurep! :editor evil)
    (map! :map epaint-mode-map
          :e "SPC" #'doom/leader)
    (evil-set-initial-state 'epaint-mode 'emacs)))

(use-package! mpv
  :config
  (defvar mpv-ipc-socket (make-temp-name
                          (expand-file-name "mpv-" temporary-file-directory)))
  (el-patch-defun mpv-start (&rest args)
    "Start an mpv process with the specified ARGS.

If there already is an mpv process controlled by this Emacs instance,
it will be killed.  Options specified in `mpv-default-options' will be
prepended to ARGS."
    (mpv-kill)
    (el-patch-swap
      (let ((socket (make-temp-name
                     (expand-file-name "mpv-" temporary-file-directory))))
        (setq mpv--process
              (apply #'start-process "mpv-player" nil mpv-executable
                     "--no-terminal"
                     (concat "--input-unix-socket=" socket)
                     (append mpv-default-options args)))
        (set-process-query-on-exit-flag mpv--process nil)
        (set-process-sentinel
         mpv--process
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (mpv-kill)
             (when (file-exists-p socket)
               (with-demoted-errors (delete-file socket)))
             (run-hooks 'mpv-on-exit-hook))))
        (while (and (mpv-live-p) (not (file-exists-p socket)))
          (sleep-for 0.05))
        (setq mpv--queue (tq-create
                          (make-network-process :name "mpv-socket"
                                                :family 'local
                                                :service socket)))
        (set-process-filter
         (tq-process mpv--queue)
         (lambda (_proc string)
           (mpv--tq-filter mpv--queue string)))
        (run-hook-with-args 'mpv-on-start-hook args)
        t)
      (progn 
        (setq mpv--process
              (apply #'start-process "mpv-player" nil mpv-executable
                     "--no-terminal"
                     (concat "--input-unix-socket=" mpv-ipc-socket)
                     (append mpv-default-options args)))
        (set-process-query-on-exit-flag mpv--process nil)
        (set-process-sentinel
         mpv--process
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (mpv-kill)
             (run-hooks 'mpv-on-exit-hook))))
        (while (and (mpv-live-p) (not (file-exists-p mpv-ipc-socket)))
          (sleep-for 0.05))
        (setq mpv--queue (tq-create
                          (make-network-process :name "mpv-socket"
                                                :family 'local
                                                :nowait t
                                                :coding '(utf-8 . utf-8)
                                                :noquery t
                                                :service mpv-ipc-socket)))
        (set-process-filter
         (tq-process mpv--queue)
         (lambda (_proc string)
           (mpv--tq-filter mpv--queue string)))
        (run-hook-with-args 'mpv-on-start-hook args)
        t))))

(use-package! vuiet
  :bind (:map doom-leader-map
         ;; ("avl" . vuiet-play-track-by-lyrics)
         ("ava" . vuiet-artist-info-search)
         ("avb" . vuiet-album-info-search)
         ("avs" . vuiet-play-track-search)
         ("avt" . vuiet-play-tag-similar)
         ("avx" . vuiet-stop)
         ("avl" . vuiet-love-track)
         ("avp" . vuiet-play-pause))
  :config
  (defun my-vuiet-buffer-hook ()
    (when (bound-and-true-p flyspell-mode) (flyspell-mode -1))
    (when (bound-and-true-p writegood-mode) (writegood-mode -1))
    (local-set-key (kbd "n") #'org-next-link)
    (local-set-key (kbd "e") #'org-previous-link)
    (local-set-key (kbd "o") #'ace-link-org)
    (when (featurep! :editor evil)
      (evil-emacs-state)))

  (el-patch-defun vuiet--play-track (track)
    "Play the TRACK in the background with mpv and ytdl."
    (mpv-kill)
    (sleep-for 0.05)
    (mpv-start
     "--no-video"
     (format "ytdl://ytsearch:%s" (vuiet--track-as-string track))))

  (el-patch-defun vuiet-album-info (artist album)
    "Display info about the ARTIST's ALBUM in a new buffer.

s   choose a song with ivy.
a   pick another album with ivy.
p   play all songs from the album.
l   save lyrics for this album."
    (vuiet--with-vuiet-buffer
     (el-patch-swap (format "%s - %s" artist album)
                    (format "*vuiet: %s - %s" artist album "*"))
     (let* ((songs (lastfm-album-get-info artist album))
            ;; Align song duration in one nice column. For this, I need to know
            ;; the longest song name from the album.
            (max-len (cl-loop for entry in songs
                              maximize (length (cadr entry)))))

       (insert (format "* %s - %s \n\n" artist album))
       (cl-loop for i from 1
                for entry in songs
                for song = (cadr entry)
                for duration = (format-seconds
                                "%m:%02s" (string-to-number (caddr entry)))
                do (insert
                    (format (concat "%2s. [[elisp:(vuiet-play '(\"%s\" \"%s\"))][%-"
                                    (number-to-string (1+ max-len))
                                    "s]] %s\n")
                            i artist song song duration)))

       (vuiet--local-set-keys
        ("s" . (vuiet--ivy-play-song songs))
        ("a" . (vuiet-album-info-search artist)) ;try another album.
        ("p" . (vuiet-play songs))
        ("l" . (versuri-save-bulk songs 10))
        (el-patch-add ("C-`" . (+popup/toggle))))
       (el-patch-add (my-vuiet-buffer-hook)))))

  (el-patch-defun vuiet-artist-info (artist)
    "Display info about ARTIST in a new buffer.

p   play all the artist songs, sequentially.
s   select and display info for a similar artist with ivy.
l   visit the artist's lastfm page."
    (interactive "sArtist: ")
    (vuiet--with-vuiet-buffer (el-patch-swap artist
                                             (concat "*vuiet: "
                                                     artist
                                                     "*"))
                              (let* ((artist-info (lastfm-artist-get-info artist))
                                     (songs (lastfm-artist-get-top-tracks
                                             artist
                                             :limit vuiet-artist-tracks-limit))
                                     (bio-summary (car artist-info))
                                     ;; The subseq indices are based on the standard lastfm.el response
                                     ;; for artist.info
                                     (similar-artists (cl-subseq artist-info 3 7))
                                     (tags (cl-subseq artist-info 8 12)))
                                (insert (format "* %s\n\n %s"
                                                artist
                                                (s-word-wrap 75 (replace-regexp-in-string
                                                                 "<a.*a>" "" bio-summary))))

                                (insert "\n\n* Similar artists: \n")
                                (dolist (artist similar-artists)
                                  (insert (format "|[[elisp:(vuiet-artist-info \"%s\")][%s]]| "
                                                  artist artist)))

                                (insert "\n\n* Popular tags: \n")
                                (dolist (tag tags)
                                  (insert (format "|[[elisp:(vuiet-tag-info \"%s\")][%s]]| "
                                                  tag tag)))

                                (insert "\n\n* Top Songs: \n")
                                (cl-loop for i from 1
                                         for song in songs
                                         do (insert
                                             (format "%2s. [[elisp:(vuiet-play '(\"%s\" \"%s\"))][%s]]\n"
                                                     i artist (cadr song) (cadr song))))

                                (vuiet--local-set-keys
                                 ("p" . (vuiet-play songs))
                                 ("s" . (vuiet-ivy-similar-artists artist))
                                 ("l" . (vuiet-artist-lastfm-page artist))
                                 (el-patch-add ("C-`" . (+popup/toggle))))
                                (el-patch-add (my-vuiet-buffer-hook)))))

  (when (featurep! :ui popup)
    (el-patch-defmacro vuiet--with-vuiet-buffer (name &rest body)
      "Basic setup for a VUIET-MODE buffer.
Create a new buffer with name NAME if it does not exist.  Turn
on VUIET-MODE for that buffer, eval BODY and then switch to it."
      (declare (debug t)
               (indent defun))
      (let ((b (make-symbol "buffer")))
        `(aif (get-buffer ,name)
             ;; Don't create a new buffer if one already exists.
             (el-patch-swap (switch-to-buffer it)
                            (progn (pop-to-buffer it)
                                   (+popup-shrink-to-fit)))
           (let ((,b (generate-new-buffer ,name)))
             (with-current-buffer ,b
               (vuiet-mode)
               ,@body)
             (el-patch-swap (switch-to-buffer ,b)
                            (progn (pop-to-buffer ,b)
                                   (+popup-shrink-to-fit)))
             (org-previous-visible-heading 1)))))

    (set-popup-rule! "vuiet:" :size 0.6 :quit t :focus t)))

(use-package! somafm
  :defer-incrementally t
  :config
  (map! :map somafm-mode-map
        :n "r" #'somafm--refresh-channels
        :n "R" #'somafm--refresh-and-show-channels-buffer
        :n "s" #'somafm--sort
        :n "x" #'somafm--stop)
  (after! evil-snipe
    (add-to-list 'evil-snipe-disabled-modes 'somafm-mode)))

(when (featurep! :ui workspaces)
  (after! persp-mode
    (defvar +somafm-workspace-name "*somafm*")

    (defun +somafm ()
      (interactive)
      (+workspace-switch +somafm-workspace-name t)
      (+workspace/display)
      (somafm))

    (defun +somafm-quit ()
      (interactive)
      (+workspace-delete +somafm-workspace-name)
      (+workspace/other))

    (map! (:after somafm
           (:map somafm-mode-map
            :n "q" #'+somafm-quit))
          (:leader :desc "Somafm" :n "a@" #'+somafm))))

(use-package! leetcode
  :commands leetcode
  :config
  (setq leetcode-prefer-language "python3"
        leetcode-prefer-sql "mysql")
  (define-key leetcode--problems-mode-map (kbd "TAB") 'leetcode-show-current-problem)
  (define-key leetcode--problems-mode-map (kbd "<return>") 'leetcode-show-current-problem))

(use-package! alarm-clock
  :bind (:map doom-leader-map
         ("aa" . alarm-clock-set)
         ("aA" . alarm-clock-list-view))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "alarm-clock-set") . (nil . "Set alarm")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "alarm-clock-list-view") . (nil . "View alarms"))))
  :config
  (evil-set-initial-state 'alarm-clock-mode 'emacs)
  (alarm-clock--turn-autosave-on)
  (setq alarm-clock-cache-file (expand-file-name ".local/cache/alarm-clock.cache"
                                                 doom-private-dir)))

(use-package! helm-linux-disks
  :bind (:map doom-leader-map
         ("aD" . helm-linux-disks))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "helm-linux-disks") . (nil . "Disk management")))))

(use-package! elescope
  :bind (:map doom-leader-map
         ("aC" . elescope-checkout))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "elescope-checkout") . (nil . "Clone GitHub repo"))))
  :config
  (setq elescope-root-folder "~/downloads"))

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
      :desc "Virtual Box" "aV" #'counsel-virtualbox)

(use-package! w3m
  :bind (:map doom-leader-map
         ("a3" . +w3m))
  :init
  (provide 'tab-line)
  :config
  ;; (load (expand-file-name "w3m-type-ahead" doom-private-dir) t t)
  ;; (add-hook 'w3m-mode-hook #'w3m-type-ahead-mode)
  (setq w3m-default-display-inline-images t))

(after! w3m
  (after! shrface
    (map! :map w3m-mode-map
          :n "J" #'org-next-visible-heading
          :n "K" #'org-previous-visible-heading))
  (map! :map w3m-mode-map
        :n "s" #'+lookup/online
        :n "S" (lambda (query provider)
                 (interactive
                  (list (if (use-region-p) (doom-thing-at-point-or-region))
                        (+lookup--online-provider current-prefix-arg)))
                 (w3m-goto-new-session-url)
                 (+lookup/online query provider))
        :n ";" #'w3m-lnum-follow
        :n ":" #'w3m-lnum-universal
        :n "f" #'w3m-lnum-goto
        :n "yy" #'w3m-print-current-url
        :n "yi" #'w3m-print-this-image-url
        :n "y." #'w3m-lnum-print-this-url
        :n "r" #'w3m-redisplay-this-page
        :n "R" #'w3m-reload-this-page
        :n "x" #'w3m-delete-buffer
        :g "C-c C-r" #'wicked/w3m-reload-this-page-with-user-agent
        :n "q" #'+workspace/other)
  (after! evil-snipe
    (add-to-list 'evil-snipe-disabled-modes 'w3m-mode)))

(defvar +w3m-workspace-name "*w3m*")
(defvar +w3m--old-wconf nil)

(add-hook 'w3m-mode-hook #'+w3m-init-h)

(defun +w3m (&rest _)
  (interactive)
  (if (featurep! :ui workspaces)
      (progn (+workspace-switch +w3m-workspace-name t)
             (+workspace/display))
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
   ((and (featurep! :ui workspaces)
         (+workspace-exists-p +w3m-workspace-name)
         (not (--filter (eq (buffer-local-value 'major-mode it)
                            'w3m-mode)
                        (buffer-list))))
    (+workspace/delete +w3m-workspace-name))

   (+w3m--old-wconf
    (set-window-configuration +w3m--old-wconf)
    (setq +w3m--old-wconf nil))))

(advice-add #'w3m-browse-url
            :before
            #'+w3m)

(advice-add #'w3m-redisplay-pages-automatically
            :around
            (defun w3m--only-redisplay-in-workspace-a (oldfun &rest args)
              (when (equal (+workspace-current-name)
                           +w3m-workspace-name)
                (apply oldfun args))))

(defun +w3m-quit ()
  (interactive)
  (+workspace/delete +w3m-workspace-name))

(map! :after w3m
      :map w3m-mode-map
      :n "q" #'+w3m-quit)

(after! w3m
  (defvar wicked/w3m-fake-user-agents ;; (1)
    `(("w3m" . ,(concat "Emacs-w3m/" emacs-w3m-version " " w3m-version))
      ("ie6" . "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")
      ("ff3" . "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008070206 Firefox/3.0.1")
      ("ff2" . "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.13) Gecko/20080208 Firefox/2.0.0.13")
      ("ie7" . "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)")
      ("ie5.5" . "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98)")
      ("iphone" . "Mozilla/5.0 (iPhone; U; CPU iPhone OS 2_0 like Mac OS X; en-us) AppleWebKit/525.18.1 (KHTML, like Gecko) Version/3.1.1 Mobile/5A347 Safari/525.20")
      ("safari" . "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_2; en-us) AppleWebKit/525.13 (KHTML, like Gecko) Version/3.1 Safari/525.13")
      ("google" . "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"))
    "*Associative list of user agent names and strings.")

  (defvar wicked/w3m-fake-user-agent-sites ;; (2)
    '(("^https?://www\\.useragentstring\\.com" . "ff2"))
    "*Associative list of regular expressions matching URLs and the agent keyword or value.
 The first matching entry will be used.")

  (defun wicked/w3m-set-user-agent (agent)
    "Set the user agent to AGENT based on `wicked/w3m-fake-user-agents'.
 If AGENT is not defined in `wicked/w3m-fake-user-agents', it is used as the user agent.
 If AGENT is empty, the default w3m user agent will be used."
    (interactive
     (list
      (completing-read "User-agent [w3m]: "
                       (mapcar 'car wicked/w3m-fake-user-agents)
                       nil nil nil nil "w3m"))) ;; (3)
    (if agent
        (progn
          (setq w3m-user-agent
                (or
                 (and (string= agent "") (assoc "w3m" wicked/w3m-fake-user-agents)) ;; (4)
                 (cdr (assoc agent wicked/w3m-fake-user-agents)) ;; (5)
                 agent))                                         ;; (6)
          (setq w3m-add-user-agent t))
      (setq w3m-add-user-agent nil)))

  (defun wicked/w3m-reload-this-page-with-user-agent (agent)
    "Browse this page using AGENT based on `wicked/w3m-fake-user-agents'.
 If AGENT is not defined in `wicked/w3m-fake-user-agents', it is used as the user agent.
 If AGENT is empty, the default w3m user agent will be used."
    (interactive (list (completing-read "User-agent [w3m]: "
                                        (mapcar 'car wicked/w3m-fake-user-agents)
                                        nil nil nil nil "w3m")))
    (let ((w3m-user-agent w3m-user-agent)
          (w3m-add-user-agent w3m-add-user-agent))
      (wicked/w3m-set-user-agent agent) ;; (7)
      (w3m-reload-this-page)))

  (defadvice w3m-header-arguments (around wicked activate) ;; (8)
    "Check `wicked/w3m-fake-user-agent-sites' for fake user agent definitions."
    (let ((w3m-user-agent w3m-user-agent)
          (w3m-add-user-agent w3m-add-user-agent)
          (sites wicked/w3m-fake-user-agent-sites))
      (while sites
        (if (string-match (caar sites) (ad-get-arg 1))
            (progn
              (wicked/w3m-set-user-agent (cdar sites))
              (setq sites nil))
          (setq sites (cdr sites))))
      ad-do-it)))

(after! w3m
  (defvar total-recall-directory
    (concat (getenv "HOME") "/.total-recall")
    "Directory where web pages will be kept.")

  (defvar total-recall-checksum-programs
    '("shasum" "gsha1sum" "sha1sum" "md5sum" "cksum")
    "List of possible checksum programs, in order of preference.
Feel free to add more!")



  ;; Zeroconf way of selecting an executable.

  (defun total-recall-select-executable (l)
    (if (null l)
        (error "No binary in list available!")
      (let ((executable (executable-find (car l))))
        (if executable
              executable
            (total-recall-select-executable (cdr l))))))

  (defvar total-recall-checksum-program
    ;; MAYBE FIXME: Maybe better if the binary is fixed after a first
    ;; inital selection - what if the user shares/syncs his
    ;; ~/.total-recall between several computers, and they have
    ;; different checksum programs installed!?  (Not a massive problem,
    ;; the user can later run a program which renames files according to
    ;; their (new) checksum.
    (total-recall-select-executable total-recall-checksum-programs)
    "Which binary to use for checksums.")

  ;; Create directory for saving history if it does not already exist.
  (unless (file-exists-p total-recall-directory)
    (make-directory total-recall-directory))

  ;; FIXME: bad style to use defvar here?
  (setq total-recall-tmp-file-name (concat total-recall-directory "/tc-test"))

  (defun total-recall-get-cksum ()
    "Compute cksum of file corresponding to temporarily saved buffer."
                                        ; Note: use as strong checksum algorithm as possible; say SHA or
                                        ; md5.  Likelyhood of adversary making html pages with intentional
                                        ; collisions is probably extremely small, but...
    (car (split-string
            (shell-command-to-string
             (concat total-recall-checksum-program " " total-recall-tmp-file-name)))))
  ;;JAVE TODO do a process here... async and works with tramp.
  ;; FIXME: hack away! :-)
  ;; Actually, is this worth doing?  The text is saved *after* async
  ;; downloading, and the actual saving and computing of the checksum
  ;; ought to be "instant".

  (defun total-recall-save-page (url)
                                        ; Dump web page text to file, compute its checksum, and rename file
                                        ; to checksum.

                                        ; Note: functions in w3m-display-hook are called with the url in
                                        ; question as an argument, but we don't use it.
    (let ((coding-system-for-write 'utf-8))
      (write-region nil nil total-recall-tmp-file-name t)
      (rename-file total-recall-tmp-file-name
                       (concat total-recall-directory "/"
                                 (total-recall-get-cksum))
                       t)))

  (add-hook 'w3m-display-hook 'total-recall-save-page))

(use-package! ivy-youtube
  :bind (:map doom-leader-map
         ("ay" . ivy-youtube-music)
         ("sy" . ivy-youtube))
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
      (emms-play-file
       (progn (goto-char (point-max))
              (string-remove-suffix
               " exists, skipping"
               (buffer-substring-no-properties
                (1- (point))
                (progn (forward-char -1)
                       (beginning-of-line)
                       (search-forward "/home/" nil t)
                       (forward-char (- (length "/home/")))
                       (point)))))))
    (setq ivy-youtube-music-only? nil))

  (defun ivy-youtube-music ()
    (interactive)
    (require 'emms)
    (setq ivy-youtube-music-only? t)
    (when (and (processp emms-player-mpv-proc)
               (process-live-p emms-player-mpv-proc))
      (kill-process emms-player-mpv-proc))
    (call-interactively #'ivy-youtube))

  ;; Set Youtube API key
  (load (expand-file-name "ivy-youtube" doom-private-dir) t t)

  (setq ivy-youtube-play-at (executable-find "mpv")))

(use-package! mentor
  :bind (:map doom-leader-map
         ("at" . mentor))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist '((nil . "mentor") . (nil . "rTorrent"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'mentor-mode 'emacs))
  (setq mentor-rtorrent-download-directory (expand-file-name "~/torrents")))

(when (featurep! :ui workspaces)
  (after! persp-mode
    (defun +mentor ()
      (interactive)
      (+workspace-switch "*mentor*" t)
      (+workspace/display)
      (mentor))

    (defun +mentor-quit ()
      (interactive)
      (+workspace-delete "*mentor*")
      (+workspace/other))

    (map! (:after mentor
           (:map mentor-mode-map
            "q" #'+mentor-quit))
          (:map doom-leader-map
           :desc "rTorrent" "at" #'+mentor))))

(use-package! forecast
  :bind (:map doom-leader-map
         ("aw" . forecast))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist '((nil . "forecast") . (nil . "Weather"))))
  :config
  (setq forecast-api-key "3952024acf85777d62f39869da12f853"
        forecast-units 'us
        forecast-language 'en)
  (map! :map forecast-mode-map
        :n "q" #'kill-current-buffer
        :n "r" #'forecast-refresh))

(when (featurep! :ui workspaces)
  (after! persp-mode
    (defun +forecast ()
      (interactive)
      (+workspace-switch "*forecast*" t)
      (+workspace/display)
      (forecast))

    (defun +forecast-quit ()
      (interactive)
      (+workspace-delete "*forecast*")
      (+workspace/other))

    (map! (:after forecast
           (:map forecast-mode-map
            :n "q" #'+forecast-quit))
          (:map doom-leader-map
           :desc "forecast" "aw" #'+forecast))))

(use-package! tldr
  :bind (:map doom-leader-map
         ("alc" . tldr))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "tldr") . (nil . "Community-driven manpages"))))
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/")))

(use-package! speed-type
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
  :bind (:map doom-leader-map
         ("ad" . disk-usage))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "disk-usage") . (nil . "Disk usage")))))

(use-package! spray
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

(use-package! sx
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

(use-package! paperless
  :bind (:map doom-leader-map
         ("ap" . paperless))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "paperless") . (nil . "Paperless"))))
  :custom
  (paperless-capture-directory "/home/tony/Paperless_Incoming/")
  (paperless-root-directory "/home/tony/documents")
  :config
  (el-patch-defun paperless ()
    "File directory contents."
    (interactive)
    (if (null paperless-capture-directory)
        (error "Set paperless-capture-directory with M-x customize-variable"))
    (if (null paperless-root-directory)
        (error "Set paperless-root-directory with M-x customize-variable"))
    (setq paperless--table-contents
          (mapcar
           (lambda (i)
             (list i (vector "" (file-name-nondirectory i) "")))
           (directory-files paperless-capture-directory t
                            (el-patch-swap ".*pdf"
                                           ".*pdf$"))))
    (pop-to-buffer (concat "*Paperless* - " paperless-capture-directory))
    (paperless-scan-directories)
    (paperless-mode)
    (tabulated-list-print t))

  (el-patch-defun paperless-display ()
    "Open a preview display for the current document."
    (interactive)
    (save-selected-window
      (let ((filename (tabulated-list-get-id)))
        (switch-to-buffer-other-window "*Paperless Preview*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert-file-contents filename)
        (el-patch-add (setq buffer-file-name filename))
        (if (fboundp 'pdf-view-mode)
            (pdf-view-mode)
          (doc-view-mode))))
    (mapc
     (lambda (i)
       (setf (elt (cadr i) 0) ""))
     paperless--table-contents)
    (setf (elt (cadr (assoc (tabulated-list-get-id) paperless--table-contents)) 0) "*")
    (tabulated-list-print t))

  (el-patch-defun paperless-file ()
    "Select the directory in which to file the current document."
    (interactive)
    (let ((new-dir ((el-patch-swap ido-completing-read
                                   completing-read)
                    "File destination: " (paperless--dirtree)))
          (vctr (cadr (assoc (tabulated-list-get-id) paperless--table-contents))))
      (setf (elt vctr 2) new-dir))
    (tabulated-list-print t))

  (el-patch-defun paperless-execute ()
    "Batch execute all pending document processing."
    (interactive)
    (let* ((delete-list
            (mapcar
             (lambda (i)
               (let ((vctr (cadr i)))
                 (if (= (length (elt vctr 2)) 0)
                     nil
                   (progn
                     (el-patch-swap (if (string-equal (elt vctr 2) "[ TRASH ]")
                                        (move-file-to-trash (car i))
                                      (rename-file (car i) (concat (elt vctr 2) "/" (elt vctr 1))))
                                    (cond ((string-equal (elt vctr 2) "[ TRASH ]")
                                           (move-file-to-trash (car i)))
                                          ((string-equal (elt vctr 2) "[ DELETE ]")
                                           (delete-file (car i)))
                                          ((string-match-p (elt vctr 2) "\\[ MERGE [0-9]+ \\]")
                                           (ignore))
                                          (t (rename-file (car i) (concat (elt vctr 2) "/" (elt vctr 1))))))
                     (car i)))))
             paperless--table-contents))
           (el-patch-add (merge-list (paperless--get-marked-merges)))
           (el-patch-add (merge-output (and merge-list
                                            (concat paperless-capture-directory
                                                    (paperless--merge-pdfs (paperless--get-marked-merges)))))))
      (when merge-output
        (add-to-list 'paperless--table-contents
                     (list merge-output
                           (vector "" (file-name-nondirectory
                                       merge-output) ""))))
      (mapc
       (lambda (i)
         (if (not (null i))
             (setq paperless--table-contents (assq-delete-all i paperless--table-contents))))
       (el-patch-swap delete-list
                      (cl-union delete-list merge-list)))
      (tabulated-list-print t)))

  (defun paperless--get-marked-merges ()
    "Returns a sorted list of PDFs marked for merging."
    (let (files-to-be-merged)
      (mapcar
       (lambda (i)
         (let ((vctr (cadr i)))
           (if (= (length (elt vctr 2)) 0)
               nil
             (progn
               (if (string-match "\\[ MERGE \\([0-9]+\\) \\]" (elt vctr 2))
                   (push (cons (string-to-number
                                (match-string-no-properties 1 (elt vctr 2)))
                               (car i))
                         files-to-be-merged))
               (car i)))))
       paperless--table-contents)
      ;; Sort by the merge number
      (mapcar
       #'cdr
       (sort files-to-be-merged
             (lambda (x y)
               (< (car x) (car y)))))))

  (defun paperless-mark-for-merge ()
    "Mark the current document for merging."
    (interactive)
    (let ((vctr (cadr (assoc (tabulated-list-get-id) paperless--table-contents))))
      (setf (elt vctr 2)
            (concat "[ MERGE "
                    (number-to-string (1+ (length (paperless--get-marked-merges))))
                    " ]")))
    (tabulated-list-print t))

  (defun paperless--merge-pdfs (pdf-files)
    "Merge PDF-FILES. This will delete PDF-FILES."
    (let ((output-file-name (completing-read "Output PDF name: "
                                             (mapcar
                                              (lambda (x)
                                                (concat (string-remove-suffix
                                                         ".pdf"
                                                         (file-name-nondirectory x))
                                                        "_combined.pdf"))
                                              pdf-files))))
      (shell-command-to-string (concat "pdfunite "
                                       (string-join pdf-files " ")
                                       " "
                                       paperless-capture-directory
                                       output-file-name))
      output-file-name))

  (defun paperless-unmark-current-entry ()
    (interactive)
    (let ((vctr (cadr (assoc (tabulated-list-get-id) paperless--table-contents))))
      ;; If the element is a merge entry, we need to adjust other entries
      (when (string-match "\\[ MERGE \\([0-9]+\\) \\]" (elt vctr 2))
        (message "MERGE NUMBER IS: %s" (match-string-no-properties 1 (elt vctr 2)))
        (let ((merge-number (string-to-number
                             (match-string-no-properties 1 (elt vctr 2)))))
          (setq paperless--table-contents
                (mapcar
                 (lambda (paperless-entry)
                   (let ((vctr (cadr paperless-entry)))
                     (if (and (string-match "\\[ MERGE \\([0-9]+\\) \\]"
                                            (elt vctr 2))
                              (> (string-to-number
                                  (match-string-no-properties 1 (elt vctr 2)))
                                 merge-number))
                         (progn (setf (elt vctr 2)
                                      (concat "[ MERGE "
                                              (number-to-string
                                               (1- (string-to-number
                                                    (match-string-no-properties 1 (elt vctr 2)))))
                                              " ]"))
                                (setf (cadr paperless-entry)
                                      vctr)
                                paperless-entry)
                       paperless-entry)))
                 paperless--table-contents))))
      (setf (elt vctr 2)
            "")
      (tabulated-list-print t)))

  (advice-add #'paperless
              :before
              (defun paperless-switch-to-workspace (&rest _)
                (+workspace-switch "*paperless*" t)
                (+workspace/display)))

  (advice-add #'paperless
              :after
              #'paperless-display)

  (add-hook 'paperless-mode-hook
            (defun paperless-adjust-preview-size ()
              (balance-windows)
              (shrink-window 15)))

  (map! :map paperless-mode-map
        :n "j" (evil-define-motion paperless-evil-next-line (count)
                 "Move the cursor COUNT lines down and preview the PDF file at point."
                 :type line
                 (let (line-move-visual)
                   (evil-line-move (or count 1)))
                 (when (and (sp-point-in-blank-line)
                            (not (bobp)))
                   (previous-line))
                 (evil-first-non-blank)
                 (paperless-display))
        :n "k" (evil-define-motion paperless-evil-previous-line (count)
                 "Move the cursor COUNT lines up and preview the PDF file at point."
                 :type line
                 (let (line-move-visual)
                   (evil-line-move (- (or count 1))))
                 (evil-first-non-blank)
                 (paperless-display))
        :n "d" (defun paperless-delete-and-move-to-next-line ()
                 (interactive)
                 (paperless-delete)
                 (paperless-evil-next-line 1))
        :n "D" (defun paperless-delete-and-move-to-previous-line ()
                 (interactive)
                 (paperless-delete)
                 (paperless-evil-previous-line 1))
        :n "u" (defun paperless-unmark-current-entry-and-move-to-next-line ()
                 (interactive)
                 (paperless-unmark-current-entry)
                 (paperless-evil-next-line 1))
        :n "U" #'paperless
        :n "G" (evil-define-motion paperless-goto-line (count)
                 "Go to the first non-blank character of line COUNT.
By default the last line."
                 :jump t
                 :type line
                 (if (null count)
                     (with-no-warnings (end-of-buffer))
                   (goto-char (point-min))
                   (forward-line (1- count)))
                 (when (and (sp-point-in-blank-line)
                            (not (bobp)))
                   (previous-line))
                 (evil-first-non-blank)
                 (paperless-display))
        :n "m" (defun paperless-mark-for-merge-and-move-to-next-line ()
                 (interactive)
                 (paperless-mark-for-merge)
                 (paperless-evil-next-line 1))
        :n "gg" (evil-define-motion paperless-goto-first-line (count)
                  "Go to the first non-blank character of line COUNT.
By default the first line."
                  :jump t
                  :type line
                  (evil-goto-line (or count 1))
                  (evil-first-non-blank)
                  (paperless-display))
        :n "f" #'paperless-file
        :n "x" #'paperless-execute
        :n "q" (defun paperless-quit ()
                 "Quit the current Paperless session."
                 (interactive)
                 (+workspace-delete "*paperless*")
                 (+workspace/other))
        :n "l" (defun paperless-next-page-of-preview (&optional n)
                 "Turn the Paperless preview buffer one page forward."
                 (interactive "p")
                 (unless (window-live-p (get-buffer-window "*Paperless Preview*"))
                   (error "The paperless preview buffer is not visible."))
                 (let ((paperless-buffer-window (selected-window)))
                   (select-window (get-buffer-window "*Paperless Preview*"))
                   (ignore-errors (pdf-view-next-page n))
                   (select-window paperless-buffer-window)))
        :n "h" (defun paperless-previous-page-of-preview (&optional n)
                 "Turn the Paperless preview buffer one page backward."
                 (interactive "p")
                 (unless (window-live-p (get-buffer-window "*Paperless Preview*"))
                   (error "The paperless preview buffer is not visible."))
                 (let ((paperless-buffer-window (selected-window)))
                   (select-window (get-buffer-window "*Paperless Preview*"))
                   (ignore-errors (pdf-view-previous-page n))
                   (select-window paperless-buffer-window)))
        :n "r" (defun paperless-rotate-document-clockwise (&optional arg)
                 "Rotate the currently previewed document
clockwise. With prefix arg, rotate only the page which is currently displayed."
                 (interactive "P")
                 (message "hello %s" arg)
                 (unless (window-live-p (get-buffer-window "*Paperless Preview*"))
                   (error "The paperless preview buffer is not visible."))
                 (let ((paperless-buffer-window (selected-window)))
                   (select-window (get-buffer-window "*Paperless Preview*"))
                   (pdf-view--rotate nil arg)
                   (select-window paperless-buffer-window)))
        :n "R" (defun paperless-rotate-document-counterclockwise (&optional arg)
                 "Rotate the currently previewed document
counterclockwise. With prefix ARG,rotate only the page which is currently
displayed."
                 (interactive "P")
                 (unless (window-live-p (get-buffer-window "*Paperless Preview*"))
                   (error "The paperless preview buffer is not visible."))
                 (let ((paperless-buffer-window (selected-window)))
                   (select-window (get-buffer-window "*Paperless Preview*"))
                   (pdf-view--rotate t arg)
                   (select-window paperless-buffer-window)))))

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
         ("agD" . doctor))
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
  :bind (:map doom-leader-map
         ("agp" . pong))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "pong") . (nil . "Pong")))))

(use-package! snake
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
  :bind (:map doom-leader-map
         ("agd" . dunnet))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "dunnet") . (nil . "Dunnet")))))

(use-package! 2048-game
  :bind (:map doom-leader-map
         ("ag2" . 2048-game))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "2048-game") . (nil . "2048")))))

(use-package! gomoku
  :bind (:map doom-leader-map
         ("ag%" . gomoku))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "gomoku") . (nil . "5-in-a-row")))))

(use-package! 5x5
  :bind (:map doom-leader-map
         ("ag5" . 5x5)))

(use-package! minesweeper
  :bind (:map doom-leader-map
         ("agm" . minesweeper))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "minesweeper") . (nil . "Minesweeper")))))

(use-package! gnugo
  :bind (:map doom-leader-map
         ("agg" . gnugo))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "gnugo") . (nil . "go")))))

(use-package! mpuz
  :bind (:map doom-leader-map
         ("agx" . mpuz))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "mpuz") . (nil . "Multiplication puzzle")))))

(use-package! bubbles
  :bind (:map doom-leader-map
         ("agb" . bubbles))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "bubbles") . (nil . "Bubbles")))))

(use-package! key-quiz
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

(use-package! snow
  :bind (:map doom-leader-map
         ("aes" . let-it-snow))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "let-it-snow") . (nil . "Let it snow")))))

(use-package! flames-of-freedom
  :bind (:map doom-leader-map
         ("aeF" . flames-of-freedom-default))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "flames-of-freedom-default") . (nil . "Flames of Freedom"))))
  :config
  (when (featurep 'perfect-margin)
    (advice-add #'flames-of-freedom-my-message
                :before
                (defun perfect-margin--disable-for-one-command-a (&rest _)
                  (perfect-margin-mode -1)
                  (run-at-time 0 nil
                               (lambda (&rest _)
                                 (perfect-margin-mode +1)))))))

(use-package! hanoi
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
  :bind (:map doom-leader-map
         ("aed" . dissociated-press))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "dissociated-press") . (nil . "Dissociated press")))))

(use-package! life
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
  :bind (:map doom-leader-map
         ("aez" . zone-choose))
  :init
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (require 'zone)
    (let ((zone-programs (list (intern pgm))))
      (zone)))

  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "zone-choose") . (nil . "Zone"))))

  :config
  (unless (memq 'zone-pgm-md5 (append zone-programs nil))
    (setq zone-programs
          (vconcat zone-programs [zone-pgm-md5])))

  (defun zone-pgm-md5 ()
    "MD5 the buffer, then recursively checksum each hash."
    (let ((prev-md5 (buffer-substring-no-properties ;; Initialize.
                     (point-min) (point-max))))
      ;; Whitespace-fill the window.
      (zone-fill-out-screen (window-width) (window-height))
      (random t)
      (goto-char (point-min))
      (while (not (input-pending-p))
        (when (eobp)
          (goto-char (point-min)))
        (while (not (eobp))
          (delete-region (point) (line-end-position))
          (let ((next-md5 (md5 prev-md5)))
            (insert next-md5)
            (setq prev-md5 next-md5))
          (forward-line 1)
          (zone-park/sit-for (point-min) 0.1))))))

(use-package! fireplace
  :bind (:map doom-leader-map
         ("aef" . fireplace))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "fireplace") . (nil . "Fireplace"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'fireplace-mode 'emacs)))

(use-package! wakatime-mode
  :defer-incrementally t
  :config
  (el-patch-defun wakatime-client-command (savep)
    "Return client command executable and arguments.
   Set SAVEP to non-nil for write action."
    (format "%s%s--file \"%s\" --plugin \"%s/%s\" --time %.2f%s%s"
            (if (s-blank wakatime-python-bin) "" (format "%s " wakatime-python-bin))
            (if (s-blank wakatime-cli-path) "wakatime " (format "%s " wakatime-cli-path))
            (el-patch-swap (buffer-file-name (current-buffer))
                           (or (buffer-file-name (current-buffer))
                               (bound-and-true-p org-src-source-file-name)))
            wakatime-user-agent
            wakatime-version
            (float-time)
            (if savep " --write" "")
            (if (s-blank wakatime-api-key) "" (format " --key %s" wakatime-api-key))))

  (el-patch-defun wakatime-ping ()
    "Send ping notice to WakaTime."
    (when (and
           (el-patch-swap (buffer-file-name (current-buffer))
                          (or (buffer-file-name (current-buffer))
                              (bound-and-true-p org-src-source-file-name)))
           (not (auto-save-file-name-p
                 (el-patch-swap
                   (buffer-file-name (current-buffer))
                   (or (buffer-file-name (current-buffer))
                       (bound-and-true-p org-src-source-file-name))))))
      (wakatime-call nil)))

  (el-patch-defun wakatime-save ()
    "Send save notice to WakaTime."
    (when (and (el-patch-swap
                 (buffer-file-name (current-buffer))
                 (or (buffer-file-name (current-buffer))
                     (bound-and-true-p org-src-source-file-name)))
               (not (auto-save-file-name-p
                     (el-patch-swap
                       (buffer-file-name (current-buffer))
                       (or (buffer-file-name (current-buffer))
                           (bound-and-true-p org-src-source-file-name))))))
      (wakatime-call t)))

  (load (expand-file-name "wakatime" doom-private-dir))
  (global-wakatime-mode +1))

(after! smartparens
  (add-hook 'pre-command-hook
            (defun my-sp-activate-post-handler ()
              (when (and (equal (this-command-keys-vector)
                                [?\C-m])
                         (not (memq 'sp--pair-overlay-post-command-handler
                                    post-command-hook)))
                (when-let* ((op (car (sp-point-in-empty-sexp)))
                            (pair (sp-get-pair (car (sp-point-in-empty-sexp))))
                            (insertion-specification (first (--first (equal (second it) "RET")
                                                                     (plist-get pair :post-handlers)))))
                  (eval (sp--parse-insertion-spec insertion-specification)))))))

(after! doom-themes
  (add-to-list 'doom-themes-base-faces
               '(hl-fill-column-face :inherit 'shadow)))

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
  (add-to-list 'cdlatex-math-symbol-alist '(?* ("\\times" "\\diamond")))
  (set-popup-rule! "\\*CDLaTeX Help\\*" :side 'bottom :focus nil))

(after! cdlatex
  (load (expand-file-name "transform" doom-private-dir) t t))

(use-package! x86-lookup
  :commands x86-lookup
  :config
  (require 'orb-core)
  (require 'bibtex-completion)
  (setq  x86-lookup-pdf (orb-process-file-field "Intel64IA322020"))
  (el-patch-defun x86-lookup-browse-pdf-pdf-tools (pdf page)
    "View PDF at PAGE using Emacs' `pdf-view-mode' and `display-buffer'."
    (require 'pdf-tools)
    (prog1 t
      (with-selected-window (el-patch-swap
                              (display-buffer (find-file-noselect pdf :nowarn))
                              (display-buffer-in-side-window
                               (find-file-noselect pdf :nowarn)
                               '((side . right)
                                 (window-width . 0.5))))
        (with-no-warnings
          (pdf-view-goto-page page))))))

(use-package! nasm-mode
  :config
  (add-hook 'asm-mode-hook 'nasm-mode)
  (when (and (boundp '+format-on-save-enabled-modes)
             (eq (car +format-on-save-enabled-modes) 'not))
    (add-to-list '+format-on-save-enabled-modes 'nasm-mode))

  (defun nasm-insert-c (arg)
    (interactive "p")
    (self-insert-command arg)
    (run-at-time 0.1 nil
                 (lambda ()
                   (when (looking-back "in\\(\\s-+\\)c" (point-at-bol))
                     (insert (match-string 1))
                     (delete-region (match-beginning 1) (match-end 1))))))

  (map! :map nasm-mode-map
        "c" #'nasm-insert-c))

(when (featurep! :ui workspaces)
  (after! persp-mode
    (map! "C-c C-1" #'+workspace/switch-to-0
          "C-c C-2" #'+workspace/switch-to-1
          "C-c C-3" #'+workspace/switch-to-2
          "C-c C-4" #'+workspace/switch-to-3
          "C-c C-5" #'+workspace/switch-to-4
          "C-c C-6" #'+workspace/switch-to-5
          "C-c C-7" #'+workspace/switch-to-6
          "C-c C-8" #'+workspace/switch-to-7
          "C-c C-9" #'+workspace/switch-to-8
          "C-c C-0" #'+workspace/switch-to-final
          "C-c 1" #'+workspace/switch-to-0
          "C-c 2" #'+workspace/switch-to-1
          "C-c 3" #'+workspace/switch-to-2
          "C-c 4" #'+workspace/switch-to-3
          "C-c 5" #'+workspace/switch-to-4
          "C-c 6" #'+workspace/switch-to-5
          "C-c 7" #'+workspace/switch-to-6
          "C-c 8" #'+workspace/switch-to-7
          "C-c 9" #'+workspace/switch-to-8
          "C-c 0" #'+workspace/switch-to-final)
    (map! :leader
          "1" #'+workspace/switch-to-0
          "2" #'+workspace/switch-to-1
          "3" #'+workspace/switch-to-2
          "4" #'+workspace/switch-to-3
          "5" #'+workspace/switch-to-4
          "6" #'+workspace/switch-to-5
          "7" #'+workspace/switch-to-6
          "8" #'+workspace/switch-to-7
          "9" #'+workspace/switch-to-8
          "0" #'+workspace/switch-to-final)
    (after! exwm
      (map! :leader
            (:prefix-map ("z" . "EXWM")
             "w" #'exwm-run-or-raise-firefox
             "q" #'exwm-run-or-raise-qutebrowser
             "RET" #'exwm-run-or-raise-urxvt
             "p" #'exwm-run-or-raise-retroarch
             "4" #'+counsel-linux-app)))))

(after! persp-mode
  (setq persp-autokill-persp-when-removed-last-buffer 'kill-auto))

(when (featurep! :ui workspaces)
  (after! persp-mode
    (advice-add #'+workspace/switch-to :before-while
                (defun +workspace--ignore-when-minibuffer-active (&rest _)
                  (not (minibufferp))))))

(after! persp-mode
  (defun browse-url-in-workspace (&rest _)
    (when (featurep 'exwm)
      (pcase browse-url-generic-program
        ("qutebrowser" (when (featurep! :ui workspaces)
                         (+workspace-switch "Qutebrowser" t)
                         (+workspace/display)))
        ("firefox" (when (featurep! :ui workspaces)
                     (+workspace-switch "Firefox" t)
                     (+workspace/display))))))

  (advice-add #'browse-url-generic :before #'browse-url-in-workspace))

(when (featurep! :ui workspaces)
  (after! (:all exwm persp-mode s cl)
    (defvar exwm--important-apps nil)
    (setq exwm--important-apps '("Qutebrowser"
                                 "Firefox" "firefox-devedition"
                                 "Chromium-browser" "Chromium"
                                 "Evince"
                                 "WeChat"
                                 "Zoom"
                                 "Wine"
                                 "Anydesk"
                                 "VBoxSDL"
                                 "VirtualBox"
                                 "krita"
                                 "Lutris"
                                 "mpv"
                                 "Discord"
                                 "Steam"
                                 "XTerm"
                                 "URxvt"
                                 "Skype"
                                 ".epsxe-wrapped" "ePSXe"
                                 ".blueman-manager-wrapped" "Bluetooth"
                                 ;; "VirtualBox Manager" "VirtualBox Machine" "VirtualBox"
                                 "PCSX2"
                                 "Virt-manager"
                                 "RetroArch"
                                 "Calibre"
                                 "Zulip"
                                 "Snes9x-gtk" "Snes9x"
                                 "Tor Browser"
                                 "Higan"
                                 "Plover"
                                 "RPCS3"
                                 "Zotero"))

    (add-hook 'exwm-floating-setup-hook
              (defun exwm--disable-floating-in-important-apps ()
                (when (cl-member (exwm--app-name)
                                 exwm--important-apps
                                 :test #'cl-equalp)
                  (exwm-floating--unset-floating exwm--id))))

    (defun exwm--app-name (&optional buffer-or-string)
      (let ((class (cond ((bufferp buffer-or-string)
                          (buffer-local-value 'exwm-class-name buffer-or-string))
                         ((stringp buffer-or-string)
                          buffer-or-string)
                         (t exwm-class-name)))
            (title (cond ((bufferp buffer-or-string)
                          (buffer-local-value 'exwm-title buffer-or-string))
                         ((stringp buffer-or-string)
                          buffer-or-string)
                         (t exwm-title))))
        (cond ((cl-equalp class "Wine")
               (add-to-list 'exwm--important-apps title)
               title)
              ((or (cl-equalp class "Firefox Developer Edition")
                   (string-prefix-p "firefox" class))
               "Firefox")
              ((string-prefix-p "VirtualBox" class t)
               "VirtualBox")
              ((cl-equalp class "VBoxSDL")
               class)
              ((cl-equalp class "qutebrowser")
               "Qutebrowser")
              ((cl-equalp class ".epsxe-wrapped")
               "ePSXe")
              ((cl-equalp class "Snes9x-gtk")
               "Snes9x")
              ((cl-equalp class "discord")
               "Discord")
              ((cl-equalp class ".blueman-manager-wrapped")
               "Bluetooth")
              ((cl-equalp class "Chromium-browser")
               "Chromium")
              ((string-prefix-p "Plover" title t)
               "Plover")
              ((string-prefix-p "Minecraft" title t)
               "Minecraft")
              (t class))))

    (advice-add #'+workspace-switch :after
                (defun +workspace--cleanup-boring-workspaces-a (&rest _)
                  (--map (when (and (not (equal it (+workspace-current-name)))
                                    (cl-member it '("*somafm*"
                                                    ;; "zoom"
                                                    )
                                               :test #'cl-equalp))
                           (+workspace-delete it))
                         (+workspace-list-names))))

    (el-patch-defun persp-buffers-to-savelist (persp)
      (delete-if
       #'symbolp
       (let (find-ret)
         (mapcar #'(lambda (b)
                     (setq find-ret nil)
                     (find-if #'(lambda (sl) (when sl (setq find-ret sl)))
                              persp-save-buffer-functions
                              :key #'(lambda (s-f)
                                       (el-patch-swap (with-current-buffer b
                                                        (funcall s-f b))
                                                      (when (buffer-live-p b)
                                                        (with-current-buffer b
                                                          (funcall s-f b))))))
                     find-ret)
                 (if persp
                     (persp-buffers persp)
                   (delete-if-not #'persp-buffer-free-p
                                  (funcall persp-buffer-list-function)))))))

    (persp-def-auto-persp
     "exwm"
     :parameters '((dont-save-to-file . t))
     :hooks '(exwm-update-class-hook
              ;; doom-switch-buffer-hook
              ;; exwm-mode-hook
              exwm-update-title-hook
              ;; focus-in-hook
              )
     :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
                (persp-add-buffer-on-find-file nil)
                persp-add-buffer-on-after-change-major-mode)
     :switch 'window
     :predicate (lambda (buffer &optional state)
                  (and (stringp (exwm--app-name buffer))
                       (cl-member (exwm--app-name buffer)
                                  exwm--important-apps
                                  :test #'cl-equalp)
                       (not (cl-member (exwm--app-name buffer)
                                       (+workspace-list-names)
                                       :test #'cl-equalp))
                       (or state t)))
     :after-match (lambda (buffer &rest _)
                    (let* ((buffer (alist-get 'buffer state))
                           (application-name (exwm--app-name buffer)))
                      (when (and (not (cl-equalp (+workspace-current-name)
                                                 application-name))
                                 (get-buffer-window buffer)
                                 (window-live-p (get-buffer-window buffer))
                                 (not (eq (get-buffer-window buffer)
                                          (window-main-window))))
                        (delete-window (get-buffer-window buffer)))
                      (+workspace-switch application-name t)
                      (+workspace/display)
                      ;; (unless (eq (get-buffer-window buffer)
                      ;;             (window-main-window))
                      ;;   (and (window-live-p (window-main-window))
                      ;;        (select-window (window-main-window)))
                      ;;   (switch-to-buffer buffer))
                      (--map-when (equalp (exwm--app-name (window-buffer it))
                                          application-name)
                                  (and (window-live-p it)
                                       (not (eq it (window-main-window)))
                                       (delete-window it))
                                  (+popup-windows))
                      (when (+popup-window-p)
                        (other-window)
                        (switch-to-buffer buffer))
                      (--map-when (not (equalp (exwm--app-name (window-buffer it))
                                               application-name))
                                  (and (window-live-p it)
                                       (not (eq it (window-main-window)))
                                       (delete-window it))
                                  (doom-visible-windows))))
     :get-name (lambda (state)
                 (let* ((buffer (alist-get 'buffer state))
                        (application-name (exwm--app-name buffer)))
                   ;; (message "HELLO AGAIN :) %s" buffer)
                   (setf (alist-get 'persp-name state) application-name)
                   state)))

    (advice-add #'+workspace-switch :after
                (defun exwm--focus-workspace-app (&rest _)
                  (when (and (cl-member (+workspace-current-name)
                                        exwm--important-apps
                                        :test #'cl-equalp)
                             (--none? (buffer-local-value 'org-capture-mode
                                                          (window-buffer it))
                                      (cl-union (+popup-windows)
                                                (doom-visible-windows))))
                    (let ((app-buffer (--first (cl-equalp (exwm--app-name it)
                                                          (+workspace-current-name))
                                               (cl-union (+workspace-buffer-list)
                                                         (buffer-list)))))
                      (unless (window-live-p (get-buffer-window app-buffer))
                        ;; (select-window (get-buffer-window app-buffer))
                        (switch-to-buffer app-buffer))))))

    (add-hook 'kill-buffer-hook
              (defun exwm--cleanup-browser-workspaces ()
                (when (eq major-mode 'exwm-mode)
                  (run-at-time
                   0.01 nil
                   (lambda (&rest _)
                     (cl-loop
                      for workspace in (+workspace-list-names) do
                      (cl-loop
                       for app in exwm--important-apps do
                       (when (and
                              (cl-equalp workspace app)
                              (--none?
                               (cl-equalp (exwm--app-name it)
                                          app)
                               (cl-union (+workspace-buffer-list
                                          (+workspace-get workspace))
                                         (doom-visible-buffers))))
                         (when (prog1 (cl-equalp (+workspace-current-name)
                                                 workspace)
                                 (+workspace-delete workspace))
                           (+workspace/other))))))))))

    ;; (advice-add #'+workspace-delete
    ;;             :before
    ;;             (defun +workspace-delete--run-exwm-hooks (app &optional _)
    ;;               (cond ((cl-equalp app "Discord")
    ;;                      (start-process
    ;;                       "Discord" nil
    ;;                       "Discord" "--start-minimized")))))

    (advice-add #'browse-url-generic
                :before
                (defun exwm--switch-to-browser-workspace (&rest _)
                  (when (eq browse-url-browser-function #'browse-url-generic)
                    (+workspace-switch (exwm--app-name browse-url-generic-program) t))))

    (advice-add #'org-roam-graph
                :before
                (defun org-roam-graph--use-browser-workspace (&rest _)
                  (+workspace-switch
                   (s-capitalize (car (last (split-string org-roam-graph-viewer
                                                          "/" t))))
                   t)
                  (+workspace/display)))))

(after! persp-mode
  (persp-def-auto-persp
   "org"
   :hooks '(find-file-hook
            doom-switch-buffer-hook)
   :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
              (persp-add-buffer-on-find-file nil)
              persp-add-buffer-on-after-change-major-mode)
   :switch 'window
   :predicate (lambda (buffer &optional state)
                (and (string-prefix-p (expand-file-name "~/org")
                                      (or buffer-file-name ""))
                     (not (member (+workspace-current-name)
                                  (-map #'cdr
                                        (bound-and-true-p org-noter-session-workspace-alist))))
                     (not (string-suffix-p "_archive" (buffer-file-name)))
                     (not (bound-and-true-p exwm-mode))
                     (not (memq last-command
                                '(org-capture
                                  org-roam-capture
                                  org-capture-kill
                                  exwm--org-roam-capture-web-page)))
                     (or state t)))
   :after-match (lambda (buffer &rest _)
                  (let* ((buffer (alist-get 'buffer state)))
                    (+workspace-switch "org")
                    (+workspace/display)
                    ;; (--map-when (not (memq (buffer-local-value
                    ;;                       'major-mode
                    ;;                       (window-buffer it))
                    ;;                        '(org-mode
                    ;;                          org-journal-mode)))
                    ;;             (and (window-live-p it)
                    ;;                  (not (eq it (window-main-window)))
                    ;;                  (delete-window it))
                    ;;             (doom-visible-windows))
                    (switch-to-buffer buffer)))))
;; (advice-add #'+workspace-switch
;;             :around
;;             (defun +workspace--use-correct-org-workspace-a (oldfun name &rest args)
;;               (when (equalp name "org")
;;                 (setq name "Org"))
;;               (apply oldfun name args)))

(when (featurep! :ui workspaces)
  (after! persp-mode
    (defvar +bookmark-workspaces nil)

    (advice-add #'bookmark-jump :before
                (defun bookmark-jump--create-workspace-a
                    (bookmark &optional display-func)
                  (when (and (not current-prefix-arg)
                             (file-exists-p
                              (cdr (assq
                                    'filename (bookmark-get-bookmark-record bookmark)))))
                    (+workspace-switch bookmark t)
                    (persp-add-buffer)
                    (add-to-list '+bookmark-workspaces (get-current-persp)))))

    (advice-add #'+dired/quit-all :around
                (defun bookmark-delete-workspace-a (oldfun)
                  (if (member (get-current-persp)
                              +bookmark-workspaces)
                      (progn (setq +bookmark-workspaces
                                   (delq (get-current-persp) +bookmark-workspaces))
                             (+workspace-delete (get-current-persp))
                             (+workspace/other))
                    (funcall oldfun))))

    (advice-add #'kill-current-buffer :around
                (defun bookmark-delete-current-buffer-workspace (oldfun)
                  (if (and (member (get-current-persp)
                                   +bookmark-workspaces)
                           (file-equal-p (cdr (assq
                                               'filename (bookmark-get-bookmark-record (+workspace-current-name))))
                                         (or (buffer-file-name)
                                             dired-directory)))
                      (progn (setq +bookmark-workspaces
                                   (delq (get-current-persp) +bookmark-workspaces))
                             (+workspace-delete (get-current-persp))
                             (+workspace/other))
                    (funcall oldfun))))))

(advice-add #'+workspace-switch
            :after
            (defun +workspaces--ensure-mu4e-is-focused-in-workspace-a (&rest _)
              (when (and (equal (+workspace-current-name)
                             +mu4e-workspace-name)
                         (--any? (eq (buffer-local-value 'major-mode it)
                                     'exwm-mode)
                                 (doom-visible-buffers)))
                (cond ((get-buffer "*mu4e-headers*")
                       (switch-to-buffer "*mu4e-headers*")
                       (delete-other-windows))
                      (t (mu4e))))))

(after! irony
  (setq irony--server-executable (executable-find "irony-server")))

(after! disaster
  (setq disaster-cflags "-O0 -g -m32 -fno-exceptions -fno-asynchronous-unwind-tables -fno-pic -D_FORTIFY_SOURCE=0 -fno-stack-protector"
        disaster-cxxflags "-O0 -g -m32 -fno-exceptions -fno-asynchronous-unwind-tables -fno-pic -D_FORTIFY_SOURCE=0 -fno-stack-protector"
        disaster-objdump "objdump -d -M intel -Sl --no-show-raw-insn"))

(when (featurep! :ui pretty-code)
  (setq +pretty-code-enabled-modes '(not mu4e-view-mode
                                         mu4e-headers-mode
                                         w3m-mode
                                         nov-mode)))

(after! nix-mode
  (defun nixos-options-completions-at-point ()
    (let ((start (+ (point)
                    (save-excursion
                      (skip-chars-backward "[a-zA-Z0-9.]"))))
          (end (point))
          (collection (mapcar #'car nixos-options)))
      (list start
            end
            collection
            :annotation-function #'company-nixos-options--annotation
            :company-doc-buffer #'company-nixos-options--doc-buffer)))

  ;; This to prevent Doom Emacs from adding company-nixos-options to
  ;; company-backends
  (set-company-backend! 'nix-mode
    (delq 'company-nixos-options (alist-get 'nix-mode +company-backend-alist)))

  (add-hook 'nix-mode-hook
            (defun company-nixos-options--initialize-completion-at-point ()
              (require 'company-nixos-options)
              (add-hook 'completion-at-point-functions
                        #'nixos-options-completions-at-point nil 'local))))

(when (featurep! :tools lookup)
  (map! :g "C-c w" #'+lookup/online)
  (add-to-list '+lookup-provider-url-alist
               '("Urban Dictionary"
                 "http://www.urbandictionary.com/define.php?term=%s"))
  (add-to-list '+lookup-provider-url-alist
               '("Explain Shell" "https://explainshell.com/explain?cmd=%s"))
  (add-to-list '+lookup-provider-url-alist
               '("DuckDuckGo Lucky"   "https://duckduckgo.com/?q=\\%s")))

(defun my-unicode-init-fonts-h ()
  "Set up `unicode-fonts' to eventually run; accomodating the daemon, if
necessary."
  (setq-default bidi-display-reordering nil
                doom-unicode-font nil)
  (if initial-window-system
      (+unicode-setup-fonts-h (selected-frame))
    (add-hook 'after-make-frame-functions #'+unicode-setup-fonts-h)))

(advice-add #'+unicode-init-fonts-h :override #'my-unicode-init-fonts-h)

(after! popup
  (set-popup-rules! '(("^\\*Agda information" :size 0.3 :focus nil)
                      ("\\*intero:global-project::repl" :size 0.3 :focus t)
                      ("\\*haskell-process-log" :size 0.3 :focus nil :quit t)
                      ("\\*test\\*" :size 0.3 :focus t)
                      ("\\*Compile-Log\\*" :size 0.3 :focus nil :quit t)
                      ("^\\*Org Agenda\\*" :size 0.5 :side 'bottom)
                      ("^\\*eww\\*" :ignore t)
                      ("^\\*cfw:details\\*" :size 0.35)
                      ("Helm systemd" :ignore t))))

(after! org-capture
  (advice-add #'+popup--kill-buffer
              :before-until
              (defun +popup--ignore-if-capture-buffer-a (buffer ttl)
                (buffer-local-value 'org-capture-mode
                                    buffer))))

(advice-add #'delete-other-windows :around
            (defun delete-other-windows--fix-in-popup (oldfun &rest args)
              (let ((ignore-window-parameters t))
                (when (+popup-buffer-p)
                  (+popup/raise (selected-window)))
                (apply oldfun args))))

(when (featurep! :editor evil)
  (after! evil-snipe
    (setq evil-snipe-scope 'line
          evil-snipe-spillover-scope 'visible)))

(add-hook 'evil-insert-state-exit-hook #'expand-abbrev)

(after! evil
  (setq evil-move-cursor-back nil))

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

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 3))

(after! (:all exwm company)
  (add-hook 'exwm-mode-hook
            (lambda ()
              (company-mode -1))))

(use-package! company-quickhelp
  :after company
  :custom
  (company-quickhelp-margin 15)
  (company-quickhelp-delay nil)
  :hook (company-mode . company-quickhelp-local-mode))

(after! company
  (setq company-show-numbers t)
  (map! :map company-active-map
        "C-c C-1" (cmd! (company-complete-number 1))
        "C-c C-2" (cmd! (company-complete-number 2))
        "C-c C-3" (cmd! (company-complete-number 3))
        "C-c C-4" (cmd! (company-complete-number 4))
        "C-c C-5" (cmd! (company-complete-number 5))
        "C-c C-6" (cmd! (company-complete-number 6))
        "C-c C-7" (cmd! (company-complete-number 7))
        "C-c C-8" (cmd! (company-complete-number 8))
        "C-c C-9" (cmd! (company-complete-number 9))
        "C-c C-0" (cmd! (company-complete-number 10))))

(after! company
  (advice-add #'company-complete-common-or-cycle
              :around
              (defun company--expand-abbrev-instead-a (oldfun &rest args)
                (if (expand-abbrev)
                    (company-abort)
                  (apply oldfun args)))))

(after! company
  (map! :map company-active-map
        "<next>" #'company-next-page
        "<prior>" #'company-previous-page))

(after! ivy
  (setq +ivy-buffer-preview 'everything)
  (map! :map ivy-mode-map
        [remap switch-to-buffer] #'+ivy/switch-buffer))

(after! ivy
  ;; Ensure this key is on a home row key
  (define-key ivy-minibuffer-map (kbl-kbd "j" 'control nil 'shift nil) #'ivy-immediate-done)
  (map! :map ivy-minibuffer-map
        "C-c C-n" #'ivy-next-line
        "C-c C-p" #'ivy-previous-line
        (:after ivy-posframe
         "C-c TAB" (defun +ivy-avy ()
                     (interactive)
                     (require 'ivy-avy)
                     (if (bound-and-true-p ivy-posframe-mode)
                         (ivy-posframe-avy)
                       (ivy-avy))))))

(unless (featurep! :completion ivy +childframe)
  (remove-hook 'ivy-mode-hook #'ivy-posframe-mode))

(add-hook! 'exwm-init-hook
  (after! ivy-posframe
    (add-to-list 'ivy-posframe-parameters '(parent-frame . nil))))

(after! ivy-posframe
  (add-hook 'doom-escape-hook
           (defun doom-escape--posframe-delete-all ()
              (posframe-delete-all))))

(unless (featurep! :completion ivy +childframe)
  (after! ivy
    (setq ivy-fixed-height-minibuffer nil)
    (require 'lv)

    (set-popup-rule! "^\\*LV\\*$" :ignore t)
    (defun ivy--lv-message (format-string &rest args)
      "Set LV window contents to (`format' FORMAT-STRING ARGS)."
      (let* ((str (apply #'format format-string args))
             (n-lines (cl-count ?\n str))
             deactivate-mark
             golden-ratio-mode)
        (with-selected-window (lv-window)
          (unless (and (string= (buffer-string) str)
                       (null lv-force-update))
            (delete-region (point-min) (point-max))
            (insert str)
            (set (make-local-variable 'window-min-height) helm-autoresize-min-height)
            (setq truncate-lines (> n-lines 1))
            (let ((window-resize-pixelwise t)
                  (window-size-fixed nil))
              (fit-window-to-buffer
               nil (floor (* (frame-height) (/ helm-autoresize-min-height 100.0)))
               (floor (* (frame-height) (/ helm-autoresize-max-height 100.0))) nil nil t)))
          (goto-char (point-min)))))


    (defun ivy-display-function-lv (text)
      (let ((lv-force-update t))
        (ivy--lv-message
         (if (string-match "\\`\n" text)
             (substring text 1)
           text))))

    (el-patch-defun swiper-isearch (&optional initial-input)
      "A `swiper' that's not line-based."
      (interactive)
      (let ((el-patch-remove (ivy-fixed-height-minibuffer t))
            (cursor-in-non-selected-windows nil)
            (swiper-min-highlight 1))
        (ivy-read
         "Swiper: "
         #'swiper-isearch-function
         :initial-input initial-input
         :keymap swiper-isearch-map
         :dynamic-collection t
         :require-match t
         :action #'swiper-isearch-action
         :re-builder #'swiper--re-builder
         :history 'swiper-history
         :extra-props (list :fname (buffer-file-name))
         :caller 'swiper-isearch)))

    (setcdr (assq 't ivy-display-functions-alist)
            (defun ivy-display-function-lv (text)
              (let ((lv-force-update t))
                (ivy--lv-message
                 (replace-regexp-in-string
                  "%" "%%"
                  (if (string-match "\\`\n" text)
                      (substring text 1)
                    text))))
              (let ((ov (make-overlay (point-min) (point-max) nil nil t))
                    (minibuffer-string (buffer-string)))
                (overlay-put ov 'face
                             (let ((bg-color (face-background 'default nil)))
                               `(:background ,bg-color :foreground ,bg-color)))
                (setq-local cursor-type nil)
                (with-current-buffer (window-buffer (lv-window))
                  (setq header-line-format
                        (concat (make-string
                                 (car (perfect-margin--init-window-margins)) ?\s)
                                minibuffer-string))))))

    (advice-add #'evil-ex-start-search
                :around
                (defun ivy--use-default-display-function-a (oldfun &rest args)
                  "This is my attempt to fix a error I don't know the cause of."
                  (let ((ivy-display-functions-alist '((t))))
                    (apply oldfun args))))))

(when (and (featurep! :completion ivy)
           (featurep! :completion helm))
  (advice-add #'helm-mode :override #'ignore)
  (advice-add #'completion-at-point
              :around
              (defun helm--use-helm-when-in-minibuffer-a (oldfun &rest args)
                (if (minibufferp)
                    (progn (advice-remove #'helm-mode #'ignore)
                           (helm-mode +1)
                           (when (featurep 'helm-ext-minibuffer)
                             (helm-ext-minibuffer-enable-header-line-maybe t))
                           (apply oldfun args)
                           (when (featurep 'helm-ext-minibuffer)
                             (helm-ext-minibuffer-enable-header-line-maybe nil))
                           (helm-mode -1)
                           (advice-add #'helm-mode :override #'ignore))
                  (apply oldfun args))))

(if helm-mode
      (progn
        (add-function :override completing-read-function
                      #'helm--completing-read-default)
        (add-function :override read-file-name-function
                      #'helm--generic-read-file-name)
        (add-function :override read-buffer-function
                      #'helm--generic-read-buffer)
        (when helm-mode-handle-completion-in-region
          (add-function :around completion-in-region-function
                        #'helm--completion-in-region))
        ;; If user have enabled ido-everywhere BEFORE enabling
        ;; helm-mode disable it and warn user about its
        ;; incompatibility with helm-mode (issue #2085).
        (helm-mode--disable-ido-maybe)
        ;; If ido-everywhere is not enabled yet anticipate and
        ;; disable it if user attempt to enable it while helm-mode
        ;; is running (issue #2085).
        (add-hook 'ido-everywhere-hook #'helm-mode--ido-everywhere-hook)
        (when (fboundp 'ffap-read-file-or-url-internal)
          ;; `ffap-read-file-or-url-internal' have been removed in
          ;; emacs-27 and `ffap-read-file-or-url' is fixed, so no need
          ;; to advice it.
          (advice-add 'ffap-read-file-or-url :override #'helm-advice--ffap-read-file-or-url)))
    (progn
      (remove-function completing-read-function #'helm--completing-read-default)
      (remove-function read-file-name-function #'helm--generic-read-file-name)
      (remove-function read-buffer-function #'helm--generic-read-buffer)
      (remove-function completion-in-region-function #'helm--completion-in-region)
      (remove-hook 'ido-everywhere-hook #'helm-mode--ido-everywhere-hook)
      (when (fboundp 'ffap-read-file-or-url-internal)
        (advice-remove 'ffap-read-file-or-url #'helm-advice--ffap-read-file-or-url))))


  (map! "C-x C-f" #'helm-find-files
        (:leader
         "." #'helm-find-files
         "ff" #'helm-find-files)
        (:after helm-files
         (:map helm-find-files-map
          "DEL" #'helm-find-files-up-one-level
          "<backspace>" #'helm-find-files-up-one-level)))
  (after! org
    (map! :map org-mode-map
          :localleader
          "." #'counsel-org-goto
          "/" #'counsel-org-goto-all
          (:prefix ("g" . "goto")
           "g" #'counsel-org-goto
           "G" #'counsel-org-goto-all))))

(use-package! ace-jump-helm-line
  :after (helm)
  :config
  (define-key helm-map (kbd "C-'") 'ace-jump-helm-line)
  (setq ace-jump-helm-line-default-action 'select)
  (setq ace-jump-helm-line-select-key ?c) ;; this line is not needed
  ;; Set the move-only and persistent keys
  (setq ace-jump-helm-line-move-only-key ?y)
  (setq ace-jump-helm-line-persistent-key ?\'))

(after! helm-files
  (setq helm-ff-file-name-history-use-recentf t)
  ;; Patching so that using RET on a directory opens Dired
  (el-patch-defun helm-ff-RET-1 (&optional must-match)
    "Used for RET action in `helm-find-files'.
See `helm-ff-RET' for details.
If MUST-MATCH is specified exit with
`helm-confirm-and-exit-minibuffer' which handle must-match mechanism."
    (let ((sel (helm-get-selection))
          ;; Ensure `file-directory-p' works on remote files.
          non-essential)
      (cl-assert sel nil "Trying to exit with no candidates")
      (if (and (or (el-patch-remove (file-directory-p sel))
                   (helm-ff--invalid-tramp-name-p sel))
               ;; Allows exiting with default action when a prefix arg
               ;; is specified.
               (null current-prefix-arg)
               (null helm-ff--RET-disabled)
               (or (and (file-remote-p sel)
                        (string= "." (helm-basename sel))
                        (string-match-p "\\`[/].*:.*:\\'"
                                        helm-pattern))
                   (not (string= "." (helm-basename sel)))))
          (helm-execute-persistent-action)
        (if must-match
            (helm-confirm-and-exit-minibuffer)
          (helm-maybe-exit-minibuffer)))))

  ;; Patching so that the checksum is displayed in a message
  (el-patch-defun helm-ff-checksum (file)
    "Calculate the checksum of FILE.
The checksum is copied to `kill-ring'.
Checksum is calculated with the md5sum, sha1sum, sha224sum,
sha256sum, sha384sum and sha512sum when available, otherwise the
Emacs function `secure-hash' is used but it is slow and may crash
Emacs and even the whole system as it eats all memory."
    (cl-assert (file-regular-p file)
               nil "`%s' is not a regular file" file)
    (let* ((algo (intern (helm-comp-read
                          "Algorithm: "
                          '(md5 sha1 sha224 sha256 sha384 sha512))))
           (cmd  (concat (symbol-name algo) "sum"))
           (bn (helm-basename file))
           proc)
      (message "Calculating %s checksum for %s..." algo bn)
      (if (executable-find cmd)
          (progn
            (set-process-filter
             (setq proc (start-file-process cmd nil cmd "-b" file))
             (lambda (_process output)
               (when output (kill-new output))))
            (set-process-sentinel
             proc
             `(lambda (_process event)
                (when (string= event "finished\n")
                  (el-patch-swap (message "Calculating %s checksum for `%s' done and copied to kill-ring"
                                          ,(symbol-name algo) ,bn)
                                 (message "The %s checksum of `%s' is %s and has been copied."
                                          ,(symbol-name algo) ,bn (car (split-string (car kill-ring) " "))))))))
        (async-let ((sum (with-temp-buffer
                           (insert-file-contents-literally file)
                           (secure-hash algo (current-buffer)))))
          (kill-new sum)
          (el-patch-swap (message "Calculating %s checksum for `%s' done and copied to kill-ring"
                                  algo bn)
                         (message "The %s checksum of `%s' is %s and has been copied to kill-ring"
                                  algo bn sum)))))))

(after! helm
  (setq helm-echo-input-in-header-line t)
  (defun spacemacs//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook
            'spacemacs//helm-hide-minibuffer-maybe))

(after! helm
  (require 'helm-ext)
  (require 'helm-ext-minibuffer)
  (require 'helm-ext-ff)

  ;; (helm-ext-ff-enable-skipping-dots nil)
  ;; (setq helm-ext-ff-skipping-dots-recenter nil)
  (helm-ext-ff-enable-zsh-path-expansion nil)
  (helm-ext-ff-enable-auto-path-expansion t)

  ;; (helm-ext-minibuffer-enable-header-line-maybe t)
  )

(after! helm-files
  (defvar external-file-types nil)
  (setq external-file-types '("mkv" "mp4" "avi"))
  (defun emacs-or-external-viewer (file)
    (let ( (extensions (mapcar #'(lambda (ext) (concat "^.*\\." ext "$")) external-file-types)) )
      (find file extensions :test #'(lambda (f regex) (string-match-p regex file) ))))

  (defun sane-open-file (file)
    (if (emacs-or-external-viewer file)
        (helm-open-file-externally file)
      (helm-find-file-or-marked file)))

  (setq helm-external-programs-associations '((("avi" . "mpv")
                                               ("mp4" . "mpv")
                                               ("pdf" . "mupdf")
                                               ("mkv" . "mpv")))
        helm-find-files-actions
        '(("Execute-command" . sane-open-file)
           ("Find File" . helm-find-file-or-marked)
           ("Find file in Dired" . helm-point-file-in-dired)
           ("View file" . view-file)
           ("Checksum File" . helm-ff-checksum)
           ("Query replace fnames on marked" . helm-ff-query-replace-on-marked)
           ("Query replace contents on marked" . helm-ff-query-replace)
           ("Query replace regexp contents on marked" . helm-ff-query-replace-regexp)
           ("Serial rename files" . helm-ff-serial-rename)
           ("Serial rename by symlinking files" . helm-ff-serial-rename-by-symlink)
           ("Serial rename by copying files" . helm-ff-serial-rename-by-copying)
           ("Open file with default tool" . helm-open-file-with-default-tool)
           ("Find file in hex dump" . hexl-find-file)
           ("Browse project" . helm-ff-browse-project)
           ("Complete at point `C-c i'" . helm-insert-file-name-completion-at-point)
           ("Insert as org link `C-c @'" . helm-files-insert-as-org-link)
           ("Find shell command `C-c /'" . helm-ff-find-sh-command)
           ("Add marked files to file-cache" . helm-ff-cache-add-file)
           ("Open file externally `C-c C-x, C-u to choose'" . helm-open-file-externally)
           ("Grep File(s) `C-s, C-u Recurse'" . helm-find-files-grep)
           ("Grep current directory with AG" . helm-find-files-ag)
           ("Git grep" . helm-ff-git-grep)
           ("Zgrep File(s) `M-g z, C-u Recurse'" . helm-ff-zgrep)
           ("Gid" . helm-ff-gid)
           ("Switch to Eshell `M-e'" . helm-ff-switch-to-eshell)
           ("Etags `M-., C-u reload tag file'" . helm-ff-etags-select)
           ("Eshell command on file(s) `M-!, C-u take all marked as arguments.'" . helm-find-files-eshell-command-on-file)
           ("Find file as root `C-c r'" . helm-find-file-as-root)
           ("Find alternate file" . find-alternate-file)
           ("Ediff File `C-c ='" . helm-find-files-ediff-files)
           ("Ediff Merge File `M-='" . helm-find-files-ediff-merge-files)
           ("Delete File(s) `M-D'" . helm-delete-marked-files)
           ("Copy file(s) `M-C, C-u to follow'" . helm-find-files-copy)
           ("Rename file(s) `M-R, C-u to follow'" . helm-find-files-rename)
           ("Backup files" . helm-find-files-backup)
           ("Symlink files(s) `M-S, C-u to follow'" . helm-find-files-symlink)
           ("Relsymlink file(s) `C-u to follow'" . helm-find-files-relsymlink)
           ("Hardlink file(s) `M-H, C-u to follow'" . helm-find-files-hardlink)
           ("Find file other window `C-c o'" . helm-find-files-other-window)
           ("Switch to history `M-p'" . helm-find-files-switch-to-hist)
           ("Find file other frame `C-c C-o'" . find-file-other-frame)
           ("Print File `C-c p, C-u to refresh'" . helm-ff-print)
           ("Locate `C-x C-f, C-u to specify locate db'" . helm-ff-locate))
        helm-type-file-actions
        '((("Execute command on file" . sane-open-file)
           ("Find file" . helm-find-many-files)
           ("Find file as root" . helm-find-file-as-root)
           ("Find file other window" . helm-find-files-other-window)
           ("Find file other frame" . find-file-other-frame)
           ("Open dired in file's directory" . helm-open-dired)
           ("Grep File(s) `C-u recurse'" . helm-find-files-grep)
           ("Zgrep File(s) `C-u Recurse'" . helm-ff-zgrep)
           ("Pdfgrep File(s)" . helm-ff-pdfgrep)
           ("Insert as org link" . helm-files-insert-as-org-link)
           ("Checksum File" . helm-ff-checksum)
           ("Ediff File" . helm-find-files-ediff-files)
           ("Ediff Merge File" . helm-find-files-ediff-merge-files)
           ("Etags `M-., C-u reload tag file'" . helm-ff-etags-select)
           ("View file" . view-file)
           ("Insert file" . insert-file)
           ("Add marked files to file-cache" . helm-ff-cache-add-file)
           ("Delete file(s)" . helm-delete-marked-files)
           ("Copy file(s) `M-C, C-u to follow'" . helm-find-files-copy)
           ("Rename file(s) `M-R, C-u to follow'" . helm-find-files-rename)
           ("Symlink files(s) `M-S, C-u to follow'" . helm-find-files-symlink)
           ("Relsymlink file(s) `C-u to follow'" . helm-find-files-relsymlink)
           ("Hardlink file(s) `M-H, C-u to follow'" . helm-find-files-hardlink)
           ("Open file externally (C-u to choose)" . helm-open-file-externally)
           ("Open file with default tool" . helm-open-file-with-default-tool)
           ("Find file in hex dump" . hexl-find-file)))))

(require 'helm-icons)
(helm-icons-enable)

(use-package! helm-ring
  :bind ("C-S-y" . helm-show-kill-ring))

(after! helm
  (helm-autoresize-mode +1)
  (setq helm-autoresize-min-height 35
        helm-autoresize-max-height helm-autoresize-min-height))

(after! helm
  (require 'historian)
  (require 'helm-flx-historian)
  (helm-flx-historian-mode +1))

(after! (:all dired emms)
  (define-key dired-mode-map (kbl-kbd "@") #'emms-play-dired)
  (after! ranger
    (define-key ranger-mode-map (kbl-kbd "@") #'emms-play-dired)))

(after! dired
  (defun dired--zip-file (file)
    (let ((zip-file (if (string-match ".zip$" file) file
                      (concat (file-name-sans-extension file) ".zip"))))
      (unless (equal file zip-file)
        (shell-command
         (concat "zip -r9 "
                 (shell-quote-argument zip-file)
                 " "
                 (shell-quote-argument file))))))

  (defvar zip-recursively--number-of-workers 0)
  (defvar zip-recursively--max-number-of-workers 100)
  (defvar zip-recursively--untouched-files nil)
  (defvar zip-recursively--timer nil)
  (defvar zip-recursively--timer-interval 0.1)

  (defun zip-recursively--daemon ()
    (if zip-recursively--untouched-files
        (while (> zip-recursively--max-number-of-workers zip-recursively--number-of-workers)
          (zip-recursively--start-worker (pop zip-recursively--untouched-files)))
      (cancel-timer zip-recursively--timer)
      (setq zip-recursively--timer nil)
      (setq zip-recursively--number-of-workers 0)))

  (defun dired--zip-file-recursively (file)
    (if (file-directory-p file)
        (setq zip-recursively--untouched-files
              (cl-union zip-recursively--untouched-files
                        (directory-files-recursively file ".*")))
      (add-to-list 'zip-recursively--untouched-files file))
    (unless (timerp zip-recursively--timer)
      (setq zip-recursively--timer
            (run-with-timer 0 zip-recursively--timer-interval
                            #'zip-recursively--daemon))))

  (defun zip-recursively--start-worker (file)
    (cl-incf zip-recursively--number-of-workers)
    (let ((zip-file (if (string-match ".zip$" file) file
                      (concat (file-name-sans-extension file) ".zip"))))
      (set-process-sentinel
       (start-process (concat "zip: " file) nil "zip" "-r9" zip-file file)
       `(lambda (process event)
          (when (string= event "finished\n")
            (set-process-sentinel
             (start-process (concat "rm: " ,file) nil "rm" ,file)
             (lambda (&rest _)
               (cl-decf zip-recursively--number-of-workers))))))))

  (defun dired-zip-files (arg)
    (interactive "P")
    "Create an archive containing the marked files."
    ;; create the zip file
    (if arg
        (mapcar #'dired--zip-file-recursively (dired-get-marked-files))
      (let ((max-lisp-eval-depth most-positive-fixnum)
            (max-specpdl-size most-positive-fixnum))
        (mapcar #'dired--zip-file
                (-map #'file-name-nondirectory (dired-get-marked-files)))))
    (revert-buffer)

    ;; remove the mark on all the files  "*" to " "
    ;; (dired-change-marks 42 ?\040)
    ;; mark zip file
    ;; (dired-mark-files-regexp (filename-to-regexp zip-file))
    )

  (define-key dired-mode-map (kbd "C-M-x") 'dired-zip-files))

;;; dired-fixups.el --- fixups for dired mode

;; Author: Dino Chiesa
;; Created: Sat, 31 Mar 2012  10:31
;; Version: 0.1
;;

(require 'ls-lisp)

    ;; (defun ls-lisp-format-time (file-attr time-index now)
    ;;   "################")

(defun ls-lisp-format-file-size (file-size human-readable)
  "This is a redefinition of the function from `dired.el'. This
fixes the formatting of file sizes in dired mode, to support very
large files. Without this change, dired supports 8 digits max,
which is up to 10gb.  Some files are larger than that.
"
  (if (or (not human-readable)
          (< file-size 1024))
      (format (if (floatp file-size) " %11.0f" " %11d") file-size)
    (do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
         ;; kilo, mega, giga, tera, peta, exa
         (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
        ((< file-size 1024) (format " %10.0f%s"  file-size (car post-fixes))))))


(defun dired-sort-toggle ()
  "This is a redefinition of the fn from dired.el. Normally,
dired sorts on either name or time, and you can swap between them
with the s key.  This function one sets sorting on name, size,
time, and extension. Cycling works the same.
"
  (setq dired-actual-switches
        (let (case-fold-search)
          (cond
           ((string-match " " dired-actual-switches) ;; contains a space
            ;; New toggle scheme: add/remove a trailing " -t" " -S",
            ;; or " -U"
            ;; -t = sort by time (date)
            ;; -S = sort by size
            ;; -X = sort by extension

            (cond

             ((string-match " -t\\'" dired-actual-switches)
              (concat
               (substring dired-actual-switches 0 (match-beginning 0))
               " -X"))

             ((string-match " -X\\'" dired-actual-switches)
              (concat
               (substring dired-actual-switches 0 (match-beginning 0))
               " -S"))

             ((string-match " -S\\'" dired-actual-switches)
              (substring dired-actual-switches 0 (match-beginning 0)))

             (t
              (concat dired-actual-switches " -t"))))

           (t
            ;; old toggle scheme: look for a sorting switch, one of [tUXS]
            ;; and switch between them. Assume there is only ONE present.
            (let* ((old-sorting-switch
                    (if (string-match (concat "[t" dired-ls-sorting-switches "]")
                                      dired-actual-switches)
                        (substring dired-actual-switches (match-beginning 0)
                                   (match-end 0))
                      ""))

                   (new-sorting-switch
                    (cond
                     ((string= old-sorting-switch "t") "X")
                     ((string= old-sorting-switch "X") "S")
                     ((string= old-sorting-switch "S") "")
                     (t "t"))))
              (concat
               "-l"
               ;; strip -l and any sorting switches
               (dired-replace-in-string (concat "[-lt"
                                                dired-ls-sorting-switches "]")
                                        ""
                                        dired-actual-switches)
               new-sorting-switch))))))

  (dired-sort-set-modeline)
  (revert-buffer))


(defun dired-sort-set-modeline ()
 "This is a redefinition of the fn from `dired.el'. This one
properly provides the modeline in dired mode, supporting the new
search modes defined in the new `dired-sort-toggle'.
"
  ;; Set modeline display according to dired-actual-switches.
  ;; Modeline display of "by name" or "by date" guarantees the user a
  ;; match with the corresponding regexps.  Non-matching switches are
  ;; shown literally.
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond ((string-match "^-[^t]*t[^t]*$" dired-actual-switches)
                   "Dired by time")
                  ((string-match "^-[^X]*X[^X]*$" dired-actual-switches)
                   "Dired by ext")
                  ((string-match "^-[^S]*S[^S]*$" dired-actual-switches)
                   "Dired by sz")
                  ((string-match "^-[^SXUt]*$" dired-actual-switches)
                   "Dired by name")
                  (t
                   (concat "Dired " dired-actual-switches)))))
    (force-mode-line-update)))


(provide 'dired-fixups)

;;; dired-fixups.el ends here

(after! ranger
  (setq ranger-cleanup-eagerly t))

(after! dired
  (add-hook 'dired-mode-hook
            (defun hl-line-mode-enable (&rest _)
              (hl-line-mode +1))))

(when (featurep! :emacs dired +ranger)
  (after! ranger
    (ranger-override-dired-mode -1)))

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
        :n "RET" #'cfw:org-onclick
        :n "<tab>" #'cfw:details-navi-next-item-command
        :n "<backtab>" #'cfw:details-navi-prev-item-command
        :n "j" #'cfw:details-navi-next-item-command
        :n "k" #'cfw:details-navi-prev-item-command
        :n "zd" #'cfw:change-view-day
        :n "zw" #'cfw:change-view-week
        :n "zt" #'cfw:change-view-two-weeks
        :n "zm" #'cfw:change-view-month)
  (defun cfw:org-clean-exit-restore-focus-advice (&rest _)
    (delete-window)
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
      (cfw:org-create-source (face-foreground 'default)))))

  (setq +calendar-open-function #'my-open-calendar))

(after! org
  ;; (add-hook! 'exwm-init-hook
  ;;   (after! persp-mode
  ;;     (let ((workspace-current (+workspace-current-name)))
  ;;       (+workspace-switch ".doom.d" t)
  ;;       (when (file-exists-p (expand-file-name "config.org" doom-private-dir))
  ;;         (find-file (expand-file-name "config.org" doom-private-dir)))
  ;;       (+workspace-switch workspace-current))))
  (require 'org-agenda)
  (org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))

  (use-package! secretaria
    :defer-incrementally t
    :config
    (setq secretaria-clocked-task-save-file
          (expand-file-name ".local/secretaria-clocked-task" doom-private-dir)
          secretaria-clocked-in-reminder-every-minutes 5)

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
                       " days)")
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

(add-to-list 'doom-incremental-packages 'projectile)

(use-package! org-pandoc-import
  :defer-incrementally t
  :config
  (org-pandoc-import-transient-mode +1))

(after! org
  (el-patch-defun org-do-emphasis-faces (limit)
    "Run through the buffer and emphasize strings."
    (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
                            (car org-emphasis-regexp-components))))
      (catch :exit
        (while (re-search-forward quick-re limit t)
          (let* ((marker (match-string 2))
                 (verbatim? (member marker '("~" "="))))
            (when (save-excursion
                    (goto-char (match-beginning 0))
                    (and
                     ;; Do not match table hlines.
                     (not (and (equal marker "+")
                               (org-match-line
                                "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
                     ;; Do not match headline stars.  Do not consider
                     ;; stars of a headline as closing marker for bold
                     ;; markup either.
                     (not (and (equal marker "*")
                               (save-excursion
                                 (forward-char)
                                 (skip-chars-backward "*")
                                 (looking-at-p org-outline-regexp-bol))))
                     ;; Match full emphasis markup regexp.
                     (looking-at (if verbatim? org-verbatim-re org-emph-re))
                     ;; Do not span over paragraph boundaries.
                     (not (string-match-p org-element-paragraph-separate
                                          (match-string 2)))
                     ;; Do not span over cells in table rows.
                     (not (and (save-match-data (org-match-line "[ \t]*|"))
                               (string-match-p "|" (match-string 4))))))
              (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
                          (m (if org-hide-emphasis-markers 4 2)))
                (font-lock-prepend-text-property
                 (match-beginning m) (match-end m) 'face face)
                (when verbatim?
                  (org-remove-flyspell-overlays-in
                   (match-beginning 0) (match-end 0))
                  (remove-text-properties (match-beginning 2) (match-end 2)
                                          '(display t invisible t intangible t)))
                (add-text-properties (match-beginning 2) (match-end 2)
                                     '(font-lock-multiline t org-emphasis t))
                (el-patch-swap (when (and org-hide-emphasis-markers
                                          (not (org-at-comment-p)))
                                 (add-text-properties (match-end 4) (match-beginning 5)
                                                      '(invisible org-link))
                                 (add-text-properties (match-beginning 3) (match-end 3)
                                                      '(invisible org-link)))
                               (when (and org-hide-emphasis-markers
                                          (not (org-at-comment-p)))
                                 (let ((s1 (match-beginning 3))
                                       (e1 (match-end 3))
                                       (s2 (match-end 4))
                                       (e2 (match-beginning 5)))
                                   (add-text-properties s2 e2 '(invisible org-link))
                                   (add-text-properties s1 e1 '(invisible org-link))
                                   (add-text-properties s1 e2
                                                        `(org-emph-start ,s1 org-emph-end ,e2)))))
                (throw :exit t))))))))

  (defvar-local sbr-org-emphasize--current-symbol-bounds nil)

  (defcustom sbr-org-emphasize-unemphasize-at-point nil
    "If non-nil, show the non-emphasized version of a symbol when point is on it.
If set to the symbol `right-edge', also unemphasize if point
is immediately after the symbol.  The emphasization will be
reapplied as soon as point moves away from the symbol.  If
set to nil, the emphasization persists even when point is
on the symbol."
    :version "25.1"
    :type '(choice (const :tag "Never unemphasize" nil)
                   (const :tag "Unemphasize when point is inside" t)
                   (const :tag "Unemphasize when point is inside or at right edge" right-edge))
    :group 'org-mode)


  (defun sbr--get-prop-as-list (prop)
    "Helper function to get sbr-org-emphasize properties as a list.
If `sbr-org-emphasize-unemphasize-at-point' is set to `t' then
return the text property PROP at point in a list. If
`sbr-org-emphasize-unemphasize-at-point' is set to `right-edge',
the also include the text property PROP at point-1 unless we are
at the beginning of the buffer."
    (remove nil
            (list (get-text-property (point) prop)
                  (when (and (eq sbr-org-emphasize-unemphasize-at-point 'right-edge)
                             (not (bobp)))
                    (get-text-property (1- (point)) prop)))))

  (defun sbr-org-emphasize--post-command-hook ()
    ;; Re-apply emphasis to the previous symbol.
    (when (and sbr-org-emphasize--current-symbol-bounds
               (or (< (point) (car sbr-org-emphasize--current-symbol-bounds))
                   (> (point) (cadr sbr-org-emphasize--current-symbol-bounds))
                   (and (not (eq sbr-org-emphasize-unemphasize-at-point 'right-edge))
                        (= (point) (cadr sbr-org-emphasize--current-symbol-bounds)))))
      (apply #'font-lock-flush sbr-org-emphasize--current-symbol-bounds)
      (setq sbr-org-emphasize--current-symbol-bounds nil))
    ;; Unemphasize the current symbol.
    (when-let* ((s (sbr--get-prop-as-list 'org-emph-start))
                (e (sbr--get-prop-as-list 'org-emph-end))
                (s (apply #'min s))
                (e (apply #'max e)))
      (with-silent-modifications
        (setq sbr-org-emphasize--current-symbol-bounds (list s e))
        (remove-text-properties s (1+ s) '(invisible org-link))
        (remove-text-properties (1- e) e '(invisible org-link)))))

  (define-minor-mode sbr-org-emphasize-mode
    "Toggle SBR Org Emphasize mode.
With a prefix argument ARG, enable SBR Org-Emphasize mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
    :init-value nil
    (if sbr-org-emphasize-mode
        ;; Turn on
        (progn
          (setq-local font-lock-extra-managed-props
                      (append font-lock-extra-managed-props
                              '(org-emph-start org-emph-end)))
          (when sbr-org-emphasize-unemphasize-at-point
            (add-hook 'post-command-hook
                      #'sbr-org-emphasize--post-command-hook nil t))
          (font-lock-flush))
      ;; Turn off
      (remove-hook 'post-command-hook #'sbr-org-emphasize--post-command-hook t)))

  (setq sbr-org-emphasize-unemphasize-at-point 'right-edge
        org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook
            (defun sbr-org-emphasize-mode-enable ()
                (sbr-org-emphasize-mode +1))))

(after! org-archive
  (setq org-archive-subtree-save-file-p nil))

(after! org
  (use-package! org-random-todo
    :defer-incrementally t
    :commands (org-random-todo-mode
               org-random-todo
               org-random-todo-goto-current
               org-random-todo-goto-new)
    :config
    (setq org-random-todo-how-often 1500)
    (org-random-todo-mode 1))

  (after! alert
    (alert-add-rule :mode 'org-mode
                    :category "random-todo"
                    :style 'notifications
                    :continue t)))

(after! secretaria
  (defun my-org-agenda-to-appt ()
    (interactive)
    (setq appt-time-msg-list nil)
    (let ((org-deadline-warning-days 0))    ;; will be automatic in org 5.23
      (org-agenda-to-appt)))

  ;; (defun aj/appt-notify (until time msg)
  ;;   "Use `alert' to for appointment notifications."
  ;;   (if (listp msg)
  ;;       (dolist (i (number-sequence 0 (1- (length until))))
  ;;         (alert (nth i msg) :title "Appointment Reminder" :category 'calendar))
  ;;     (alert msg :title "Appointment Reminder" :category 'calendar)))

  ;; Advice the agenda refresh to update appts.
  (defadvice org-agenda-redo (after update-appts activate)
    "Update `appt' lists from the agenda."
    (message "Updating appointments...")
    (my-org-agenda-to-appt))

  (my-org-agenda-to-appt)
  (appt-activate +1)
  (setq appt-message-warning-time 12
        appt-display-interval 3
        appt-display-mode-line t
        ;; appt-disp-window-function #'aj/appt-notify
        ;; appt-delete-window-function #'ignore
        ))

(after! org-roam
  (setq org-roam-graph-viewer (executable-find "qutebrowser")))

(after! org-roam
  (el-patch-defun org-roam-capture ()
    "Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'."
    (interactive)
    (when (org-roam-capture--in-process-p)
      (user-error "Nested Org-roam capture processes not supported"))
    (let* ((completions (org-roam--get-title-path-completions))
           (title-with-keys (org-roam-completion--completing-read "File: "
                                                                  completions))
           (res (cdr (assoc title-with-keys completions)))
           (title (or (plist-get res :title) title-with-keys))
           (file-path (plist-get res (el-patch-swap :file-path
                                                    :path))))
      (let ((org-roam-capture--info (list (cons 'title title)
                                          (cons 'slug (org-roam--title-to-slug title))
                                          (cons 'file file-path)))
            (org-roam-capture--context 'capture))
        (setq org-roam-capture-additional-template-props (list :capture-fn 'org-roam-capture))
        (condition-case err
            (org-roam-capture--capture)
          (error (user-error "%s.  Please adjust `org-roam-capture-templates'"
                             (error-message-string err))))))))

(map! :after org
        :prefix ("C-c n r" . "org-roam")
        "b" #'org-roam-switch-to-buffer
        "f" #'org-roam-find-file
        "g" #'org-roam-graph
        "i" #'org-roam-insert
        "m" #'org-roam
        (:prefix ("d" . "by date")
          :desc "Arbitrary date" "d" #'org-roam-dailies-date
          :desc "Today"          "t" #'org-roam-dailies-today
          :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
          :desc "Yesterday"      "y" #'org-roam-dailies-yesterday))

(after! org
  (set-file-template! 'org-mode :ignore t))

(after! org-capture
  ;; (add-to-list 'org-capture-templates
  ;;              `("a" "Appointment" entry
  ;;                ,(list 'file
  ;;                       (concat org-directory "appointments.org"))
  ;;                "* %?\nSCHEDULED: %^T\n%a\n"))
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t))))

(after! org-capture
  (set-popup-rule! "^CAPTURE-" :side 'right :size 0.5)
  (advice-remove #'org-capture
              (defun org-capture--adjust-window-position (&rest _)
                (setq display-buffer-alist
                      (--remove (equalp (car it) "^CAPTURE-")
                                display-buffer-alist))
                (if (one-window-p)
                    (set-popup-rule! "^CAPTURE-" :side 'right :size 0.5)
                  (set-popup-rule! "^CAPTURE-" :side 'bottom :size 0.35)))))

(after! org
  (setq org-adapt-indentation nil
        org-hide-leading-stars t))
(after! org-indent
  (setq org-indent-indentation-per-level 1))

(after! org-clock
  (setq org-clock-history-length 23
        org-clock-out-remove-zero-time-clocks t
        org-clock-persist-file (expand-file-name ".local/org-clock-save.el" doom-private-dir))
  (org-clock-persistence-insinuate))

(after! deft
  (setq deft-directory (expand-file-name "roam" org-directory)))

(after! org
  (use-package! org-web-tools
    :defer-incrementally t))

(after! org
  (defun unpackaged/org-fix-blank-lines (prefix)
    "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
    (interactive "P")
    (org-map-entries (lambda ()
                       (org-with-wide-buffer
                        ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                        ;; newlines before the current heading, so we do this part widened.
                        (while (not (looking-back "\n\n" nil))
                          ;; Insert blank lines before heading.
                          (insert "\n")))
                       (let ((end (org-entry-end-position)))
                         ;; Insert blank lines before entry content
                         (forward-line)
                         (while (and (org-at-planning-p)
                                     (< (point) (point-max)))
                           ;; Skip planning lines
                           (forward-line))
                         (while (re-search-forward org-drawer-regexp end t)
                           ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                           ;; for some reason it doesn't work correctly when operating on hidden text.
                           ;; This works, taken from `org-agenda-get-some-entry-text'.
                           (re-search-forward "^[ \t]*:END:.*\n?" end t)
                           (goto-char (match-end 0)))
                         (unless (or (= (point) (point-max))
                                     (org-at-heading-p)
                                     (looking-at-p "\n"))
                           (insert "\n"))))
                     t (if prefix
                           nil
                         'tree)))

  (add-hook 'org-mode-hook
            (defun org-fix-blank-lines--add-local-hook ()
              (add-hook 'before-save-hook
                        (defun org-fix-blank-lines-in-buffer ()
                          (unpackaged/org-fix-blank-lines '(4)))))))

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

(defvar +org-exit-src-code-hook nil
  "Hook run just before exiting a org source block buffer.")

(defun +org|run-exit-src-code-hooks (&rest _)
  "Runs all hooks in `+org-exit-src-code-hook`."
  (run-hooks '+org-exit-src-code-hook))

(advice-add #'org-edit-src-exit :before #'+org|run-exit-src-code-hooks)

(add-hook '+org-exit-src-code-hook #'ws-butler-trim-eob-lines)

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
  (use-package! org-sidebar
    :bind (:map org-mode-map
           ("<f6>" . org-sidebar-toggle)
           ("<f7>" . org-sidebar-tree-toggle))
    :custom
    (org-sidebar-side 'left)
    (org-sidebar-tree-side 'left)))

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
  (add-hook 'org-capture-mode-hook
            (defun org-capture--insert-timestamp ()
              (when (org-at-heading-p)
                (akirak/org-set-created-timestamp)))))

(after! org-noter
  (setq org-noter-default-notes-file-names
        '("noter-notes.org")
        org-noter-notes-search-path '("~/org")
        org-noter-always-create-frame nil
        org-noter-arrow-delay 0.1
        org-noter-doc-split-fraction (cons 0.5 0.5)
        org-noter-insert-note-no-questions t)
  (add-hook 'org-noter-insert-heading-hook
            #'akirak/org-set-created-timestamp))

(after! org-noter
  (el-patch-define-minor-mode org-noter-doc-mode
    "Minor mode for the document buffer.
Keymap:
\\{org-noter-doc-mode-map}"
    :keymap `((,(kbd   "i")   . org-noter-insert-note)
              (,(kbd "C-i")   . org-noter-insert-note-toggle-no-questions)
              (,(kbd "M-i")   . org-noter-insert-precise-note)
              (,(kbd   "q")   . org-noter-kill-session)
              (,(kbd "M-p")   . org-noter-sync-prev-page-or-chapter)
              (,(kbd "M-.")   . org-noter-sync-current-page-or-chapter)
              (,(kbd "M-n")   . org-noter-sync-next-page-or-chapter)
              (,(kbd "C-M-p") . org-noter-sync-prev-note)
              (,(kbd "C-M-.") . org-noter-sync-current-note)
              (,(kbd "C-M-n") . org-noter-sync-next-note))

    (el-patch-remove (let ((mode-line-segment '(:eval (org-noter--mode-line-text))))
                       (if org-noter-doc-mode
                           (if (symbolp (car-safe mode-line-format))
                               (setq mode-line-format (list mode-line-segment mode-line-format))
                             (push mode-line-segment mode-line-format))
                         (setq mode-line-format (delete mode-line-segment mode-line-format)))))))

(after! org-noter
    (defvar org-noter-session-workspace-alist nil)

    (advice-add #'org-noter--setup-windows
                :before
                (defun org-noter--setup-workspace (session)
                  (when (org-noter--valid-session session)
                    (+workspace-switch
                     (progn (setf (alist-get session org-noter-session-workspace-alist)
                                  (s-truncate 15 (-> session
                                                    (org-noter--session-notes-buffer)
                                                    (buffer-file-name)
                                                    (file-name-base)
                                                    (substring nil -4)) "..."))
                            (alist-get session org-noter-session-workspace-alist))
                     t))))

    (advice-add #'org-noter-kill-session
                :before
                (defun org-noter--delete-workspace (&optional session)
                  (+workspace-delete (alist-get (or session org-noter--session)
                                                org-noter-session-workspace-alist))
                  (run-at-time 0 nil
                               #'+workspace/other))))

(after! org
  (advice-add #'org-entry-get
              :around
              (defun org-entry-get--eval-lisp-forms (oldfun &rest args)
                (let ((output (apply oldfun args)))
                  (when (and (stringp output) (string-match "^%(.*)$" output))
                    (setq output (eval (read (substring-no-properties output 1 nil)))))
                  output))))

(use-package! org-special-block-extras
  :hook (org-mode . org-special-block-extras-mode))

(use-package! org-treeusage
  :bind (:map org-mode-map
         ("C-c C-%" . org-treeusage-mode))
  :custom (org-treescope-overlay-header nil))

(modify-coding-system-alist 'file "\\.org\\'" 'utf-8)

(after! org
  (map! :map org-mode-map
        "C-c <C-backspace>" #'org-up-element))

(use-package! decide
  :hook (org-mode . decide-mode)
  :defer-incrementally t
  :config
  (map! :map decide-mode-map
        "? ESC" (cmd! (insert-char ??)
                         (evil-normal-state))
        "?\\" (cmd! (insert "?\\")
                       (+org-realign-table-maybe-a))))

(setq
 org-priority-faces '((?A . error)
                      (?B . warning)
                      (?C . success)
                      (?D . default))
 org-highest-priority ?A
 org-lowest-priority ?D
 org-default-priority ?D)

(after! org-pomodoro
  (setq org-pomodoro-manual-break t))

(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :defer-incrementally t)

(after!  org-capture
  (setq org-capture-bookmark nil))

(use-package! org-superstar
  :init
  (add-hook! 'org-mode-hook
    (org-superstar-mode +1)))

(add-hook 'org-mode-hook #'visual-line-mode)

(add-hook! 'org-mode-hook
  (setq display-line-numbers nil))

(add-hook 'org-mode-hook #'rainbow-delimiters-mode-disable)

(after! org-keys
  (setq org-use-speed-commands t))

(after! org
  (setq org-startup-with-inline-images t
        org-image-actual-width 800)
  (add-hook 'org-mode-hook
            (defun org-display-inline-images--force ()
              (org-display-inline-images t t))))

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
               '("" "newunicodechar"))
  (add-to-list 'org-latex-packages-alist
               '("all" "xy")))

(after! org
  (setq org-pretty-entities-include-sub-superscripts nil))

(after! org
  (setq org-latex-pdf-process '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")
        org-preview-latex-default-process 'dvisvgm))

(after! org
  (setq org-startup-with-latex-preview nil)

  (defun org-latex--prettify-escaped-braces (limit)
    (when (re-search-forward "\\\\\\([{}$]\\)" limit t)
      (put-text-property (match-beginning 0) (match-end 0) 'display
                         (match-string-no-properties 1))
      t))

  (defun org-add-my-math-highlighting ()
    (add-to-list 'org-font-lock-extra-keywords
                 '(org-latex--prettify-escaped-braces)))

  (add-hook 'org-font-lock-set-keywords-hook #'org-add-my-math-highlighting))

(after! org
  (load (expand-file-name "lattie" doom-private-dir) t t)
  (setq org-highlight-latex-and-related nil)
  (remove-hook 'org-mode-hook 'org-cdlatex-mode))
(add-hook 'org-mode-hook
          (defun org-roam-activate-math ()
            (when (and (bound-and-true-p org-roam-directory)
                       (stringp buffer-file-name)
                       (string-prefix-p org-roam-directory
                                        buffer-file-name))
              (setq org-highlight-latex-and-related '(native script entities))
              (org-cdlatex-mode 1)
              (org-set-regexps-and-options))))

(add-hook 'doom-switch-buffer-hook
          (defun org--activate-deactivate-math-maybe ()
            (if (and (bound-and-true-p org-roam-directory)
                     (stringp buffer-file-name)
                     (string-prefix-p org-roam-directory
                                      buffer-file-name))
                (setq org-highlight-latex-and-related '(native script entities))
              (setq org-highlight-latex-and-related nil))))

(add-transient-hook! 'org-cdlatex-mode-hook
  (map! :map org-cdlatex-mode-map
        "]" #'lattie-close-bracket
        "[" #'lattie-open-bracket
        "(" #'lattie-open-paren
        "n" #'special-lattie-down
        ;; "-" #'special-lattie-punctuation
        "-" #'lattie-self-insert-command
        "SPC" #'special-lattie-space
        "RET" #'special-lattie-newline-and-indent
        "^" #'special-lattie-underscore-caret
        [remap org-cdlatex-underscore-caret] #'special-lattie-underscore-caret
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
        "^" #'special-lattie-back-to-heading
        ;; "^" #'org-cdlatex-underscore-caret
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
        [tab] #'special-lattie-tab
        "C-c C-t" #'lattie-org-todo
        :map evil-org-mode-map
        :i [return] nil
        :i "RET" nil)

  (add-hook 'evil-org-mode-hook
            (defun lattie--remap-return ()
              (map! :map evil-org-mode-map
                    :i "<return>" #'special-lattie-newline-and-indent
                    :i "RET" #'special-lattie-newline-and-indent))))

(add-hook 'org-mode-hook #'auto-fill-mode)

(after! org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))

(after! org
  (defun get-newest-picture-file ()
    "Get latest file (including directory) in PATH."
    (require 'picpocket)
    (car (sort (directory-files-recursively "~/Pictures" picpocket-picture-regexp)
               #'file-newer-than-file-p)))

  (defun org-insert-image--internal (image-file)
    (require 'dired-aux)
    (let* ((infile image-file)
           (outdir (concat (file-name-directory (buffer-file-name)) "/media"))
           (outfile (expand-file-name (file-name-nondirectory infile) outdir)))
      (unless (file-directory-p outdir)
        (make-directory outdir t))
      (dired-hardlink infile outfile t)
      (insert (concat (concat "[[./media/" (file-name-nondirectory outfile)) "]]")))
    (newline)
    (newline))
  
  (defun org-insert-image ()
    "Moves image from Dropbox folder to ./media, inserting org-mode link"
    (interactive)
    (org-insert-image--internal (read-file-name "Image file: " "~/Pictures")))

  (defun org-insert-newest-image ()
    (interactive)
    (org-insert-image--internal (get-newest-picture-file))
    (org-display-inline-images t))

  (map! :map org-mode-map :localleader
        "lR" #'org-insert-newest-image
        "lI" #'org-insert-image)
  
  (after! picpocket
    (defun picpocket-insert-org-link-to-current-image ()
      (interactive)
      (when-let ((buf (--first (apply #'provided-mode-derived-p
                                      (buffer-local-value 'major-mode it)
                                      '(org-mode))
                               (doom-visible-buffers))))
        (with-current-buffer buf
          (org-insert-image--internal (picpocket-absfile))
          (org-display-inline-images t))))

    (map! :map picpocket-mode-map
          :g "o" #'picpocket-insert-org-link-to-current-image)))

(use-package! org-preview-html
  :commands (org-preview-html/preview
             org-preview-html-mode))

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

  (advice-add 'org-meta-return :override #'modi/org-meta-return))

(use-package! org-bookmark-heading
  :after org
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
  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto))
        org-pretty-entities t)

  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  (setq org-cycle-separator-lines 0
        org-catch-invisible-edits 'show-and-error))

(after! org-id
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package! org-make-toc
  :hook (org-mode . org-make-toc-mode)
  :defer-incrementally t
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

(after! org-agenda
  (setq org-agenda-sticky t
        org-agenda-start-with-clockreport-mode t))
        ;; org-agenda-files (list (expand-file-name "mobile.org" org-directory)
        ;;                        (expand-file-name "todo.org" org-directory)))

(advice-add #'org-check-agenda-file :around
            (defun org--ignore-non-org-file-a (oldfun file)
              (if (string-match-p "\\.org$" file)
                  (funcall oldfun file)
                (org-remove-file file)
                nil)))

(after! bibtex
  (setq
   bibtex-completion-notes-path (expand-file-name "~/org/roam")
   bibtex-completion-bibliography (expand-file-name
                                   "~/documents/zotero-library.bib")
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    ;; "#+TITLE: ${title}\n"
    "${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n")))

 (use-package! org-roam-bibtex
   :after (org-roam)
   :hook (org-roam-mode . org-roam-bibtex-mode)
   :config
   (require 'bibtex-completion)
   (setq org-roam-bibtex-preformat-keywords
         '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
   (setq orb-templates
         '(("r" "ref" plain #'org-roam-capture--get-point
            ""
            :file-name "${slug}"
            :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

            :unnarrowed t))))

(use-package! org-ref
  :defer-incrementally t
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography
   (list (expand-file-name "~/documents/zotero-library.bib"))
   org-ref-bibliography-notes (expand-file-name "~/org/roam/bibnotes.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory (expand-file-name "~/org/roam")
   org-ref-notes-function 'orb-edit-notes))

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

(use-package mu4e-views
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
	 ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	 ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	 ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
	 )
  :config
  (map! :map mu4e-headers-mode-map
        :n "v" #'mu4e-views-mu4e-select-view-msg-method)
  (setq mu4e-views-completion-method 'ivy)     ;; use ivy for completion
  (setq mu4e-views-default-view-method "text") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "text") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)) ;; when pressing n and p stay in the current window

(map! :leader
      :desc "Mu4e" "am" #'=mu4e)
(after! mu4e
  (map! (:map (mu4e-view-mode-map
               mu4e-headers-mode-map)
         :n "C-n" #'mu4e-view-headers-next
         :n "C-p" #'mu4e-view-headers-prev
         :g "C-c C-n" #'mu4e-view-headers-next
         :g "C-c C-p" #'mu4e-view-headers-prev)))

(advice-add #'=mu4e :after
            (defun =mu4e--maxmize-a (&rest _)
              (delete-other-windows)))

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
  (add-hook! 'mu4e-headers-mode-hook
    (setq hl-line-sticky-flag t))
  (set-face-attribute 'mu4e-header-highlight-face nil :weight 'normal))

(after! mu4e
  (add-hook 'mu4e-view-mode-hook
            (defun mu4e-clean-up-whitespace-in-message (&rest _)
              (read-only-mode -1)
              (whitespace-cleanup)
              (read-only-mode 1))))

(after! mu4e
  (setq mu4e-update-interval nil
        mu4e-update-func #'ignore
        mu4e-get-mail-command "true" ;; Do not fetch mail.
        mu4e-index-update-in-background nil)
  (defvar mu4e-update-index-timer nil)
  (setq mu4e-update-index-timer
        (run-with-timer 5 5 #'mu4e-update-index)))

(after! mu4e
  (setq mu4e-save-multiple-attachments-without-asking t
        mu4e-compose-dont-reply-to-self t
        mu4e-cache-maildir-list t
        mu4e-view-use-gnus t
        mu4e-use-fancy-chars nil
        mu4e-headers-fields '(;; (:account . 12)
                              (:human-date . 12)
                              (:flags . 4)
                              ;; (:size . 6)
                              (:mailing-list . 16)
                              (:from . 28)
                              (:subject)))

  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (setq mu4e-bookmarks
        '((:name "Unread messages"
            :query "flag:unread AND NOT flag:trashed AND maildir:/outlook/Inbox"
            :key ?u)
          (:name "Today's messages"
            :query "date:today..now AND NOT flag:trashed AND maildir:/outlook/Inbox"
            :key ?t)
          (:name "Last 7 days"
            :query "date:7d..now AND NOT flag:trashed AND maildir:/outlook/Inbox"
            :key ?w)
          (:name "Flagged"
            :query "maildir:/outlook/Inbox and flag:flagged"
            :key ?f)
          (:name "Inbox"
            :query "maildir:/outlook/Inbox AND NOT flag:trashed"
            :key ?i)
          (:name "Drafts"
            :query "flag:draft AND NOT flag:trashed"
            :key ?d))))

(after! mu4e
  (el-patch-defun mu4e~stop ()
    "Stop the mu4e session."
    (when mu4e~update-timer
      (cancel-timer mu4e~update-timer)
      (setq mu4e~update-timer nil))
    (mu4e-clear-caches)
    (mu4e~proc-kill)
    ;; kill all mu4e buffers
    (mapc
     (lambda (buf)
       (with-current-buffer buf
           (when (member major-mode
                           '(mu4e-headers-mode mu4e-view-mode mu4e-main-mode))
             (kill-buffer))))
     (el-patch-swap (buffer-list)
                    (-filter #'buffer-live-p (buffer-list))))))

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
  ;; (after! gnus
  ;;   (require 'ebdb-gnus))
  ;; (after! mu4e
  ;;   (require 'ebdb-mu4e))
  (bbdb-initialize 'gnus 'message))

(use-package! bbdb-vcard
  :commands (bbdb-vcard-import-file bbdb-vcard-export))

(after! vterm
  (setq vterm-shell "/run/current-system/sw/bin/bash"))

(after! esh-module
  (setq eshell-module-list
        (delq 'eshell-banner eshell-modules-list)))

(use-package! eshell-syntax-highlighting
  :after esh-mode
  :config (eshell-syntax-highlighting-global-mode +1))

(use-package! pcomplete-declare
  :after pcomplete)

(use-package! pcmpl-git
  :after pcomplete)

(after! eshell-z
  (defalias 'eshell/p (symbol-function #'eshell/z)))

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

(after! em-cmpl
  (el-patch-defun eshell-complete-parse-arguments ()
    "Parse the command line arguments for `pcomplete-argument'."
    (when (and eshell-no-completion-during-jobs
                 (eshell-interactive-process))
      (insert-and-inherit "\t")
      (throw 'pcompleted t))
    (let ((end (point-marker))
            (begin (save-excursion (eshell-bol) (point)))
            (posns (list t))
            args delim)
      (when (memq this-command '(pcomplete-expand
                                       pcomplete-expand-and-complete))
        (run-hook-with-args 'eshell-expand-input-functions begin end)
        (if (= begin end)
              (end-of-line))
        (setq end (point-marker)))
      (if (setq delim
                  (catch 'eshell-incomplete
                      (ignore
                       (setq args (eshell-parse-arguments begin end)))))
            (cond ((memq (car delim) '(?\{ ?\<))
                   (setq begin (1+ (cadr delim))
                           args (eshell-parse-arguments begin end)))
                  ((eq (car delim) ?\()
                   (eshell-complete-lisp-symbol)
                   (throw 'pcompleted t))
                  (t
                   (insert-and-inherit "\t")
                   (throw 'pcompleted t))))
      (when (get-text-property (el-patch-swap (1- end)
                                              (max (1- end) 1))
                               'comment)
        (insert-and-inherit "\t")
        (throw 'pcompleted t))
      (let ((pos begin))
        (while (< pos end)
            (if (get-text-property pos 'arg-begin)
                (nconc posns (list pos)))
            (setq pos (1+ pos))))
      (setq posns (cdr posns))
      (cl-assert (= (length args) (length posns)))
      (let ((a args)
              (i 0)
              l)
        (while a
            (if (and (consp (car a))
                       (eq (caar a) 'eshell-operator))
                (setq l i))
            (setq a (cdr a) i (1+ i)))
        (and l
               (setq args (nthcdr (1+ l) args)
                       posns (nthcdr (1+ l) posns))))
      (cl-assert (= (length args) (length posns)))
      (when (and args (eq (char-syntax (char-before end)) ? )
                   (not (eq (char-before (1- end)) ?\\)))
        (nconc args (list ""))
        (nconc posns (list (point))))
      (cons (mapcar
               (function
                (lambda (arg)
                  (let ((val
                           (if (listp arg)
                                 (let ((result
                                          (eshell-do-eval
                                           (list 'eshell-commands arg) t)))
                                   (cl-assert (eq (car result) 'quote))
                                   (cadr result))
                             arg)))
                      (if (numberp val)
                          (setq val (number-to-string val)))
                      (or val ""))))
               args)
              posns))))

(add-transient-hook! 'eshell-mode-hook
  (after! company-files
    (add-to-list 'company-files--regexps
                 "\\(?:[ 	=[]\\|^\\)\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)\\(?:[^\\]\\\\[ 	\n]\\|[^ 	\n]\\)*\\)")

    (defun company-files--unquote-string (s)
      (apply #'s-concat
             (--map (if (and (eq (length it) 2)
                             (eq (aref it 1) ?\\))
                        (char-to-string (aref it 0))
                      it)
                    (my-split-string s "[^\\]\\\\\\|\\`\\\\" nil t))))

    (el-patch-defun company-files (command &optional arg &rest ignored)
      "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings."
      (interactive (list 'interactive))
      (cl-case command
        (interactive (company-begin-backend 'company-files))
        (prefix (company-files--grab-existing-name))
        (candidates
         (el-patch-swap (company-files--complete arg)
                        (if (derived-mode-p 'eshell-mode)
                            (--map (->> it
                                        (eshell-quote-argument)
                                        (s-replace "\\~" "~"))
                                   (company-files--complete
                                    (company-files--unquote-string arg))))))
        (location (cons (dired-noselect
                         (file-name-directory (directory-file-name arg))) 1))
        (post-completion (company-files--post-completion arg))
        (sorted t)
        (no-cache t)))))

(add-hook 'eshell-mode-hook
          (defun eshell-initialize-autocompletion ()
            (company-mode +1)
            (setq company-backends (delq 'company-tabnine company-backends))
            (setq company-backends (delq 'company-native-complete company-backends))
            (add-to-list 'company-backends 'company-files 'append)))

(use-package! native-complete
  :after shell
  :config
  (native-complete-setup-bash)
  (use-package! company-native-complete
    :hook (shell-mode . (lambda () (push 'company-native-complete company-backends)))))

(when (featurep! :term shell)
  (map! [remap +term/toggle] #'+shell/toggle
        [remap +term/here] #'+shell/here
        "M-`" #'+eshell/toggle))

(use-package! comint-intercept
  :hook ((shell-mode . comint-intercept-mode))
  :config
  (setq comint-intercept-prompt-regexp shell-prompt-pattern)
  (setq comint-intercept-term-commands
        '("top" "less" "vim")))

(setq shell-file-name "/run/current-system/sw/bin/bash")

(use-package! pdf-tools
  :defer-incrementally t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq pdf-misc-print-program-args '("-o" "media=letter"))
  (setq pdf-misc-print-program "/run/current-system/sw/bin/lpr")
  (delq 'pdf-misc-size-indication-minor-mode pdf-tools-enabled-modes)
  (delq 'pdf-misc-context-menu-minor-mode pdf-tools-enabled-modes)
  (delq 'pdf-misc-menu-bar-minor-mode pdf-tools-enabled-modes)

  (when (featurep! :editor evil)
    (map! :map pdf-view-mode-map
          :n "J" #'pdf-view-next-page
          :n "K" #'pdf-view-previous-page)))

(after! pdf-view
  (add-hook 'pdf-view-mode-hook #'pdf-view-auto-slice-minor-mode))

(after! pdf-tools
  (defun pdf-view--rotate (&optional counterclockwise-p page-p)
    "Rotate PDF 90 degrees.  Requires pdftk to work.\n
Clockwise rotation is the default; set COUNTERCLOCKWISE-P to
non-nil for the other direction.  Rotate the whole document by
default; set PAGE-P to non-nil to rotate only the current page.
\nWARNING: overwrites the original file, so be careful!"
    ;; error out when pdftk is not installed
    (if (null (executable-find "pdftk"))
        (error "Rotation requires pdftk")
      ;; only rotate in pdf-view-mode
      (when (eq major-mode 'pdf-view-mode)
        (let* ((rotate (if counterclockwise-p "left" "right"))
               (file   (format "\"%s\"" (pdf-view-buffer-file-name)))
               (page   (pdf-view-current-page))
               (pages  (cond ((not page-p) ; whole doc?
                              (format "1-end%s" rotate))
                             ((= page 1) ; first page?
                              (format "%d%s %d-end"
                                      page rotate (1+ page)))
                             ((= page (pdf-info-number-of-pages)) ; last page?
                              (format "1-%d %d%s"
                                      (1- page) page rotate))
                             (t         ; interior page?
                              (format "1-%d %d%s %d-end"
                                      (1- page) page rotate (1+ page))))))
          ;; empty string if it worked
          (if (string= "" (shell-command-to-string
                           (format (concat "pdftk %s cat %s "
                                           "output %s.NEW "
                                           "&& mv %s.NEW %s")
                                   file pages file file file)))
              (pdf-view-revert-buffer nil t)
            (error "Rotation error!"))))))

  (defun pdf-view-rotate-clockwise (&optional arg)
    "Rotate PDF page 90 degrees clockwise.  With prefix ARG, rotate
entire document."
    (interactive "P")
    (pdf-view--rotate nil (not arg)))

  (defun pdf-view-rotate-counterclockwise (&optional arg)
    "Rotate PDF page 90 degrees counterclockwise.  With prefix ARG,
rotate entire document."
    (interactive "P")
    (pdf-view--rotate :counterclockwise (not arg)))

  (map! :map pdf-view-mode-map
        :n "r" #'pdf-view-rotate-clockwise
        :n "R" #'pdf-view-rotate-counterclockwise))

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
