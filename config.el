;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;;; preface
;;;; essential bindings
(map! "C-M-s-b" #'bookmark-jump)
(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit)
(map! "C-M-;" #'comment-or-uncomment-region
      "s-u" #'previous-buffer
      "s-i" #'next-buffer
      ;; Map movement keys to home row
      ;; TODO Remap word-movement and symbol-movement keys?
      (:when (display-graphic-p)
        "<return>" #'newline-and-indent
        "<tab>" #'indent-for-tab-command
        "C-m" #'backward-char
        "C-i" #'forward-char
        "C-." #'end-of-line
        "C-," #'beginning-of-line
        "C-e" #'previous-line
        "C->" #'end-of-buffer
        "C-<" #'beginning-of-buffer
        "C-{" #'backward-paragraph
        "C-}" #'forward-paragraph
        (:map button-map
          "RET" nil
          "<return>" #'push-button)
        (:map minibuffer-local-map
          "<return>" #'exit-minibuffer
          "RET" nil)
        (:map minibuffer-local-ns-map
          "TAB" nil
          "<tab>" #'exit-minibuffer)
        (:map read-expression-map
          "TAB" nil
          "<tab>" #'completion-at-point)
        (:map completion-in-region-mode-map
          "TAB" nil
          "<tab>" #'completion-at-point)
        (:map minibuffer-local-shell-command-map
          "TAB" nil
          "<tab>" #'completion-at-point)
        (:after dired
          "RET" nil
          "<return>" #'dired-find-file)
        (:after lispy
          (:map lispy-mode-map
            "<return>" #'lispy-newline-and-indent-plain
            "<tab>" #'lispy-tab
            "C-m" #'backward-char
            "TAB" nil
            "C-." #'lispy-move-end-of-line
            "C-," #'lispy-move-beginning-of-line
            "C-e" #'previous-line))
        (:after company-coq
          (:map company-coq-map
            "RET" nil
            "<return>" #'company-coq-maybe-exit-snippet
            "C-m" #'backward-char))
        (:after outshine
          (:map outshine-mode-map
            "<tab>" #'outshine-kbd-TAB
            "TAB" nil
            ))
        (:after helpful
          (:map helpful-mode-map
            "<return>" #'helpful-visit-reference
            "<tab>" #'forward-button
            "C-m" #'backward-char
            "TAB" nil))
        (:after org
          (:map org-mode-map
            "<return>" #'org-return
            "TAB" nil
            "C-." #'org-end-of-line
            "C-," #'org-beginning-of-line
            "C-e" nil
            "RET" nil))
        (:after ivy
          (:map ivy-minibuffer-map
            "C-m" #'backward-char
            "C-i" #'ivy-forward-char
            "<tab>" #'ivy-alt-done
            "<return>" #'ivy-done)))
      (:when (and (featurep! :term eshell)
                  (featurep! :term vterm))
        "M-`" #'+vterm/toggle
        (:map vterm-mode-mpa
          "M-`" #'+vterm/toggle)))
;;;; delete by moving to trash
(setq delete-by-moving-to-trash t)
;;;; speed up garbage collection at the cost of increased memory usage.
(setq inhibit-compacting-font-caches t)
;;;; use emacs-lisp-mode for scratch buffers
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)
;;;; other font performance tweaks
(after! font-utils
  (setq font-utils-use-memory-cache t))
;;;; turn off transient-mark-mode
(unless (featurep 'evil)
  (transient-mark-mode -1))
;;;; setup my favorite fonts
(setq doom-font (font-spec :family "Iosevka" :size 16)
      ;; doom-variable-pitch-font (font-spec :family "Fira Sans" :size 17)
      doom-unicode-font (font-spec :family "Dejavu Sans Mono" :size 19)
      doom-serif-font (font-spec :family "Dejavu Serif" :size 19))
;; doom-big-font (font-spec :family "sf mono" :size 18)
;; doom-font (font-spec :family "sf mono" :size 14)
;; doom-unicode-font (font-spec :family "sarasa mono sc" :size 14)
;; doom-variable-pitch-font (font-spec :family "sf compact display" :size 14))
;;;; Set default printer
(after! lpr (setq printer-name "Brother_HL-L2320D_series"))
(after! ps-print (setq ps-printer-name "Brother_HL-L2320D_series"))
;;;; disable line-number
(after! display-line-numbers (setq display-line-numbers-type 'relative))
;;;; this is for smoother scrolling.
(setq mouse-wheel-scroll-amount
      '(1 ((shift) . 2) ((control) . 2))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse nil
      mouse-scroll-min-lines 0)
;;;; do not ask me to re-enter passwords
(setq password-cache-expiry most-positive-fixnum)
;;;; my doom-private-dir has many files.
(setq doom-projectile-cache-limit 75000)
;;;; add startpage as search provider
(when (boundp '+lookup-provider-url-alist)
  (let ((startpage-entry
         (cons "startpage"
               "https://www.startpage.com/do/search?query=%s&?prf=7de10a290cc3cee4fa552d4b43dc3f48")))
    (setq +lookup-provider-url-alist (assoc-delete-all "google" +lookup-provider-url-alist))
    (add-to-list '+lookup-provider-url-alist startpage-entry)))
;;;; turn off shift selection
(setq shift-select-mode nil)
;;;; preview minibuffer defaults
(defvar mcfly-commands
  '(query-replace-regexp
    query-replace
    flush-lines
    keep-lines))

(defvar mcfly-back-commands
  '(self-insert-command
    ivy-yank-char
    ivy-yank-word
    ivy-yank-symbol))

(defun mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command mcfly-back-commands)
         (delete-region (point)
                        (point-max)))))

(defun mcfly-time-travel ()
  (when (memq this-command mcfly-commands)
    (let* ((kbd (kbd "M-n"))
           (cmd (key-binding kbd))
           (future (and cmd
                        (with-temp-buffer
                          (when (ignore-errors
                                  (call-interactively cmd) t)
                            (buffer-string))))))
      (when future
        (save-excursion
          (insert (propertize future 'face 'shadow)))
        (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)))))

;; setup code
(add-hook 'minibuffer-setup-hook #'mcfly-time-travel)

(with-eval-after-load 'ivy
  (push (cons 'swiper 'mcfly-swiper)
        ivy-hooks-alist)
  (defun mcfly-swiper ()
    (let ((sym (with-ivy-window
                 (thing-at-point 'symbol))))
      (when sym
        (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)
        (save-excursion
          (insert (propertize sym 'face 'shadow)))))))
;;; misc non-interactive functions
;;; doom-modeline
(when (featurep! :ui modeline)
  (setq doom-modeline-github t
        doom-modeline-icon nil)
  (add-transient-hook! 'after-change-major-mode-hook
    (display-time-mode)))
;;; modeline
;; (unless (fboundp 'file-local-name)
;;   (defun file-local-name (file)
;;     "return the local name component of file."
;;     (or (file-remote-p file 'localname) file)))

;; ;; (setq mode-line-position
;; ;;       '((line-number-mode ("(%l" (column-number-mode ",%c")))
;; ;;         (-4 ":%p" ) (")")))
;; (setq mode-line-position
;;       '((column-number-mode ("(%c"))
;;         (-4 ":%p") (")")))

;; (defun modeline-project-root ()
;;   "get the path to the root of your project.
;; return `default-directory' if no project was found."
;;   (file-local-name
;;    (or
;;     (when (featurep 'projectile)
;;       (ignore-errors (projectile-project-root)))
;;     default-directory)))

;; (defun truncate-relative-path (path)
;;   "return the truncate of relative path."
;;   (save-match-data
;;     (let ((pos 0) matches)
;;       (setq path (concat "/" path))
;;       (while (string-match "\\(\/\\.?.\\)" path pos)
;;         (setq matches (concat matches (match-string 0 path))
;;               pos (match-end 0)))
;;       (concat matches "/"))))

;; (defun modeline-buffer-file-name ()
;;   "propertized variable `buffer-file-name'."
;;   (let* ((buffer-file-truename (file-local-name (or (buffer-file-name (buffer-base-buffer)) "")))
;;          (project-root (modeline-project-root)))
;;     (concat
;;      ;; project
;;      (propertize
;;       (concat (file-name-nondirectory (directory-file-name project-root)) "/")
;;       'face '(:inherit font-lock-string-face :weight bold))
;;      ;; relative path
;;      (propertize
;;       (when-let (relative-path (file-relative-name
;;                                 (or (file-name-directory buffer-file-truename) "./")
;;                                 project-root))
;;         (if (string= relative-path "./") ""
;;           (substring (truncate-relative-path relative-path) 1)))
;;       'face 'font-lock-comment-face)
;;      ;; file name
;;      (propertize (file-name-nondirectory buffer-file-truename)
;;                  'face 'mode-line-buffer-id))))

;; (defvar-local modeline-buffer-info nil)
;; (defvar mode-line-buffer-info
;;   '(:propertize
;;     (:eval (or modeline-buffer-info
;;                (setq modeline-buffer-info
;;                      (if buffer-file-name
;;                          (modeline-buffer-file-name)
;;                        (propertize "%b" 'face '(:weight bold))))))))
;; (put 'mode-line-buffer-info 'risky-local-variable t)

;; (defsubst modeline-column (pos)
;;   "get the column of the position `pos'."
;;   (save-excursion (goto-char pos)
;;                   (current-column)))

;; (defun selection-info ()
;;   "information about the current selection."
;;   (when mark-active
;;     (cl-destructuring-bind (beg . end)
;;         (cons (region-beginning) (region-end))
;;       (propertize
;;        (let ((lines (count-lines beg (min end (point-max)))))
;;          (concat (cond ((bound-and-true-p rectangle-mark-mode)
;;                         (let ((cols (abs (- (modeline-column end)
;;                                             (modeline-column beg)))))
;;                           (format " (%dx%d)" lines cols)))
;;                        ((> lines 1)
;;                         (format " (%d,%d)" lines (- end beg)))
;;                        ((format " (%d,%d)" 0 (- end beg))))))
;;        'face 'font-lock-warning-face))))

;; (column-number-mode 1)
;; (setq display-time-24hr-format t
;;       display-time-day-and-date t
;;       display-time-default-load-average nil
;;       display-time-load-average-threshold 0
;;       display-time-mail-string ""
;;       display-time-mail-icon nil)
;; ;; (display-time)

;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 ;; mode-line-frame-identification -- this is for text-mode emacs only
;;                 " "
;;                 mode-line-buffer-info
;;                 ;; mode-line-buffer-identification
;;                 " "
;;                 mode-line-position
;;                 ;; (:eval (selection-info))
;;                 (vc-mode vc-mode)
;;                 " "
;;                 ;; mode-line-modes
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))
;;; exwm
;;;; keybindings
(when (display-graphic-p)
  (add-transient-hook! 'post-command-hook
    (dolist (k '(?\s-0 ?\C-x
                       ?\C-g ?\M-\;
                       ?\s-u ?\s-i
                       ;; f2 f3 f4
                       ?\s-o ?\s-W ?\s-y ?\s-e ?\s-w ?\s-i ?\s-r ?\s-f
                       ?\s-d ?\s-s ?\s-q ?\s-a ?\C-\` ?\C-~ ?\s-t ?\C-\\
                       ?\M-0 ?\M-1 ?\M-2 ?\M-3 ?\M-4 ?\M-5 ?\M-6 ?\M-7 ?\M-8 ?\M-9
                       XF86AudioLowerVolume
                       XF86AudioRaiseVolume
                       XF86AudioPlay
                       XF86AudioStop
                       XF86AudioMute
                       XF86AudioPrev
                       XF86AudioNext))
      (pushnew k exwm-input-prefix-keys))
    (delq ?\C-f exwm-input-prefix-keys)

    (exwm-input-set-key
     (kbd "s-q")
     (lambda! (call-process-shell-command "taskset 0x6 qutebrowser" nil 0)))
    (exwm-input-set-key
     (kbd "s-Q")
     (lambda! (call-process-shell-command "taskset 0x6 firefox" nil 0)))
    (exwm-input-set-key (kbd "M-;") #'eval-expression)
    (exwm-input-set-key (kbd "s-x") #'+tabbar/close-tab-or-window)
    (when (featurep! :editor evil)
      (after! evil
        (exwm-input-set-key (kbd "s-f") #'evil-window-right)
        (exwm-input-set-key (kbd "s-d") #'evil-window-up)
        (exwm-input-set-key (kbd "s-s") #'evil-window-down)
        (exwm-input-set-key (kbd "s-a") #'evil-window-left)
        (exwm-input-set-key (kbd "s-C-f") #'+evil/window-move-right)
        (exwm-input-set-key (kbd "s-C-d") #'+evil/window-move-up)
        (exwm-input-set-key (kbd "s-C-s") #'+evil/window-move-down)
        (exwm-input-set-key (kbd "s-C-a") #'+evil/window-move-left)))
    (after! popup
      (exwm-input-set-key (kbd "C-`") #'+popup/toggle)
      (exwm-input-set-key (kbd "C-~") #'+popup/raise))
    (after! emms
      ;; (setq emms-volume-change-function #'emms-volume-amixer-change)
      ;; (setq emms-volume-mode-timeout 0)
      ;; (setq emms-volume-change-amount 2)
      ;; (map! "<XF86AudioRaiseVolume>" #'emms-volume-mode-plus
      ;;       "<XF86AudioLowerVolume>" #'emms-volume-mode-minus)
      (exwm-input-set-key (kbd "<XF86AudioPlay>") #'emms-pause)
      (exwm-input-set-key (kbd "s-t") #'emms-smart-browse)
      (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'emms-volume-mode-plus)
      (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'emms-volume-mode-minus))))
;;;; core
(when (display-graphic-p)
  (require 'exwm-workspace)
  (require 'exwm-randr)
  (require 'exwm-xim)
  (exwm-xim-enable)
  (require 'exwm)
  (require 'exwm-systemtray)
  (add-transient-hook! exwm-init-hook
    (start-process "steam" nil "steam")
    ;; (start-process "wowhead client" "wowhead" "wine" (expand-file-name "~/win64/drive_c/Program Files (x86)/Wowhead_Client.exe"))
    )

  (exwm-systemtray-enable)
  (exwm-randr-enable)
  (exwm-enable)

  (defun exwm-rename-buffer ()
    (when exwm-title
      (exwm-workspace-rename-buffer
       (string-remove-suffix " - qutebrowser" exwm-title))))

  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)
  (add-hook 'exwm-update-title-hook #'exwm-rename-buffer))
;;;; Persp compatibility
(when (featurep! :ui workspaces)
  ;; FIXME When NET_WM_NAME is changed and EXWM runs exwm--update-utf8-title,
  ;; persp-mode does not update the buffer's name in its persp objects.
  (defun exwm--update-utf8-title-advice (oldfun id &optional force)
    "Only update the title when the buffer is visible."
    (when (doom-visible-buffer-p (exwm--id->buffer id))
      (funcall oldfun id force)))
  (advice-add #'exwm--update-utf8-title :around #'exwm--update-utf8-title-advice))
;;;; mouse follows focus
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
    (advice-add #'exwm-mff-hook :around #'exwm-mff-hook-advice)))
;;;; next browser
(defun exwm-next-browser ()
  (when (string-equal exwm-class-name "next")
    (exwm-input-release-keyboard)))
(add-hook 'exwm-update-class-hook #'exwm-next-browser)
;;;; nvidia
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
;;; org
;;;; calfw
;; for some reason 'evil-set-initial-state' does not work here.
(when (and (featurep! :editor evil +everywhere)
           (featurep! :app calendar))
  (after! calfw
    (add-hook 'cfw:details-mode-hook #'evil-emacs-state)))
;;;;;; keybindings
(when (and (featurep! :editor evil)
           (featurep! :app calendar))
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "=calendar") . (nil . "Calendar"))))
  (map! :leader
        (:prefix "a"
          "c" #'=calendar)))
;;;; disable org-bullets
(remove-hook 'org-mode-hook #'org-bullets-mode)
;;;; load secrets file
(load! ".secret")
;;;; pomodoro
(after! org-pomodoro
  (setq org-pomodoro-manual-break t))
;;;; secretaria
(use-package! secretaria
  :defer-incrementally t
  :init
  (after! org
    (run-with-idle-timer
     1 nil
     (lambda (&rest _) (require 'secretaria))))
  :config
  ;; use this for getting a reminder every 30 minutes of those tasks scheduled
  ;; for today and which have no time of day defined.
  (secretaria-unknown-time-always-remind-me))
;;;; exclude agenda files from recentf
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
  (push "\\.?ido\\.last$" recentf-exclude))
;;;; turn off display-line-numbers in org-mode
(add-hook 'org-mode-hook #'doom-disable-line-numbers-h)
;;; howm
(use-package! howm
  :defer-incrementally t
  :bind (("C-c , ," . howm-menu)
         ("C-c , ." . howm-find-today)
         ("C-c , :" . howm-find-yesterday)
         ("C-c , a" . howm-list-around)
         ("C-c , c" . howm-create-here)
         ("C-c , d" . howm-dup)
         ("C-c , h" . howm-first-memo)
         ("C-c , i" . howm-create-interactively)
         ("C-c , k" . howm-keyword-to-kill-ring)
         ("C-c , l" . howm-last-memo)
         ("C-c , m" . howm-open-named-file)
         ("C-c , n" . howm-next-memo)
         ("C-c , p" . howm-previous-memo)
         ("C-c , q" . howm-kill-all)
         ("C-c , SPC" . howm-toggle-buffer)
         ("C-c , t" . howm-insert-dtime)
         ("C-c , a" . howm-list-all)
         ("C-c , b" . howm-list-buffers)
         ("C-c , c" . howm-create)
         ("C-c , d" . howm-insert-date)
         ("C-c , e" . howm-remember)
         ("C-c , g" . howm-list-grep)
         ("C-c , h" . howm-history)
         ("C-c , i" . howm-insert-keyword)
         ("C-c , l" . howm-list-recent)
         ("C-c , m" . howm-list-migemo)
         ("C-c , n" . action-lock-goto-next-link)
         ("C-c , o" . howm-occur)
         ("C-c , p" . action-lock-goto-previous-link)
         ("C-c , r" . howm-refresh)
         ("C-c , s" . howm-list-grep-fixed)
         ("C-c , t" . howm-list-todo)
         ("C-c , w" . howm-random-walk)
         ("C-c , x" . howm-list-mark-ring)
         ("C-c , y" . howm-list-schedule))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '(("\\`C-c ,\\'" . nil) . (nil . "howm"))))
  (add-hook! 'org-mode-hook
    (map! :map org-mode-map
          "C-c ," nil))
  (add-to-list 'auto-mode-alist '("\\.howm$'" . org-mode))
  :config
  (after! org
    (map! :map howm-mode-map
          :ie "<C-return>" #'+org/insert-item-below))
  (setq
   ;; howm-view-use-grep t
   ;; howm-view-grep-command "rg"
   ;; howm-view-grep-option "-nh --no-heading --color never"
   ;; howm-view-grep-extended-option nil
   ;; howm-view-grep-fixed-option "-f"
   ;; howm-view-grep-expr-option nil
   ;; howm-view-grep-file-stdin-option nil
   howm-menu-file "0000-00-00-000000.txt"
   ;; howm-file-name-format "%y/%m/%y-%m-%d-%h%m%s.howm"

   howm-view-title-header "*"
   howm-menu-schedule-days 14
   howm-menu-expiry-hours 1
   howm-keyword-case-fold-search t
   howm-template "\* %title%cursor\n%date %file\n\n"
   howm-view-title-regexp "^\*\\( +\\(.*\\)\\|\\)$"
   howm-view-title-skip-regexp "\\(^\\(\*\\)? *$\\)\\|\\(^\\[[-: 0-9]+\\]\\)"
   howm-view-title-regexp-grep "^\* +"
   )
  (defun stringify-first-argument-if-nil (oldfun &rest args)
    (when (and (listp args)
               (null (car args)))
      (setf (car args) ""))
    (apply oldfun args))
  (defun fix-stringp-nil-error-advice (oldfun &rest args)
    (advice-add #'search-forward :around #'stringify-first-argument-if-nil)
    (advice-add #'search-backward :around #'stringify-first-argument-if-nil)
    (advice-add #'howm-keyword-aliases :around #'stringify-first-argument-if-nil)
    (apply oldfun args)
    (advice-remove #'search-forward #'stringify-first-argument-if-nil)
    (advice-remove #'search-backward #'stringify-first-argument-if-nil)
    (advice-remove #'howm-keyword-aliases #'stringify-first-argument-if-nil))
  (advice-add #'howm-random-walk-text :around #'fix-stringp-nil-error-advice)
  (when (featurep! :editor evil)
    (add-hook! 'howm-mode-hook
      (if (memq major-mode '(text-mode org-mode))
          (progn
            (if (and (or (eobp)
                         (>= 3 (count-lines (point-min) (point-max))))
                     (eolp))
                (evil-insert-state)
              (evil-normal-state)))
        (evil-emacs-state)))
    (add-hook 'howm-view-summary-mode-hook #'evil-emacs-state))
  (add-hook! 'howm-mode-hook
    (setq-local org-complex-heading-regexp
                "^\\(\\*+\\)\\(?: +\\(abrt\\|done\\|hold\\|next\\|proj\\|todo\\|wait\\|\\[\\(?:[ ?x-]]\\)\\)\\)?\\(?: +\\(\\[#.\\]\\)\\)?\\(?: +\\(.*?\\)\\)??\\(?:[ 	]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[ 	]*$")))
;;; navigation
;;;; isearch
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
;;;; avy
(use-package! avy
  :bind (("C-M-s-s" . avy-goto-char-timer))
  :config
  (avy-setup-default)
  (setq avy-all-windows t
        avy-timeout-seconds 0.3)
  ;; (setq avy-style 'pre)
  (setq avy-keys
        '(?q ?a ?r ?s ?t ?i
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
;;;; ace-window
(after! ace-window
  (setq aw-background nil
        aw-ignore-current nil))
;;;; deadgrep
(use-package! deadgrep
  :defer-incrementally t
  :commands (deadgrep)
  :bind ((:map doom-leader-map
           ("/ d" . deadgrep)))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "deadgrep") . (nil . "Deadgrep")))))
;;;; anzu
(use-package! anzu
  :defer-incrementally t
  :commands (anzu-query-replace
             anzu-query-replace-regexp)
  :init
  ;; (defun anzu-enable-advice (&rest _)
  ;; (global-anzu-mode 1))
  ;; (advice-add #'isearch-forward :before #'anzu-enable-advice)
  ;; (advice-add #'isearch-backward :before #'anzu-enable-advice)
  ;; (advice-add #'isearch-forward-regexp :before #'anzu-enable-advice)
  ;; (advice-add #'isearch-backward-regexp :before #'anzu-enable-advice)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         (:map isearch-mode-map
           ([remap isearch-query-replace] . anzu-isearch-query-replace)
           ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))))
;;; ui
;;;; theme
(setq doom-themes-enable-bold nil)
;;;; visual scrolling
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

(global-set-key (kbd "C-M-v")
                'my-View-scroll-half-page-forward-other-window)
(global-set-key (kbd "C-M-S-v")
                'my-View-scroll-half-page-backward-other-window)

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
     `(keymap (?v . ,real-this-command)))
    (save-excursion
      (goto-line linen)
      (pulse-momentary-highlight-one-line (point) 'highlight))
    (sit-for 0.1)
    (apply f args)))

(defun my-indicate-scroll-forward (f &rest args)
  (my-indicate-scroll (1- (window-end)) f args))

(defun my-indicate-scroll-backward (f &rest args)
  (my-indicate-scroll (window-start) f args))
;;;; frog-menu
(use-package! frog-menu
  :commands frog-menu-read
  :init
  (after! flyspell-correct
    (defun frog-menu-flyspell-correct (candidates word)
      "Run `frog-menu-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return selected word to use as a replacement or a tuple
of (command . word) to be used by `flyspell-do-correct'."
      (let* ((corrects (if flyspell-sort-corrections
                           (sort candidates 'string<)
                         candidates))
             (actions `(("C-s" "Save word"         (save    . ,word))
                        ("C-a" "Accept (session)"  (session . ,word))
                        ("C-b" "Accept (buffer)"   (buffer  . ,word))
                        ("C-c" "Skip"              (skip    . ,word))))
             (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
                                                       ispell-dictionary
                                                       "default")))
             (res      (frog-menu-read prompt corrects actions)))
        (unless res
          (error "Quit"))
        res))

    (setq flyspell-correct-interface #'frog-menu-flyspell-correct)))
;;;; Workspaces
(when (featurep! :ui workspaces)
  (map! (:map minibuffer-local-map
          "M-1" #'ignore
          "M-2" #'ignore
          "M-3" #'ignore
          "M-4" #'ignore
          "M-5" #'ignore
          "M-6" #'ignore
          "M-7" #'ignore
          "M-8" #'ignore
          "M-9" #'ignore
          "M-0" #'ignore)
        (:when (featurep! :completion ivy)
          (:map ivy-minibuffer-map
            "M-1" #'ignore
            "M-2" #'ignore
            "M-3" #'ignore
            "M-4" #'ignore
            "M-5" #'ignore
            "M-6" #'ignore
            "M-7" #'ignore
            "M-8" #'ignore
            "M-9" #'ignore
            "M-0" #'ignore))))
;;;; popups
(when (featurep! :ui popup)
  (set-popup-rules!
    '(("^\\*howm" :ignore t)
      ("^\\*eww" :size 0.6 :side right)
      ("^\\*deadgrep" :size 0.6 :side right)
      ("^\\*wiki-summary" :size 0.35)
      ("^\\*Synonyms List" :size 0.35)
      ("^\\*leetcode" :ignore t)
      ("^\\*assembly\\*$" :ignore t)
      ("^\\*dungeon\\*$" :ignore t)
      ("^\\*tldr\\*$" :ignore t)))
  (set-popup-rules! '(("^\\*helpful" :size 0.35)
                      ("^\\*ibuffer\\*$" :size 0.35)
                      ("^\\*info.*" :size 80 :side right)
                      ("^\\*man.*" :size 80 :side right)
                      ("^\\*customize" :actions display-buffer)
                      ("^\\*edit-indirect" :size 0.6)
                      ("^\\*yasnippet tables\\*$" :size 0.35)
                      ("^\\*grep\\*$" :size 0.35)
                      ("^\\*anaconda\\*$" :size 0.35)
                      ("^\\*helm kill ring\\*$" :size 0.35)
                      ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35))))
;;;; fill column indicator
(advice-add #'hl-fill-column-mode
            :around
            (lambda (oldfun &rest args)
              (if (eq major-mode 'mhtml-mode)
                  (message "hl-fill-column mode is currently disabled in mhtml-mode")
                (apply oldfun args))))
;;;; annotations
(use-package! annot
  :commands (annot-edit/add annot-remove annot-load-annotations)
  :load-path "moose/vendor"
  ;; :defer-incrementally t
  :bind (:map doom-leader-map
          ("ua" . annot-edit/add)
          ("ux" . annot-remove)
          ("uA" . annot-add-image))
  :init
  (require 'annot)
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
;;; objed
(when (featurep! :editor objed)
  (remove-hook 'prog-mode-hook #'hl-line-mode)
  (remove-hook 'text-mode-hook #'hl-line-mode)

  (defun my-objed-electric-event (beg end &optional event)
    (interactive "r")
    (require 'elec-pair)
    (let ((electric-pair-mode t)
          (last-command-event (or event last-command-event))
          (epos nil)
          ;; make sure to go to beginning
          (rbeg (if (> beg end) end beg))
          (rend (if (> beg end) beg end))
          (pair (pcase event
                  (?' '("'" . "'"))
                  (?\) '("(" . ")"))
                  (?\( '("( " . " )"))
                  (?\] '("[" . "]"))
                  (?\[ '("[ " . " ]"))
                  (?\} '("{" . "}"))
                  (?\{ '("{ " . " }"))
                  (?\" '("\"" . "\""))
                  (event (cons (char-to-string event)
                               (char-to-string event))))))
      (save-mark-and-excursion
        ;; skip ws optionally?
        (push-mark (objed--skip-backward rend 'ws) t t)
        (goto-char rbeg)
        (objed --skip-ws)
        (insert (car pair))
        (goto-char (1+ rend))
        (objed--skip-ws 'backward)
        (insert (cdr pair))
        ;; todo: additional expansion/insertion
        (setq epos (point))
        (goto-char epos))))

  (advice-add #'objed-electric-event :override #'my-objed-electric-event)

  (use-package! beacon
    :init
    (add-hook! objed-mode
      (setq beacon-blink-when-window-scrolls nil)
      (setq beacon-blink-when-focused t)
      (beacon-mode 1)
      (add-hook 'beacon-before-blink-hook
                (lambda ()
                  (run-at-time 0.1 nil 'objed-activate)))))
  (after! objed
    ;; (add-to-list 'objed-cmd-alist '(View-scroll-half-page-forward . char))
    ;; (add-to-list 'objed-cmd-alist '(View-scroll-half-page-backward . char))
    ;; (dolist (command '(sp-beginning-of-sexp
    ;;                    sp-end-of-sexp
    ;;                    sp-backward-sexp
    ;;                    sp-backward-up-sexp
    ;;                    sp-up-sexp
    ;;                    sp-down-sexp
    ;;                     nsp-backward-down-sexp
    ;;                    ;; sp-split-sexp
    ;;                    ;; sp-kill-sexp
    ;;                    ;; sp-transpose-sexp
    ;;                    ;; sp-splice-sexp
    ;;                    ;; sp-forward-slurp-sexp
    ;;                    ;; sp-forward-barf-sexp
    ;;                    ;; sp-backward-slurp-sexp
    ;;                    ;; sp-backward-barf-sexp
    ;;                    ))
    ;;   (add-to-list 'objed-cmd-alist (cons command 'bracket)))
    ;; (objed--install-advices objed-cmd-alist t)

    (map! (:map objed-mode-map
            "M-o" #'objed-activate-object
            )
          ;; (:map objed-map
          ;;   "C-1" #'+workspace/switch-to-0
          ;;   "C-2" #'+workspace/switch-to-1
          ;;   "C-3" #'+workspace/switch-to-2
          ;;   "C-4" #'+workspace/switch-to-3
          ;;   "C-5" #'+workspace/switch-to-4
          ;;   "C-6" #'+workspace/switch-to-5
          ;;   "C-7" #'+workspace/switch-to-6
          ;;   "C-8" #'+workspace/switch-to-7
          ;;   "C-9" #'+workspace/switch-to-8
          ;;   "C-0" #'+workspace/switch-to-9)
          (:map objed-op-map
            "p" #'recentf-open-files
            "l" #'recenter-top-bottom))))
;;; emacs
(unless (featurep! :editor evil)
  (use-package! visible-mark
    :init
    ;; (defface visible-mark-active ;; put this before (require 'visible-mark)
    ;;   '((((type tty) (class mono)))
    ;;     (t (:background "magenta"))) "")

    (add-hook! 'find-file-hook
      (global-visible-mark-mode +1))
    :config
    ;; (setq visible-mark-max 2
    ;;       visible-mark-faces `(visible-mark-face1 visible-mark-face2))
    )
  ;; (use-package! auto-mark
  ;;   :init
  ;;   (add-hook! 'find-file-hook
  ;;     (global-auto-mark-mode +1)))
  )
;;; evil
;;;; keybindings
(when (featurep! :editor evil)
  (map! :n "<backspace>" #'evil-substitute
        :i "C-\\" #'evil-paste-from-register
        :i "C-r" #'isearch-backward
        (:after org
          (:map evil-org-mode-map
            :i "C-h" help-map))
        (:map evil-window-map
          "o" #'other-window
          "c" #'delete-window
          "e" #'evil-window-up)
        (:leader
          (:prefix "/"
            (:after counsel
              :nv "3" #'counsel-outline))
          (:prefix "t"
            :nv "." #'find-file-at-point))))
;;;; evil-mc
(when (and (featurep! :editor multiple-cursors)
           (featurep! :editor evil))
  (after! (:all evil-mc-known-commands
                evil-mc)
    (cl-loop for x in evil-mc-custom-known-commands
             do (add-to-list 'evil-mc-known-commands x))
    (cl-loop for command in '(doom/forward-to-last-non-comment-or-eol
                              doom/backward-to-bol-or-indent
                              coqie-insert-asterisk
                              coqie-insert-minus
                              coqie-insert-comma
                              coqie-delete-backward-char
                              coqie-insert-period
                              coqie-insert-semicolon
                              coqie-insert-colon)
             do (setf (alist-get command evil-mc-known-commands)
                      (list '(:default . evil-mc-execute-default-call))))
    (cl-loop for command in '(coqie-self-insert
                              coqie-insert-slash-char
                              coqie-insert-open-paren
                              coqie-insert-plus
                              coqie-insert-equals
                              coqie-insert-space
                              backward-kill-word)
             do (setf (alist-get command evil-mc-known-commands)
                      (list '(:default . evil-mc-execute-default-call-with-count))))
    (defmacro add-override-in-evil-mc (oldfun newfun)
      `(progn (defun ,(intern (concat "evil-mc--"
                                      (symbol-name oldfun)
                                      "->"
                                      (symbol-name newfun)))
                  (f &rest args)
                (if evil-mc-cursor-state
                    (apply ',newfun args)
                  (apply f args)))
              (advice-add ',oldfun
                          :around
                          ',(intern (concat "evil-mc--"
                                            (symbol-name oldfun)
                                            "->"
                                            (symbol-name newfun))))))
    (defmacro remove-override-in-evil-mc (oldfun newfun)
      `(advice-remove ',oldfun
                      ',(intern (concat "evil-mc--"
                                        (symbol-name oldfun)
                                        "->"
                                        (symbol-name newfun)))))
    (add-override-in-evil-mc doom/forward-to-last-non-comment-or-eol end-of-line)
    (when (and (featurep! :editor lispy)
               (featurep! :editor evil))
      (add-hook! 'evil-mc-before-cursors-created
        (lispy-mode 0)
        (lispyville-mode 0))
      (add-hook! 'evil-mc-after-cursors-deleted
        (when (eq major-mode 'emacs-lisp-mode)
          (lispy-mode 1)
          (lispyville-mode 1))))))
;;;; code folding
(use-package! evil-vimish-fold
  :when (featurep! :editor evil)
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold
                                        evil-vimish-fold/delete evil-vimish-fold/delete-all
                                        evil-vimish-fold/create evil-vimish-fold/create-line)
  :init
  (setq vimish-fold-dir (concat doom-cache-dir "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  (evil-define-key* 'motion 'global
                    "xt" #'evil-vimish-fold/create
                    "xt" #'evil-vimish-fold/create-line
                    "xs" #'vimish-fold-delete
                    "xc" #'vimish-fold-delete-all)
  (vimish-fold-global-mode 1))
;;;; evil-snipe
(when (featurep! :editor evil)
  (after! evil-snipe
    (setq evil-snipe-scope 'line
          evil-snipe-spillover-scope 'visible)))
;;;; evil-ex
;; reduce the amount of garbage generated by evil-ex.
(when (featurep! :editor evil)
  (setq evil-ex-hl-update-delay 0.02))
;;;; lispyville
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
;;; lispy
(when (featurep! :editor lispy)
  (customize-set-variable 'lispy-key-theme '(special lispy))
  (after! lispy
    (setq lispy-eval-display-style 'overlay
          lispy-no-permanent-semantic t)
    (map! :map lispy-mode-map-special
          "m" #'special-lispy-left)))
;;; completion
;;;; company
;;;;;; company settings
(when (featurep! :completion company)
  (after! company
    (setq company-idle-delay 0
          company-minimum-prefix-length 1
          company-show-numbers t)
    (defun company-select-by-number (i)
      (company-finish (nth i company-candidates)))
    (map! :map company-active-map
          "C-1" (lambda! (company-select-by-number 0))
          "C-2" (lambda! (company-select-by-number 1))
          "C-3" (lambda! (company-select-by-number 2))
          "C-4" (lambda! (company-select-by-number 3))
          "C-5" (lambda! (company-select-by-number 4))
          "C-6" (lambda! (company-select-by-number 5))
          "C-7" (lambda! (company-select-by-number 6))
          "C-8" (lambda! (company-select-by-number 7))
          "C-9" (lambda! (company-select-by-number 8))
          "C-0" (lambda! (company-select-by-number 9)))
    (define-key! company-active-map
      "RET" nil
      [return] nil
      "TAB" nil
      [tab] nil
      [backtab] nil)))
;;;;;; enable fuzzy completion in emacs lisp mode.
;; (use-package! company-flx
;;   :hook (emacs-lisp-mode . company-flx-mode)
;;   :config
;;   (setq company-flx-limit 150)
;;   (defun company-prescient-disable-in-elisp-mode-advice (oldfun &rest args)
;;     (if (eq major-mode 'emacs-lisp-mode)
;;         (apply #'identity args)
;;       (apply oldfun args)))
;;   (defun company-flx-transformer-enable-only-in-elisp-mode-advice (oldfun &rest args)
;;     (if (eq major-mode 'emacs-lisp-mode)
;;         (apply oldfun args)
;;       (apply #'identity args)))
;;   (advice-add #'company-flx-transformer
;;               :around
;;               #'company-flx-transformer-enable-only-in-elisp-mode-advice)
;;   (advice-add #'company-prescient-transformer
;;               :around
;;               #'company-prescient-disable-in-elisp-mode-advice))
;;;; abbrev
(use-package! abbrev
  :hook ((prog-mode . abbrev-mode)
         (text-mode . abbrev-mode))
  :custom
  (abbrev-file-name (concat doom-private-dir "abbrev.el"))
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
        :desc "edit abbrevs" :nmv "a" #'edit-abbrevs))
;;;; ivy
(when (featurep! :completion ivy)
  (setq +ivy-buffer-preview t)
  (after! ivy
    (when (and (featurep 'exwm)
               (featurep! :completion ivy +childframe))
      (after! ivy-posframe
        (setq ivy-posframe-parameters '((parent-frame nil)))))
    ;; (map! :g [remap execute-extended-command] #'counsel-M-x)
    (add-to-list 'ivy-ignore-buffers "^\\*undo propose:")
    ;; keybindings
    (map! :map ivy-minibuffer-map
          "M-s" #'ivy-toggle-fuzzy)
    (after! swiper
      (map! :map swiper-map
            "C-s" #'ivy-next-line
            "C-r" #'ivy-previous-line))))
;;;;;; swiper-isearch
(when (featurep! :completion ivy)
  (use-package! swiper
    :defer-incrementally t
    :commands (swiper-isearch swiper-isearch-backward swiper)
    :bind (("C-S-s" . swiper-isearch)
           ("C-S-r" . swiper-isearch-backward)
           (:map isearch-mode-map
             ("C-S-s" . swiper-from-isearch)
             ("C-S-r" . swiper-from-isearch)))))
;;; email
;;;; smtp-mail
(after! message
  (setq message-send-mail-function #'smtpmail-send-it))
;;;; mu4e
;;;;;; unsorted
(when (featurep! :email mu4e)
  (use-package! mu4e
    :bind (:map doom-leader-map
            ("am" . =mu4e))
    :defer-incrementally t
    :init
    (after! which-key
      (add-to-list 'which-key-replacement-alist
                   '((nil . "=mu4e") . (nil . "Mu4e"))))
    (after! notmuch
      (require 'mu4e))
    :config
    (set-face-attribute 'variable-pitch nil :foreground "#ffffff")
    (setq mu4e-update-interval 300)
    (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
    (add-hook 'mu4e-compose-mode-hook #'flyspell-mode)
    (require 'org-mu4e)
    ;; (setq org-mu4e-link-query-in-headers-mode nil)
    (after! all-the-icons
      (setq all-the-icons-scale-factor 1.0)
      (set-face-background 'mu4e-highlight-face
                           (face-background 'default))
      (set-face-foreground 'mu4e-highlight-face
                           (face-foreground 'all-the-icons-yellow)))))

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
;;;;;; email notifications
(use-package! mu4e-alert
  :after mu4e
  :config
  (defun my-mu4e-alert-mode-line-formatter (mail-count)
    "default formatter used to get the string to be displayed in the mode-line.
mail-count is the count of mails for which the string is to displayed"
    (when (not (zerop mail-count))
      (if (zerop mail-count)
          ""
        (format " [%d] " mail-count))))
  (mu4e-alert-set-default-style 'libnotify)
  (when (featurep! :ui modeline)
    (setq doom-modeline-mu4e t))
  (setq mu4e-alert-modeline-formatter #'my-mu4e-alert-mode-line-formatter
        mu4e-alert-notify-repeated-mails t
        mu4e-alert-email-notification-types '(subjects))
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))
;;;; notmuch
(when (featurep! :email notmuch)
  (use-package! notmuch
    :bind (:map doom-leader-map
            ("an" . =notmuch)
            ("aN" . counsel-notmuch))
    :defer-incrementally t
    :init
    (after! which-key
      (add-to-list 'which-key-replacement-alist
                   '((nil . "=notmuch") . (nil . "Notmuch")))

      (add-to-list 'which-key-replacement-alist
                   '((nil . "counsel-notmuch") . (nil . "Search emails"))))
    :config
    (setq notmuch-init-file "~/.config/notmuch/notmuchrc")
    ;; (defadvice! mu4e-update-mail-and-index-a  (info)
    ;;   "load babel libraries lazily when babel blocks are executed."
    ;;   :after #'mu4e-update-mail-and-index
    ;;   (start-process-shell-command "notmuch-index" "*notmuch-index*" "notmuch --config=~/.config/notmuch/notmuchrc new"))
    (advice-add #'notmuch-bury-or-kill-this-buffer :after
                (lambda (&rest _args)
                  (when (equal (+workspace-current-name)
                               "*mail*")
                    (+workspace/delete (+workspace-current-name)))))))
;;; term
;;;; vterm
(after! vterm
  (setq vterm-shell "/run/current-system/sw/bin/zsh"))
;;; proofs
;;;; proof general
(after! proof-splash
  (setq proof-splash-enable nil))
(after! proof-useropts
  (setq proof-toolbar-enable nil))
;;;;;; disable holes feature
(after! pg-custom
  (setq coq-use-holes nil))
(after! coq-db
  (setq coq-holes-minor-mode nil))
(advice-add #'coq-build-abbrev-table-from-db :override #'ignore)
;;;; coq
(after! company-coq
  (setq company-coq-live-on-the-edge t)
  (setq company-coq-disabled-features '(hello))
  (setq-hook! 'coq-mode-hook tab-width 2))
(after! coq-mode
  (defvar coq-favourites nil)
  (defvar coq-prog-args nil)
  (defvar coq-one-command-per-line nil)
  (setq coq-prog-env
        '("home=~/"
          "path=$path:~/.nix-profile/bin:/run/current-system/sw/bin/"))
  (setq coq-prog-name (expand-file-name "~/hott/hoqtop"))
  (load (concat doom-private-dir "+coq") nil t)
  (defun my-coq-mode-hook ()
    (setq flycheck-coq-executable (expand-file-name "~/hott/hoqtop")
          proof-prog-name-ask nil)
    (after! proof-config
      (setq proof-prog-name (expand-file-name "~/hott/hoqtop"))))
  (add-hook 'coq-mode-hook #'my-coq-mode-hook))
;; (add-hook! 'coq-mode-hook (abbrev-mode -1))
;;; haskell
(after! haskell-mode
  (after! intero
    (setq intero-stack-executable "~/.nix-profile/bin/stack")))
;;; emacs lisp
(after! lispyville
  (map! :map lispyville-mode-map
        :n "M-t" nil))
;; (use-package! auto-compile
;;   :after-call before-save-hook
;;   :config
;;   (auto-compile-on-load-mode 1)
;;   (auto-compile-on-save-mode 1))
;;; common lisp
(after! sly
  (setq sly-lisp-implementations
        '((sbcl ("~/.nix-profile/bin/sbcl")
                :coding-system utf-8-unix))))
;;; undo
(use-package! undo-propose
  :bind (("C-x u" . undo-propose)
         ("C-M-s-u" . undo-propose)))
;;; files
;;;; exclude more boring files from recent files.
(after! recentf
  (add-to-list 'recentf-exclude "^/nix/store/")
  (add-to-list 'recentf-exclude ".+\\.mp3$"))
;;;; dired
(when (featurep! :emacs dired)
  (after! dired
    (setq dired-auto-revert-buffer t)
    (unless (featurep! :emacs dired +ranger)
      (map! :map dired-mode-map
            "e" #'dired-previous-line)))
;;;; sunrise commander, a two-pane version of dired.
  (use-package! sunrise-commander
    :defer-incrementally t
    :commands (sunrise)
    :bind (:map doom-leader-map
            ("t -" . sunrise))
    :init
    (after! which-key
      (add-to-list 'which-key-replacement-alist
                   '((nil . "sunrise") . (nil . "sunrise commander"))))
    :config
    (setq sr-show-file-attributes t
          sr-cursor-follows-mouse nil
          sr-show-hidden-files t)
    (define-key sr-mode-map [mouse-1] nil)
    (define-key sr-mode-map [mouse-movement] nil)))
;;; treemacs
(when (featurep! :ui treemacs)
  (after! treemacs
    (setq treemacs-position 'left)

    (defun treemacs-ignore-boring-files-extensions (file-name absolute-path)
      (ignore absolute-path)
      (string-match-p "\\.elc$" file-name))

    (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-boring-files-extensions)

    (map! :map ctl-x-map
          "9" #'+treemacs/toggle)))
;;; projectile
(after! projectile
  (add-to-list 'projectile-globally-ignored-files "*.~undo-tree~")
  (add-to-list 'projectile-globally-ignored-files "*.latexmkrc")
  (add-to-list 'projectile-globally-ignored-files "*.log")
  (add-to-list 'projectile-globally-ignored-files "*.fls")
  (add-to-list 'projectile-globally-ignored-files "*.aux")
  (add-to-list 'projectile-globally-ignored-files "*.fdb_latexmk")
  (add-to-list 'projectile-globally-ignored-files "*.latexmkrc")
  (add-to-list 'projectile-globally-ignored-files "*.projectile")
  (add-to-list 'projectile-globally-ignored-directories "auto")
  (add-to-list 'projectile-globally-ignored-directories "ltximg")
  (add-to-list 'projectile-globally-ignored-directories "~/.doom.d/dict/")
  (projectile-register-project-type 'latexmk '(".latexmkrc")
                                    :compile "latexmk -pdf -pvc")
  (setq projectile-require-project-root t))
;;; text-mode
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(when (featurep! :app write +langtool)
  (after! langtool
    (load! "pos-tip" (concat doom-private-dir "packages/"))
    (load! "popup-pos-tip" (concat doom-private-dir "packages/"))
    (setq langtool-bin (concat (getenv "languagetool_path") "/bin/languagetool")
          langtool-language-tool-jar
          (concat (getenv "languagetool_path")
                  "/share/languagetool-commandline.jar")
          langtool-language-tool-server-jar
          (concat (getenv "languagetool_path")
                  "/share/languagetool-server.jar")
          langtool-java-bin (concat (getenv "jdk_path") "/bin/java")
          ;; Custom message function
          ;; langtool-autoshow-message-function
          ;; #'langtool--autoshow-detail-popup
          )))
(use-package! fountain-mode
  :defer-incrementally t
  :defer t)
(use-package! lorem-ipsum
  :defer-incrementally t
  :bind (:map doom-leader-map
          ("ull" . lorem-ipsum-insert-list)
          ("ulp" . lorem-ipsum-insert-paragraphs)
          ("uls" . lorem-ipsum-insert-sentences))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '(("SPC u l" . nil) . (nil . "lorem ipsum")))
    (add-to-list 'which-key-replacement-alist
                 '(("SPC u l l" . nil) . (nil . "insert list")))
    (add-to-list 'which-key-replacement-alist
                 '(("SPC u l p" . nil) . (nil . "insert paragraph")))
    (add-to-list 'which-key-replacement-alist
                 '(("SPC u l s" . nil) . (nil . "insert sentence"))))
  :config
  (setq lorem-ipsum-paragraph-separator "\n\n"
        lorem-ipsum-sentence-separator " "))
(after! counsel
  (map!
   (:leader
     (:prefix "u"
       :nv "u" #'counsel-unicode-char))))
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
(use-package! auto-capitalize
  :defer t
  :commands (auto-capitalize-mode
             turn-on-auto-capitalize-mode))
(use-package! wiki-summary
  :defer-incrementally t
  :commands (wiki-summary wiki-summary-insert)
  :bind (:map doom-leader-map
          ("alW" . wiki-summary)))
(when (featurep! :tools flyspell +aspell)
  (after! ispell
    (setq ispell-quietly nil
          ispell-dictionary "en_us"
          ispell-complete-word-dict "~/.doom.d/dict/english-words.txt"))
  (after! flyspell
    (setq flyspell-issue-message-flag t
          flyspell-abbrev-p t)))
;;; prog-mode
;;;; outshine
(use-package! outshine
  :hook ((outline-minor-mode . outshine-mode)))
;;;; show matching lines when parens go off-screen
;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)
;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)

(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
   (defun show-paren--off-screen+ (&rest _args)
     "Display matching line for off-screen paren."
     (when (overlayp ov)
       (delete-overlay ov))
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
                      (setq ov (display-line-overlay+
                                (window-start) msg ))))))
         (blink-matching-open))))))

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
;;;; nix-mode
(after! nix-mode
  (setf (alist-get 'nix-mode +company-backend-alist)
        '((company-nixos-options company-files))))
;;;; cc-mode
(use-package! disaster
  :defer-incrementally t
  :after cc-mode
  :config
  (setq disaster-cflags "-fno-stack-protector -D_FORTIFY_SOURCE=0 -O0"
        disaster-cxxflags "-fno-stack-protector -D_FORTIFY_SOURCE=0 -O0")
  (map! :map c-mode-map
        :nmvie "C-c d" #'disaster
        :localleader
        :desc "Disassemble at point" "d" #'disaster))
;;;; make eldoc print help messages faster.
(after! eldoc
  (setq eldoc-idle-delay 0.3))
;;;; change amx save file location.
(after! amx
  (setq amx-save-file "~/.doom.d/.local/cache/amx-items")
  (advice-remove #'eval-last-sexp #'amx-post-eval-force-update)
  (advice-remove #'eval-expression #'amx-post-eval-force-update))
;;;; reindent upon narrowing and widening.
(use-package! narrow-reindent
  :init
  (defun turn-on-narrow-reindent-mode ()
    (require 'narrow-reindent)
    (narrow-reindent-mode 1))
  (add-hook 'after-change-major-mode-hook #'turn-on-narrow-reindent-mode))
;;;; fontify page-break characters.
(use-package! page-break-lines
  :defer nil
  :config
  (global-page-break-lines-mode 1))
;;;; syntax checking
(after! flycheck
  (setq flycheck-ghc-stack-use-nix t
        flycheck-coq-executable "~/.nix-profile/bin/hoqtop"
        flycheck-haskell-ghc-executable "~/.nix-profile/bin/ghc"
        flycheck-haskell-hlint-executable "~/.nix-profile/bin/hlint"
        flycheck-haskell-stack-ghc-executable "~/.nix-profile/bin/stack"
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.5)
  (load (concat doom-private-dir "+haskell") nil t)
  (add-transient-hook! 'haskell-mode-hook
    (hie-nix/init-haskell-mode)
    (use-package! nix-sandbox
      :after haskell-mode)
    (use-package! flycheck-haskell
      :commands flycheck-haskell-configure
      :config
      (progn
        (add-hook 'haskell-mode-hook 'flycheck-haskell-configure)
        (setq lsp-haskell-process-wrapper-function #'hie-nix//default-nix-wrapper
              flycheck-command-wrapper-function #'hie-nix//default-nix-wrapper
              flycheck-executable-find (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))))))
;;;; go playground
(use-package! go-playground
  ;; :defer-incrementally t
  ;; :bind ((:map doom-leader-map
  ;;          ("ng" . go-playground)))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "go-playground") . (nil . "Go playground"))))
  :config
  (map! :map go-playground-mode-map
        "<f8>" #'go-playground-rm))
;;;; rust playground
(use-package! rust-playground
  ;; :defer-incrementally t
  ;; :bind ((:map doom-leader-map
  ;;          ("nr" . rust-playground)))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "rust-playground") . (nil . "Rust playground"))))
  :config
  (map! :map rust-playground-mode-map
        "<f8>" #'rust-playground-rm)
  (setq rust-playground-basedir "~/rust-playground"))
;;;; python playground
(use-package! py-playground
  ;; :defer-incrementally t
  ;; :bind ((:map doom-leader-map
  ;;          ("np" . py-playground)))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "py-playground") . (nil . "Python playground"))))
  :config
  (map! :map py-playground-mode-map
        "<f8>" #'py-playground-rm
        "s-RET" #'py-playground-rm
        "C-c r" #'py-playground-add-or-modify-tag
        "C-c d" #'py-playground-debug))
;;;; c++ playground
(use-package! cc-playground
  ;; :defer-incrementally t
  :commands cc-playground cc-playground-mode cc-playground-find-snippet cc-playground-leetcode
  ;; :bind ((:map doom-leader-map
  ;;          ("nc" . cc-playground)))
  :init
  (put 'cc-exec 'safe-local-variable #'stringp)
  (put 'cc-flags 'safe-local-variable #'stringp)
  (put 'cc-links 'safe-local-variable #'stringp)
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "CC-playground") . (nil . "C++ playground"))))
  :config
  ;; This definition is required!
  (defun shell-command! (command &optional out-buffer err-buffer)
    (let ((inhibit-message t))
      (shell-command command out-buffer err-buffer)))

  (map! (:map cc-playground-mode-map
          "<f8>" #'cc-playground-rm     ; terminal
          "s-RET" #'cc-playground-rm    ; gui
          "C-c r" #'cc-playground-add-or-modify-tag
          "C-c b" #'cc-playground-bench
          "C-c d" #'cc-playground-debug
          "C-c t" #'cc-playground-debug-test
          "C-c l" #'cc-playground-ivy-add-library-link
          "C-c c" #'cc-playground-change-compiler
          "C-c o" #'cc-playground-switch-optimization-flag
          "C-c f" #'cc-playground-add-compilation-flags)))
;;; debug font lock keywords.
(use-package! font-lock-studio
  :defer-incrementally t
  :commands (font-lock-studio))
;;; amusement
(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("\\`SPC a\\'" . nil) . (nil . "apps"))))
(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("\\`SPC a l\\'" . nil) . (nil . "lookup"))))
;;;; games
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
;;;; quotes
(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("\\`SPC a q\\'" . nil) . (nil . "quotes"))))
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
;;;; eye candy
(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("\\`SPC a e\\'" . nil) . (nil . "eyecandy"))))
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
;; (use-package! landmark
;;   :defer-incrementally t
;;   :commands landmark
;;   :bind (:map doom-leader-map
;;           ("ael" . landmark))
;;   :init
;;   (after! which-key
;;     (add-to-list 'which-key-replacement-alist
;;                  '((nil . "landmark") . (nil . "Landmark"))))
;;   :config
;;   (when (featurep! :editor evil)
;;     (evil-set-initial-state 'landmark-mode 'emacs)))
(use-package! zone
  :defer-incrementally t
  :commands zone
  :bind (:map doom-leader-map
          ("aez" . zone))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "zone") . (nil . "Zone")))))
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
(use-package! disssociate
  :defer-incrementally t
  :commands dissociated-press
  :bind (:map doom-leader-map
          ("aed" . dissociated-press))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "dissociated-press") . (nil . "Dissociated press")))))
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
;;; miscellaneous apps
;;;; disk usage
(use-package! disk-usage
  :commands disk-usage
  :bind (:map doom-leader-map
          ("ad" . disk-usage))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "disk-usage") . (nil . "Disk usage")))))
;;;; speed reading
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
;;;; encode string into QR
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
;;;; reddit
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
;;;; weather
;; (after! solar
;;   (setq calendar-latitude 40.1106
;;         calendar-longitude -88.2073
;;         calendar-location-name "Urbana, IL"))
(after! calendar
  (setq calendar-week-start-day 1))
;; (use-package! forecast
;;   :defer-incrementally t
;;   :commands forecast
;;   :bind (:map doom-leader-map
;;           ("aw" . forecast))
;;   :init
;;   (after! which-key
;;     (add-to-list 'which-key-replacement-alist '((nil . "forecast") . (nil . "Weather"))))
;;   :config
;;   (setq forecast-api-key "3952024acf85777d62f39869da12f853"
;;         forecast-units 'us
;;         forecast-language 'en)
;;   (map! :map forecast-mode-map
;;         :n "q" #'bury-buffer
;;         :n "l" #'forecast-refresh
;;         :n "h" #'describe-mode))
;;;; stumpwm
(use-package! stumpwm-mode
  :defer-incrementally t
  :mode (".stumpwmrc" . stumpwm-mode))
;;;; magnifying glass
(use-package! downplay-mode
  :defer-incrementally t
  :bind (("C-c z" . downplay)))
;;;; community-driven man pages
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
;;;; rtorrent
(use-package! mentor
  :bind (:map doom-leader-map
          ("at" . mentor))
  :init
  (after! which-key
    (add-to-list 'which-key-replacement-alist '((nil . "mentor") . (nil . "rTorrent"))))
  :config
  (when (featurep! :editor evil)
    (evil-set-initial-state 'mentor-mode 'emacs)))
;;;; typing practice
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
;;;; command history
(use-package! keyfreq
  :defer-incrementally t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
;;;; coding exercises
(use-package! leetcode
  :defer-incrementally t
  :commands (leetcode)
  :init
  (use-package request-deferred :defer t)
  (use-package graphql :defer t)
  :config
  (setq leetcode-prefer-language "rust")
  (when (featurep! :editor evil)
    (map! :map leetcode--problems-mode-map
          :n "<return>" #'leetcode-show-current-problem)))
;;;; calculator
(use-package! calc
  :defer t
  ;; :bind (:map doom-leader-map
  ;;         ("yc" . calc))
  :init
  ;; (after! which-key
  ;;   (add-to-list 'which-key-replacement-alist
  ;;                '((nil . "calc") . (nil . "calculator"))))
  :custom
  (math-additional-units
   '((gib "1024 * mib" "giga byte")
     (mib "1024 * kib" "mega byte")
     (kib "1024 * b" "kilo byte")
     (b nil "byte")
     (gib "1024 * mib" "giga bit")
     (mib "1024 * kib" "mega bit")
     (kib "1024 * b" "kilo bit")
     (b "b / 8" "bit")))
  (math-units-table nil))
;;;; documentation
(after! info
  (map! :map info-mode-map
        "o" #'ace-link))

;; (use-package! counsel-dash
;;   :commands counsel-dash
;;   :defer-incrementally t
;;   :init
;;   (setq
;;    counsel-dash-docsets-path "~/.docsets"
;;    counsel-dash-enable-debugging t
;;    counsel-dash-ignored-docsets nil))
;;;; pdfs
(use-package! pdf-tools
  :defer-incrementally t
  :magic ("%pdf" . pdf-view-mode)
  :config
  (require 'dash)
  (setq pdf-cache-image-limit 512)
  ;; Prevent killed-buffer error
  ;; (add-hook 'kill-buffer-hook #'clean-up-pdf-idle-timers)
  (add-hook! 'pdf-view-mode-hook
    (company-mode -1))

  ;; (advice-add #'pdf-misc-size-indication :around #'ignore)
  (setq pdf-misc-print-programm "/run/current-system/sw/bin/lpr"
        pdf-misc-print-programm-args '("-P" "Brother_HL-L2320D_series"
                                       "-o" "media=letter"))
  (delq 'pdf-misc-size-indication-minor-mode pdf-tools-enabled-modes)
  (delq 'pdf-misc-context-menu-minor-mode pdf-tools-enabled-modes)
  (delq 'pdf-misc-menu-bar-minor-mode pdf-tools-enabled-modes))
;;;; cell-mode
(use-package! cell
  :defer-incrementally t
  :init
  (add-to-list 'auto-mode-alist (cons "\\.cell" 'cell-mode))
  :config
  (when (featurep! :editor evil)
    (map! :map cell-mode-map
          :m "n" #'cell-sheet-move-cursor-down
          :m "e" #'cell-sheet-move-cursor-up
          :m "m" #'cell-sheet-move-cursor-left
          :m "i" #'cell-sheet-move-cursor-right
          :m "$" #'cell-sheet-move-eol
          :m "gg" #'cell-sheet-move-bob
          :m "G" #'cell-sheet-move-eob
          :n "a" #'cell-sheet-create-cell
          :n "u" #'cell-sheet-create-cell
          :n "v" #'cell-sheet-delete-cell
          :n "<backspace>" #'cell-sheet-delete-cell
          :n "/" #'cell-sheet-undo
          :n "C-r" #'cell-sheet-redo
          :n "d" (lambda! (cell-sheet-set-mark)
                          (evil-visual-state))
          :n "<return>" #'cell-sheet-execute
          :nv "j" #'cell-sheet-copy-to-clipboard
          :nv "'" #'cell-sheet-paste
          :v "d" #'cell-sheet-clear-mark
          :v "s" #'cell-sheet-cut-to-clipboard
          :n "<escape>" (lambda! (evil-force-normal-state)
                                 (cell-sheet-clear-mark))
          :v "<escape>" (lambda! (evil-exit-visual-state)
                                 (cell-sheet-clear-mark)))

    (defun evil-preserve-visual-state-advice (oldfun &rest args)
      (when (prog1 (eq evil-state 'visual)
              (apply oldfun args))
        (evil-visual-state)))

    (cl-loop for fn in '(cell-sheet-move-cursor-down
                         cell-sheet-move-cursor-up
                         cell-sheet-move-cursor-left
                         cell-sheet-move-cursor-right
                         cell-sheet-move-eol
                         cell-sheet-move-bob
                         cell-sheet-move-eob
                         ;; cell-sheet-delete-cell
                         cell-sheet-undo
                         cell-sheet-redo)
             do (advice-add fn :around #'evil-preserve-visual-state-advice))

    (defun evil-exit-visual-state-advice (oldfun &rest args)
      (when (prog1 (eq evil-state 'visual)
              (evil-normal-state)
              (apply oldfun args))
        (advice-remove #'cell-sheet-clear-mark #'evil-exit-visual-state-advice)
        (cell-sheet-clear-mark)
        (advice-add #'cell-sheet-clear-mark :around #'evil-exit-visual-state-advice)))

    (cl-loop for fn in '(cell-sheet-paste
                         cell-sheet-clear-mark
                         cell-sheet-cut-to-clipboard
                         cell-sheet-delete-cell
                         cell-sheet-copy-to-clipboard)
             do (advice-add fn :around #'evil-exit-visual-state-advice))

    (evil-redirect-digit-argument
     (evil-get-auxiliary-keymap cell-mode-map 'motion t)
     "0" #'cell-sheet-move-bol)))
