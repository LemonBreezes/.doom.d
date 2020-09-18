;;; ~/.doom.d/lattie.el -*- lexical-binding: t; -*-

(require 'org-element)
(require 'texmathp)
(require 'cdlatex)
;; (require 'typo)

(defconst alphanumerics
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n
       ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
       ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
       ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ?= ?+ ?\; ?\: ?_)
  "Characters which are assumed to not be a part of smartparens pairs for code
blocks.")

(modify-syntax-entry ?$ "." org-mode-syntax-table)

;; Copied from Lispy
(defmacro lattie-dotimes (n &rest bodyform)
  "Execute N times the BODYFORM unless an error is signaled.
Return nil if couldn't execute BODYFORM at least once.
Otherwise return the amount of times executed."
  (declare (indent 1)
           (debug (form body)))
  `(let ((i 0))
     (catch 'result
       (condition-case e
           (progn
             (while (<= (cl-incf i) ,n)
               ,@bodyform)
             ,n)
         (error
          (when (eq (car e) 'buffer-read-only)
            (message "Buffer is read-only: %s" (current-buffer)))
          (cl-decf i)
          (and (> i 0) i))))))

(defvar lattie-dollar-regexp "[^\\]\\$")
(defvar lattie-dollar-regexp-for-looking-at "\\$")

(defvar lattie-closing-math-regexp
  (concat lattie-dollar-regexp
          "\\|[^\\]\\\\\\(\]\\|\)\\|end\{[^\\s$\\s-]+\}\\)"))
(defvar lattie-closing-math-regexp-for-looking-at
  (concat lattie-dollar-regexp-for-looking-at
          "\\|\\\\\\(\]\\|\)\\|end\{[^\\s$\\s-]+\}\\)"))
(defvar lattie-opening-math-regexp
  (concat lattie-dollar-regexp
          "\\|[^\\]\\\\\\(\\\[\\|\(\\|begin\{[^\\s$\\s-]+\}\\)"))
(defvar lattie-opening-math-regexp-for-looking-at
  (concat lattie-dollar-regexp-for-looking-at
          "\\|\\\\\\(\\\[\\|\(\\|begin\{[^\\s$\\s-]+\}\\)"))

(defvar lattie-closing-math-regexp-with-whitespace
  (concat "\\s-*"
          lattie-dollar-regexp
          "\\|[^\\]\\\\\\(\\s-*\]\\|\\s-*\)\\|\\s-*end\{[^\\s$\\s-]+\}\\)"))
(defvar lattie-closing-math-regexp-with-whitespace-for-looking-at
  (concat "\\s-*"
          lattie-dollar-regexp-for-looking-at
          "\\|[^\\]\\\\\\(\\s-*\]\\|\\s-*\)\\|\\s-*end\{[^\\s$\\s-]+\}\\)"))

(defvar lattie-opening-math-regexp-with-whitespace
  (concat lattie-dollar-regexp
          "\\s-*\\|[^\\]\\\\\\(\\\[\\s-*\\|\(\\s-*\\|begin\{[^\\s$\\s-]+\}\\)"))
(defvar lattie-opening-math-regexp-with-whitespace-for-looking-at
  (concat lattie-dollar-regexp-for-looking-at
          "\\s-*\\|\\\\\\(\\\[\\s-*\\|\(\\s-*\\|begin\{[^\\s$\\s-]+\}\\)"))

(defvar lattie-closing-or-environment-math-regexp
  (concat lattie-dollar-regexp
          "\\|[^\\]\\\\\\(\]\\|\)\\|end\{[^\\s$\\s-]+\}\\|begin\{[^\\s$\\s-]+\}\\)"))
(defvar lattie-closing-or-environment-math-regexp-for-looking-at
  (concat lattie-dollar-regexp-for-looking-at
          "\\|\\\\\\(\]\\|\)\\|end\{[^\\s$\\s-]+\}\\|begin\{[^\\s$\\s-]+\}\\)"))

(defvar lattie-opening-or-environment-math-regexp
  (concat lattie-dollar-regexp
          "\\|[^\\]\\\\\\(\\[\\|\(\\|end\{[^\\s$\\s-]+\}\\|begin\{[^\\s$\\s-]+\}\\)"))
(defvar lattie-opening-or-environment-math-regexp-for-looking-at
  (concat lattie-dollar-regexp-for-looking-at
          "\\|\\\\\\(\\[\\|\(\\|end\{[^\\s$\\s-]+\}\\|begin\{[^\\s$\\s-]+\}\\)"))

(defvar lattie-closing-or-opening-math-regexp
  (concat lattie-dollar-regexp
          "\\|[^\\]\\\\\\(\]\\|\)\\|\[\\|\(\\|end\{[^\\s$\\s-]+\}\\|begin\{[^\\s$\\s-]+\}\\)"))
(defvar lattie-closing-or-opening-math-regexp
  (concat lattie-dollar-regexp-for-looking-at
          "\\|\\\\\\(\]\\|\)\\|\[\\|\(\\|end\{[^\\s$\\s-]+\}\\|begin\{[^\\s$\\s-]+\}\\)"))

(defvar lattie-closing-environment-regexp "[^\\]\\\\end\{\\([^\\s$\\s-]+\\)\}")
(defvar lattie-closing-environment-regexp-for-looking-at "\\\\end\{\\([^\\s$\\s-]+\\)\}")
(defvar lattie-opening-environment-regexp "[^\\]\\\\begin\{\\([^\\s$\\s-]+\\)\}")
(defvar lattie-opening-environment-regexp-for-looking-at "\\\\begin\{\\([^\\s$\\s-]+\\)\}")
(defvar lattie-open-or-close-environment-regexp "[^\\]\\\\end\{[^\\s$\\s-]+\}\\|[^\\]\\\\begin\{[^\\s$\\s-]+\}")
(defvar lattie-open-or-close-environment-regexp-for-looking-at "\\\\end\{[^\\s$\\s-]+\}\\|\\\\begin\{[^\\s$\\s-]+\}")

(defvar lattie-debug nil)

(defconst english-punctuation '(?, ?. ?! ?\? ?\; ?…))

(defun lattie--testing (&optional pt)
  (let ((pt-old (or pt (point))))
    (lattie--)))

(defun lattie--math-p (&optional pt)
  (let* ((pt-old (or pt (point)))
         (pt-new (if (and (eq (char-after pt-old) ?\n)
                          (not (eq (char-before (max (1- pt-old) (point-min))) ?$)))
                     (max (1- pt-old) (point-min))
                   pt-old))
         (face-at-pt (get-text-property (or pt-new (point)) 'face)))
    (or (and (listp face-at-pt)
             (or (memq 'org-latex-and-related face-at-pt)
                 (memq 'font-latex-math-face face-at-pt)
                 (memq 'font-latex-sedate-face face-at-pt)
                 (memq 'font-function-function-name-face face-at-pt)
                 (memq 'tex-math face-at-pt)
                 (memq 'endless/unimportant-latex-face face-at-pt)
                 ;; (memq 'rainbow-delimiters-depth-1-face face-at-pt)
                 ;; (memq 'rainbow-delimiters-depth-2-face face-at-pt)
                 ;; (memq 'rainbow-delimiters-depth-3-face face-at-pt)
                 ;; (memq 'rainbow-delimiters-depth-4-face face-at-pt)
                 ))
        (memq face-at-pt '(org-latex-and-related
                           font-latex-math-face
                           font-latex-sedate-face
                           font-function-function-name-face
                           tex-math
                           endless/unimportant-latex-face)))))

(defun lattie--left-p ()
  (or (and (looking-at lattie-opening-math-regexp-for-looking-at)
           (or (not (eq (char-after) ?$))
               (looking-back "[\s\t]*\n" (max (1- (point-at-bol)) (point-min)))
               (lattie--at-left-side-of-opening-expression?))
           (or (not (eq (char-after) ?$))
               (looking-back "[\s\t]*\n" (max (1- (point-at-bol)) (point-min)))
               (not (looking-back lattie-closing-math-regexp (point-at-bol) nil))))
      (looking-at lattie-open-or-close-environment-regexp-for-looking-at)))

(defun lattie--right-p (&rest _)
  (or (and (looking-back lattie-closing-math-regexp (max (1- (point-at-bol)) (point-min)) nil)
           (or (not (eq (char-before) ?$))
               (looking-at "[\s\t]*\n")
               (lattie--at-right-side-of-closing-expression?))
           (or (not (eq (char-before) ?$))
               (looking-at "[\s\t]*\n")
               (not (looking-at lattie-opening-math-regexp-for-looking-at))))
      (looking-back lattie-open-or-close-environment-regexp (point-min) nil)))

(defmacro lattie-stay-special (&rest forms)
  `(let ((pt (point)))
     ,@forms
     (while (and (not (eq (point) pt))
                 (or (eq (char-before) ?\\)
                     ;; (looking-back "\\\\]" (- (point) 3))
                     (invisible-p (point))
                     (looking-back "\\\\\\\\\\\(\)\\|\]\\)" (max (- (point) 3) 1))))
       ,@forms
       (setq pt (point)))
     (when (or (eq (point) ,(point))
               (eq (char-before) ?\\)
               ;; (looking-back "\\\\)" (- (point) 3))
               ;; (looking-back "\\\\]" (- (point) 3))
               (invisible-p (point))
               (looking-back "\\\\\\\\\\\(\)\\|\]\\)" (max (- (point) 3) (point-min))))
       (goto-char ,(point)))))

(defun lattie--at-left-side-of-opening-expression? ()
  (save-excursion
    (not (lattie--math-p
          (progn (re-search-backward "[^\\s.]" nil t)
                 ;; (re-search-backward "[^\\sw]" nil t)
                 (while (eq (char-before) ?\\)
                   (forward-char -1))
                 (point))))))

(defun lattie--at-right-side-of-closing-expression? ()
  (save-excursion
    (and (not (and (eq (char-after) ?$)
                   (eq (- (point-max) (point))
                       1)))
         ;; or (and (eq (char-before) ?$)
         ;;      (eq (char-after) ?\n))
         (not (lattie--math-p
               (progn (unless (or (looking-at "[^\s.\$]+\\$")
                                  (and (eq (char-before) ?$)
                                       (eq (char-after) ?$)))
                        (re-search-forward "[^\\s.]" nil t))
                      (while (eq (char-before) ?\\)
                        (forward-char -1))
                      (point)))))))

(defun lattie--special-p (&rest _)
  (or (lattie--left-p)
      (lattie--right-p)))

(defun lattie--not-special-p (&rest _)
  (not (lattie--special-p)))

(defun lattie--not-math-p (&rest _)
  (not (lattie--math-p)))

(defun lattie--math-p-ignore-args (&rest _)
  (lattie--math-p))

(defun lattie--is-alphanumeric-p (x)
  (memq x alphanumerics))

(defun lattie--get-forward-limit ()
  (save-excursion
    (if (looking-back lattie-opening-environment-regexp (1- (point-at-bol)) nil)
        (goto-char (match-beginning 0))
      (forward-char -1))
    ;; (message "initial: %s %s" (match-string 0) (match-string 1))
    (let* ((limit (save-excursion (re-search-forward lattie-closing-math-regexp nil t)))
           (match (match-data))
           (op (re-search-forward lattie-opening-environment-regexp limit t))
           (new-limit? (and (match-string-no-properties 1)
                            ;; (message "%s" (match-string-no-properties 1))
                            (save-excursion
                              (re-search-forward
                               (concat "[^\\]\\\\end{\\("
                                       (regexp-quote (match-string-no-properties 1))
                                       "\\)}")
                               nil
                               t)))))
      (unless new-limit? (set-match-data match))
      ;; (message "limits: %s %s %s %s" limit new-limit? op (match-string-no-properties 1))
      (when (match-string-no-properties 1)
        (while (and (integerp op)
                    (integerp new-limit?)
                    (integerp limit)
                    (< op limit)
                    (< limit new-limit?))
          (setq limit new-limit?
                match (match-data)
                op (re-search-forward lattie-opening-environment-regexp limit t)
                new-limit? (save-excursion
                             (re-search-forward
                              (concat "[^\\]\\\\end{\\("
                                      (regexp-quote (match-string-no-properties 1))
                                      "\\)}")
                              nil
                              t)))
          ;; (message "new limit: %s %s %s" limit new-limit? op)
          ))
      (set-match-data match)
      ;; (message "final limit: %s %s" limit (match-string 0))
      limit)))

;; (defun lattie--get-backward-limit ()
;;   (save-excursion
;;     (if (looking-at lattie-closing-environment-regexp-for-looking-at)
;;         (goto-char (match-end 0))
;;       (forward-char -1))
;;     ;; (message "initial: %s %s" (match-string 0) (match-string 1))
;;     (let* ((limit (save-excursion (re-search-backward lattie-opening-math-regexp nil t)))
;;            (match (match-data))
;;            (cl (re-search-backward lattie-closing-environment-regexp limit t))
;;            (new-limit? (and (match-string-no-properties 1)
;;                             ;; (message "%s" (match-string-no-properties 1))
;;                             (save-match-data
;;                               (re-search-backward
;;                                (concat "[^\\]\\\\begin{"
;;                                        (regexp-quote (match-string-no-properties 1))
;;                                        "}")
                                        ;Rop ;                                nil
;;                                t)))))
;;       ;; (message "limits: %s %s %s %s" limit new-limit? op (match-string-no-properties 1))
;;       (when (match-string-no-properties 1)
;;         (while (and (integerp cl)
;;                     (integerp new-limit?)
;;                     (integerp limit)
;;                     (> op limit)
;;                     (> limit new-limit?))
;;           (setq limit new-limit?
;;                 match (match-data)
;;                 op (re-search-backward lattie-closing-environment-regexp limit t)
;;                 new-limit? (save-excursion
;;                              (re-search-backward
;;                               (concat "[^\\]\\\\begin{"
;;                                       (regexp-quote (match-string-no-properties 1))
;;                                       "}")
;;                               nil
;;                               t)))
;;           ;; (message "new limit: %s %s %s" limit new-limit? op)
;;           ))
;;       (set-match-data match)
;;       limit)))

;; (evil-define-operator lattie-fontify-region (beg end)
;;   (interactive "<R>")
;;   (let ((faces (get-text-property (point) 'face)))
;;     (put-text-property beg
;;                        end
;;                        'font-lock-face
;;                        (cons 'font-latex-math-face faces))))

(defun lattie-fontify-buffer ()
  (interactive)
  (font-lock-default-fontify-region (point-min) (point-max) nil))

(defun lattie-forward (arg &optional do-not-trim-p)
  (interactive "p")
  (lattie-dotimes arg
    (unless do-not-trim-p (lattie--trim-whitespace-right))
    (forward-char -1)
    (lattie--get-forward-limit)
    (if (eq (point)
            (progn (cond ((string-prefix-p "\\end{" (substring-no-properties (match-string-no-properties 0) 1 nil))
                          (goto-char (match-end 0)))
                         ((re-search-forward lattie-closing-math-regexp nil t)
                          (when (not (lattie--right-p))
                            (re-search-forward lattie-closing-math-regexp nil t)))
                         ((eq (char-after) ?\})
                          (forward-char 1)))
                   (point)))
        (progn (unless do-not-trim-p (lattie--trim-whitespace-right))
               (forward-char 1))
      (unless do-not-trim-p (lattie--trim-whitespace-right)))))

(defmacro lattie-dotimes (n &rest forms)
  `(dotimes (_i ,n)
     ,@forms))

(defun lattie-backward (arg)
  (interactive "p")
  (lattie-dotimes arg
    (if (looking-back lattie-closing-environment-regexp (1- (point-at-bol)) nil)
        (re-search-backward (concat "\\\\begin\{"
                                    (regexp-quote (match-string-no-properties 1))
                                    "}")
                            nil
                            t)
      (lattie-stay-special (re-search-backward lattie-opening-math-regexp nil t)
                           (forward-char 1))
      (when (not (lattie--left-p))
        (lattie-stay-special
         (re-search-backward lattie-opening-math-regexp nil t)
         (forward-char 1))))))

(defun lattie--trim-whitespace-right ()
  (cond ((eq (char-before) ?$)
         (forward-char -1)
         (let* ((end (point))
                (offset (skip-syntax-backward "\\s-"))
                (s (buffer-substring-no-properties (+ end offset) end)))
           (unless (progn (delete-region (+ end offset) end)
                          (forward-char 1)
                          (prog1 (lattie--at-right-side-of-closing-expression?)
                            (forward-char -1)))
             (insert s)))
         (forward-char 1))))

;; (defun lattie--trim-whitespace-left ()
;;   (cond ((eq (char-after) ?$)
;;          (forward-char 1)
;;          (let* ((beg (point))
;;                 (offset (skip-syntax-forward "\\s-"))
;;                 (s (buffer-substring-no-properties (+ beg offset) beg)))
;;            (unless (progn (delete-region beg (+ beg offset))
;;                           (lattie--at-left-side-of-opening-expression?))
;;              (insert s)))
;;          (forward-char -1))))

(defun lattie--debug (&rest args)
  (when lattie-debug
    (apply #'message args)))

(defun lattie-right (arg)
  "FIXME Maybe I should just remove this."
  (skip-syntax-backward "\\s-" (point-at-eol))
  (lattie-dotimes arg
    (unless (lattie--at-right-side-of-closing-expression?)
      (cond ((looking-at lattie-opening-environment-regexp-for-looking-at)
             (goto-char (match-end 0)))
            (t (lattie-forward 1))))))

(defun lattie-left (arg)
  "FIXME Maybe I should just remove this"
  (let ((pt (point)))
    (skip-syntax-backward "\\s-" (point-at-bol))
    (when (eq (point)
              (progn (lattie-dotimes arg
                       (unless (and (lattie--at-left-side-of-opening-expression?)
                                    (not (looking-at lattie-open-or-close-environment-regexp-for-looking-at)))
                         (cond ((looking-at lattie-open-or-close-environment-regexp-for-looking-at)
                                (goto-char (match-end 0))
                                (lattie-backward 1))
                               ((looking-back lattie-closing-environment-regexp (1- (point-at-bol)) t)
                                (goto-char (1+ (match-beginning 0))))
                               (t (lattie-backward 1)))))
                     (point)))
      (goto-char pt))))

(defun lattie-up (arg)
  (let ((data nil)
        (pt (point)))
    (lattie-dotimes arg
      (lattie--special-p)
      (cond ((or (string-prefix-p "\\end\{" (match-string-no-properties 0))
                 (string-prefix-p "\\begin\{" (match-string-no-properties 0)))
             (if (looking-at lattie-open-or-close-environment-regexp-for-looking-at)
                 (progn (re-search-backward lattie-closing-or-environment-math-regexp nil t)
                        (forward-char 1)
                        (when (string-suffix-p "$" (match-string-no-properties 0))
                          (re-search-backward lattie-opening-or-environment-math-regexp nil t)
                          (forward-char 1)))
               (re-search-backward lattie-closing-or-environment-math-regexp nil t)
               (re-search-backward lattie-closing-or-environment-math-regexp nil t)
               (when (string-suffix-p "$" (match-string-no-properties 0))
                 (re-search-forward lattie-closing-math-regexp nil t))
               (goto-char (match-end 0))))
            ((eq (char-before) ?$)
             (save-excursion (re-search-backward lattie-closing-math-regexp nil t))
             (when (string-suffix-p "$" (match-string-no-properties 0))
               (progn (goto-char (1+ (match-beginning 0)))
                      (setq data (match-data))
                      (re-search-backward lattie-closing-math-regexp nil t)
                      (re-search-backward lattie-closing-math-regexp nil t)))
             (goto-char (match-end 0))
             (when (and (eq (char-before) ?$)
                        (not (lattie--right-p)))
               (set-match-data data)
               (goto-char (match-end 0))))
            ((lattie--right-p)
             (goto-char (1+ (match-beginning 0)))
             (re-search-backward lattie-closing-or-environment-math-regexp nil t)
             (goto-char (match-end 0)))
            ((lattie--left-p)
             (re-search-backward lattie-opening-or-environment-math-regexp nil t)
             (unless (equal (match-string-no-properties 0) "\n")
               (goto-char (1+ (match-beginning 0))))
             (when (string-suffix-p "$" (match-string-no-properties 0))
               (progn (goto-char (match-beginning 0))
                      (re-search-backward lattie-closing-or-environment-math-regexp nil t)
                      (forward-char 1))))))
    (when (< pt (point))
      (goto-char pt))))

(defun lattie-down (arg)
  (let ((data nil)
        (pt (point)))
    (lattie-dotimes arg
      (lattie--special-p)
      (cond ((or (string-match-p "\\\\end\{" (match-string-no-properties 0))
                 (string-match-p "\\\\begin\{" (match-string-no-properties 0)))
             (if (looking-at lattie-open-or-close-environment-regexp-for-looking-at)
                 (progn (goto-char (match-end 0))
                        (re-search-forward lattie-closing-or-environment-math-regexp nil t)
                        (goto-char (1+ (match-beginning 0))))
               (goto-char (match-end 0))
               ;; I am currently at the right side of \\begin/end{...} statement
               (re-search-forward lattie-open-or-close-environment-regexp nil t)
               (when (string-suffix-p "$" (match-string-no-properties 0))
                 (progn (goto-char (match-end 0))
                        (setq data (match-data))
                        (re-search-forward lattie-closing-math-regexp nil t)))))
            ((eq (char-before) ?$)
             (save-excursion (re-search-forward lattie-closing-or-opening-math-regexp nil t))
             (setq data (match-data))
             (when (string-suffix-p "$" (match-string-no-properties 0))
               (progn (goto-char (match-end 0))
                      (setq data (match-data))
                      (re-search-forward lattie-closing-math-regexp nil t)))
             (goto-char (match-end 0))
             (when (and (eq (char-before) ?$)
                        (not (lattie--right-p)))
               (set-match-data data)
               (goto-char (match-end 0))))
            ((eq (char-after) ?$)
             (forward-char 1)
             (re-search-forward lattie-opening-or-environment-math-regexp nil t)
             (when (string-match-p "\$$" (match-string-no-properties 0))
               (progn (goto-char (match-end 0))
                      (re-search-forward lattie-opening-or-environment-math-regexp nil t)
                      (goto-char (1+ (match-beginning 0))))))
            ((lattie--right-p)
             (goto-char (match-end 0))
             (re-search-forward lattie-closing-or-environment-math-regexp nil t)
             (when (string-suffix-p "$" (match-string-no-properties 0))
               (progn (goto-char (match-end 0))
                      (setq data (match-data))
                      (re-search-forward lattie-opening-or-environment-math-regexp nil t))))
            ((lattie--left-p)
             (goto-char (match-end 0))
             (re-search-forward lattie-opening-or-environment-math-regexp nil t)
             (goto-char (match-beginning 0)))))
    (when (<= (1- (point)) pt)
      (goto-char pt))))

;; (defun lattie-up (arg)
;;   (interactive "p")
;;   (cond ((lattie--left-p)
;;          (lattie-backward arg)
;;          (lattie-left 1))
;;         ((lattie--right-p)
;;          (lattie-backward (1+ arg))
;;          (lattie-right 1))))

(defun lattie--char-before-is-not-whitespace-p (&rest _)
  (or (bobp)
      (not (eq (char-syntax (char-before))
               ?\s))))

(defun lattie--is-english-punctuation-p (c)
  (memq c english-punctuation))

;; (advice-add #'org-babel-do-in-edit-buffer :around
;;             (defun lattie--remove-hooks-for-org-babel (oldfun &rest args)
;;               (apply oldfun args)))

;; (advice-add #'org-src-font-lock-fontify-block
;;             :around
;;             #'lattie--remove-hooks-for-org-babel)

(defun lattie--ansi-number-p (c)
  (<= ?0 c ?9))

(defun lattie-self-insert-command (arg)
  (interactive "p")
  (lattie--skip-past-deadline)
  (lattie--skip-past-property-drawer)
  (when (looking-back org-property-end-re (point-at-bol))
    (if (looking-at "\n\n")
        (forward-char 1)
      (insert-char ?\n)))
  (if (memq major-mode '(org-mode org-journal-mode))
      (let ((faces (get-text-property (point) 'face))
            (lattie--math-p (lattie--math-p)))
        (when (and lattie--math-p
                   (not (lattie--right-p)))
          (run-at-time 0 nil
                       (lambda (&rest _)
                         (put-text-property (save-excursion (forward-char -1)
                                                            (skip-chars-backward "^\s\t\n")
                                                            (point))
                                            (1+ (point))
                                            'font-lock-face
                                            ;; (cons 'font-latex-math-face faces)
                                            (cl-pushnew 'font-latex-math-face faces :test #'eq)))))
        (cond ((eq (get-text-property (max (1- (point)) (point-min)) 'face)
                   'org-meta-line)
               (org-self-insert-command arg))
              ((org-in-src-block-p t)
               (if (and (eq (length (this-command-keys-vector)) 1)
                        (memq (aref (this-command-keys-vector) 0)
                              alphanumerics))
                   (self-insert-command arg)
                 (org-babel-do-in-edit-buffer
                  (self-insert-command arg))))
              (t (pcase (and (> (length (this-command-keys)) 0)
                             (aref (this-command-keys-vector) 0))
                   (?- (cond ((and (bolp)
                                   (not lattie--math-p))
                              (insert "- "))
                             ((and lattie--math-p
                                   (not (memq (char-before) '(?- ?|))))
                              (lattie--insert-string-with-1-space-padding-maybe
                               "-" nil 'no-right-padding))
                             (t (org-self-insert-command arg))))
                   (?. (if (looking-back "\\. ?")
                           (progn (delete-region (match-beginning 0) (match-end 0))
                                  (lattie--insert-string-with-1-space-padding-maybe "…" 'no-left-padding))
                         (org-self-insert-command arg)
                         (unless (eq (char-after) ?\")
                           (setq unread-input-method-events '(32)))))
                   (?, (cond ((lattie--math-shorthand-p)
                              (insert ","))
                             (lattie--math-p
                              (lattie--insert-string-with-1-space-padding-maybe "," 'no-left-padding))
                             (t (org-self-insert-command arg)
                                (unless (lattie--math-shorthand-p)
                                  (setq unread-input-method-events '(32))))))
                   ;; (?p (cond (lattie--math-p
                   ;;            (org-self-insert-command arg))
                   ;;           ((equal (s-trim (buffer-substring-no-properties (max (- (point) 2)
                   ;;                                                                (point-min))
                   ;;                                                           (point)))
                   ;;                   "c")
                   ;;            (delete-char -1)
                   ;;            (special-lattie-dollar))
                   ;;           (t (org-self-insert-command arg))))
                   ;; ((and (or (pred lattie--not-math-p)
                   ;;           (pred lattie--right-p))
                   ;;       (pred lattie--char-before-is-not-whitespace-p)
                   ;;       (pred lattie--is-english-punctuation-p)
                   ;;       (pred lattie--not-math-shorthand-p))
                   ;;  (org-self-insert-command arg)
                   ;;  (setq unread-input-method-events '(32))
                   ;;  (put-text-property (1- (point))
                   ;;                     (point)
                   ;;                     'font-lock-face
                   ;;                     nil))
                   (?= (cond ((lattie--math-shorthand-p)
                              (insert "="))
                             (lattie--math-p
                              (if (equal (buffer-substring-no-properties (max (- (point) 2) (point-min))
                                                                         (point))
                                         ": ")
                                  (progn (delete-char -1)
                                         (insert "= "))
                                (lattie--insert-string-with-1-space-padding-maybe "=")))
                             (t (cond ((eq (char-syntax (char-before)) ?\s)
                                       (org-self-insert-command 2)
                                       (forward-char -1))
                                      ((eq (char-after) ?=)
                                       (forward-char 1))
                                      (t (let ((faces (get-text-property (point) 'face))
                                               (end (+ 2 (point)))
                                               (beg (and (skip-chars-backward "^\s\t\n")
                                                         (not (eq (char-after) ?=))
                                                         (point))))
                                           (if (not beg)
                                               (progn (goto-char (- end 2))
                                                      (org-self-insert-command arg))
                                             (insert-char ?$)
                                             (goto-char (1- end))
                                             (insert-char ?$)
                                             (forward-char -1)
                                             (put-text-property beg end 'font-lock-face

                                                                (cl-pushnew 'font-latex-math-face faces :test #'eq))
                                             (lattie-self-insert-command arg))))))))
                   (?\; (cond ((and lattie--math-p
                                    (not (lattie--right-p)))
                               (expand-abbrev)
                               (lattie--insert-string-with-1-space-padding-maybe ":" 'no-left-padding)
                               ;; (let ((faces (get-text-property (point) 'face)))
                               ;;   (put-text-property (1- (point))
                               ;;                      (1+ (point))
                               ;;                      'font-lock-face
                               ;;                      (cl-pushnew 'font-latex-math-face faces :test #'eq)))
                               )
                              ((lattie--math-shorthand-p)
                               (insert ":"))
                              ((equal (buffer-substring-no-properties (- (point) 2)
                                                                      (point))
                                      "$ ")
                               (save-excursion (forward-char -1)
                                               (insert-char ?\;)))
                              ((and (<= (length (thing-at-point 'word 'no-properties))
                                        2)
                                    (eq (point)
                                        (cdr (bounds-of-thing-at-point 'word))))
                               (let ((l (length (thing-at-point 'word 'no-properties))))
                                 (goto-char (car (bounds-of-thing-at-point 'word)))
                                 (insert-char ?$)
                                 (goto-char (cdr (bounds-of-thing-at-point 'word)))
                                 (insert ": $")
                                 (forward-char -1)
                                 (put-text-property (- (point)
                                                       (length ": $")
                                                       l)
                                                    (1+ (point))
                                                    'font-lock-face
                                                    'font-latex-math-face)))
                              (t (lattie--insert-string-with-1-space-padding-maybe ";" 'no-left-padding))))
                   (?\) (if-let* ((parsed-syntax (syntax-ppss))
                                  (op (second (syntax-ppss)))
                                  (in-top-sexp-p (and (integerp op)
                                                      (string-match-p "[\s\t\n],"
                                                                      (buffer-substring-no-properties (- op 2)
                                                                                                      op))))
                                  (math-shorthand-p (progn (forward-char 1)
                                                           (lattie--math-shorthand-p))))
                            (progn (lattie--apply-math-shorthand-before-point)
                                   (unless (eq (char-before) ?\n)
                                     (insert " ")))
                          (org-self-insert-command arg)))
                   (?~ (if (lattie--math-shorthand-p)
                           (insert "~")
                         (org-self-insert-command arg)))
                   (?* (cond ((lattie--math-shorthand-p)
                              (insert "*"))
                             (lattie--math-p
                              (expand-abbrev)
                              (lattie--insert-string-with-1-space-padding-maybe "∗"))
                             (t (org-self-insert-command arg))))
                   (?< (cond ((lattie--math-shorthand-p)
                              (insert "<"))
                             (lattie--math-p
                              (expand-abbrev)
                              (lattie--insert-string-with-1-space-padding-maybe "<"))
                             (t (org-self-insert-command arg))))
                   (?> (cond ((lattie--math-shorthand-p)
                              (insert ">"))
                             (lattie--math-p
                              (lattie--insert-string-with-1-space-padding-maybe ">" (memq (char-before) '(?- ?=)))
                              (save-excursion (skip-chars-backward "\s\t" (point-at-bol))
                                              (expand-abbrev)))
                             (t (org-self-insert-command arg))))
                   (?/ (cond ((lattie--math-shorthand-p)
                              (insert "/"))
                             ((eq (char-after) ?/)
                              (forward-char 1))
                             (t (org-self-insert-command arg))))
                   ((and (pred lattie--math-p-ignore-args)
                         (pred lattie--looking-back-at-math-symbol-p))
                    (let ((faces (get-text-property (point) 'face)))
                      (put-text-property (1- (point))
                                         (progn (if (string-match-p "\\s-+" (this-command-keys))
                                                    (insert (this-command-keys))
                                                  (insert " " (this-command-keys)))
                                                (point))
                                         'font-lock-face
                                         (cl-pushnew 'font-latex-math-face faces :test #'eq))))
                   (?\s (org-self-insert-command arg))
                   ((pred lattie--ansi-number-p)
                    (cond ((looking-back "\\. " (max (- (point) 2) (point-min)))
                           (delete-char -1)
                           (org-self-insert-command arg))
                          (t (org-self-insert-command arg))))
                   (_ (when (or (and (lattie--right-p)
                                     (eq (char-before) ?$)
                                     (not (and (> (length (this-command-keys-vector)) 0)
                                               (memq (aref (this-command-keys-vector) 0)
                                                     '(?\s ?\t ?\n)))))
                                (and (memq (char-before) '(?> ?. ?\;))
                                     (not (lattie--math-shorthand-p))))
                        (expand-abbrev)
                        (insert " "))
                      (org-self-insert-command arg))))))
    (self-insert-command arg))
  (when (and (memq major-mode '(org-mode org-journal-mode))
             (save-excursion (forward-char -1)
                             (or (lattie--right-p)
                                 (eq (char-before) ?\n))))
    (put-text-property (1- (point))
                       (point)
                       'font-lock-face
                       nil)))

(with-eval-after-load 'company
  (add-to-list 'company-begin-commands #'lattie-self-insert-command))

;; (defvar lattie-unicode-abbrevs nil)
;; (setq lattie-unicode-abbrevs
;;       '(("<->" . 57600)
;;         ("<-->" . 57601)
;;         ("<--->" . 57602)
;;         ("<---->" . 57603)
;;         ("<----->" . 57604)
;;         ("<=>" . 57605)
;;         ("<==>" . 57606)
;;         ("<===>" . 57607)
;;         ("<====>" . 57608)
;;         ("<=====>" . 57609)
;;         ("<**>" . 57610)
;;         ("<***>" . 57611)
;;         ("<****>" . 57612)
;;         ("<*****>" . 57613)
;;         ("<!--" . 57614)
;;         ("<!---" . 57615)
;;         ("<$" . 57616)
;;         ("<$>" . 57617)
;;         ("$>" . 57618)
;;         ("<." . 57619)
;;         ("<.>" . 57620)
;;         (".>" . 57621)
;;         ("<*" . 57622)
;;         ("<*>" . 57623)
;;         ("*>" . 57624)
;;         ("<\\" . 57625)
;;         ("<\\>" . 57626)
;;         ("\\>" . 57627)
;;         ("</" . 57628)
;;         ("</>" . 57629)
;;         ("/>" . 57630)
;;         ("<\"" . 57631)
;;         ("<\">" . 57632)
;;         ("\">" . 57633)
;;         ("<'" . 57634)
;;         ("<'>" . 57635)
;;         ("'>" . 57636)
;;         ("<^" . 57637)
;;         ("<^>" . 57638)
;;         ("^>" . 57639)
;;         ("<&" . 57640)
;;         ("<&>" . 57641)
;;         ("&>" . 57642)
;;         ("<%" . 57643)
;;         ("<%>" . 57644)
;;         ("%>" . 57645)
;;         ("<@" . 57646)
;;         ("<@>" . 57647)
;;         ("@>" . 57648)
;;         ("<#" . 57649)
;;         ("<#>" . 57650)
;;         ("#>" . 57651)
;;         ("<+" . 57652)
;;         ("<+>" . 57653)
;;         ("+>" . 57654)
;;         ("<-" . 57655)
;;         ("<->" . 57656)
;;         ("->" . 57657)
;;         ("<!" . 57658)
;;         ("<!>" . 57659)
;;         ("!>" . 57660)
;;         ("<?" . 57661)
;;         ("<?>" . 57662)
;;         ("?>" . 57663)
;;         ("<|" . 57664)
;;         ("<|>" . 57665)
;;         ("|>" . 57666)
;;         ("<:" . 57667)
;;         ("<:>" . 57668)
;;         (":>" . 57669)
;;         ("::" . 57670)
;;         (":::" . 57671)
;;         ("::::" . 57672)
;;         ("->" . 57673)
;;         ("->-" . 57674)
;;         ("->--" . 57675)
;;         ("->>" . 57676)
;;         ("->>-" . 57677)
;;         ("->>--" . 57678)
;;         ("->>>" . 57679)
;;         ("->>>-" . 57680)
;;         ("->>>--" . 57681)
;;         ("-->" . 57682)
;;         ("-->-" . 57683)
;;         ("-->--" . 57684)
;;         ("-->>" . 57685)
;;         ("-->>-" . 57686)
;;         ("-->>--" . 57687)
;;         ("-->>>" . 57688)
;;         ("-->>>-" . 57689)
;;         ("-->>>--" . 57690)
;;         (">-" . 57691)
;;         (">--" . 57692)
;;         (">>-" . 57693)
;;         (">>--" . 57694)
;;         (">>>-" . 57695)
;;         (">>>--" . 57696)
;;         ("=>" . 57697)
;;         ("=>=" . 57698)
;;         ("=>==" . 57699)
;;         ("=>>" . 57700)
;;         ("=>>=" . 57701)
;;         ("=>>==" . 57702)
;;         ("=>>>" . 57703)
;;         ("=>>>=" . 57704)
;;         ("=>>>==" . 57705)
;;         ("==>" . 57706)
;;         ("==>=" . 57707)
;;         ("==>==" . 57708)
;;         ("==>>" . 57709)
;;         ("==>>=" . 57710)
;;         ("==>>==" . 57711)
;;         ("==>>>" . 57712)
;;         ("==>>>=" . 57713)
;;         ("==>>>==" . 57714)
;;         (">=" . 57715)
;;         (">==" . 57716)
;;         (">>=" . 57717)
;;         (">>==" . 57718)
;;         (">>>=" . 57719)
;;         (">>>==" . 57720)
;;         ("<-" . 57721)
;;         ("-<-" . 57722)
;;         ("--<-" . 57723)
;;         ("<<-" . 57724)
;;         ("-<<-" . 57725)
;;         ("--<<-" . 57726)
;;         ("<<<-" . 57727)
;;         ("-<<<-" . 57728)
;;         ("--<<<-" . 57729)
;;         ("<--" . 57730)
;;         ("-<--" . 57731)
;;         ("--<--" . 57732)
;;         ("<<--" . 57733)
;;         ("-<<--" . 57734)
;;         ("--<<--" . 57735)
;;         ("<<<--" . 57736)
;;         ("-<<<--" . 57737)
;;         ("--<<<--" . 57738)
;;         ("-<" . 57739)
;;         ("--<" . 57740)
;;         ("-<<" . 57741)
;;         ("--<<" . 57742)
;;         ("-<<<" . 57743)
;;         ("--<<<" . 57744)
;;         ("<=" . 57745)
;;         ("=<=" . 57746)
;;         ("==<=" . 57747)
;;         ("<<=" . 57748)
;;         ("=<<=" . 57749)
;;         ("==<<=" . 57750)
;;         ("<<<=" . 57751)
;;         ("=<<<=" . 57752)
;;         ("==<<<=" . 57753)
;;         ("<==" . 57754)
;;         ("=<==" . 57755)
;;         ("==<==" . 57756)
;;         ("<<==" . 57757)
;;         ("=<<==" . 57758)
;;         ("==<<==" . 57759)
;;         ("<<<==" . 57760)
;;         ("=<<<==" . 57761)
;;         ("==<<<==" . 57762)
;;         ("=<" . 57763)
;;         ("==<" . 57764)
;;         ("=<<" . 57765)
;;         ("==<<" . 57766)
;;         ("=<<<" . 57767)
;;         ("==<<<" . 57768)
;;         (">=>" . 57769)
;;         (">->" . 57770)
;;         (">-->" . 57771)
;;         (">==>" . 57772)
;;         ("<=<" . 57773)
;;         ("<-<" . 57774)
;;         ("<--<" . 57775)
;;         ("<==<" . 57776)
;;         (">>" . 57777)
;;         (">>>" . 57778)
;;         ("<<" . 57779)
;;         ("<<<" . 57780)
;;         (":+" . 57781)
;;         (":-" . 57782)
;;         (":=" . 57783)
;;         ("+:" . 57784)
;;         ("-:" . 57785)
;;         ("=:" . 57786)
;;         ("=^" . 57787)
;;         ("=+" . 57788)
;;         ("=-" . 57789)
;;         ("=*" . 57790)
;;         ("=/" . 57791)
;;         ("=%" . 57792)
;;         ("^=" . 57793)
;;         ("+=" . 57794)
;;         ("-=" . 57795)
;;         ("*=" . 57796)
;;         ("/=" . 57797)
;;         ("%=" . 57798)
;;         ("/\\" . 57799)
;;         ("\\/" . 57800)
;;         ("<>" . ?⋄)
;;         ("<+" . 57802)
;;         ("<+>" . 57803)
;;         ("+>" . 57804)))

;; (defun lattie--get-unicode-at-point ()
;;   (alist-get (lattie--thing-at-point) lattie-unicode-abbrevs nil nil #'equal))

;; (defun lattie--expand-unicode-at-point ()
;;   (when-let ((abbrev (lattie--get-unicode-at-point)))
;;     (delete-region (match-beginning 0)
;;                    (point))
;;     (lattie--insert-string-with-1-space-padding-maybe (char-to-string abbrev))))

;; (defun lattie--thing-at-point ()
;;   (when-let ((end (point))
;;              (beg (save-excursion
;;                     (when (memq (char-before) '(?\s ?\t ?\n))
;;                       (forward-char -1)
;;                       (setq end (1- end)))
;;                     (re-search-backward "[\s\t\n})]"
;;                                         (max 1 (1- (point-at-bol)))
;;                                         t))))
;;     (buffer-substring-no-properties (1+ beg)
;;                                     end)))

;; (remove-hook 'post-self-insert-hook #'lattie--expand-unicode-at-point)

(defun lattie--insert-string-with-1-space-padding-maybe (s &optional no-left-padding no-right-padding)
  (let ((faces (get-text-property (point) 'face))
        (lattie--right-p (lattie--right-p))
        (no-right-padding (or no-right-padding (memq (char-after) '(?\s ?\t)))))
    (if (and (not (sp-point-in-empty-sexp))
             (looking-back "\\(?:[^\\s(]\\|\n\\|\\w\\)\\([\s\t\n]\\)?" (max (point-min)
                                                                            (1- (point-at-bol))) t))
        (if (and (match-string-no-properties 1)
                 (not no-right-padding))
            (insert (concat s " "))
          (insert (concat (if (or no-left-padding
                                  (eq (char-before) ?\s))
                              "" " ") s
                              (if no-right-padding "" " "))))
      (insert s))
    (if lattie--right-p
        (put-text-property (- (point) (length s) 1)
                           (1+ (point))
                           'font-lock-face
                           nil)
      (put-text-property (- (point) (length s) 1)
                         (1+ (point))
                         'font-lock-face
                         faces))))

(defun lattie--looking-back-at-math-symbol-p (&rest _)
  (looking-back "[ \n\t\\$\"\[\\({]\\\\[a-zA-Z]+" (point-at-bol)))

(defun lattie--not-math-shorthand-p (&rest _)
  (not (lattie--math-shorthand-p)))

(defun lattie--skip-past-property-drawer ()
  (when (string-match-p org-property-start-re (buffer-substring-no-properties (point-at-bol)
                                                                              (point-at-eol)))
    (re-search-forward org-property-end-re)))

(defun lattie--skip-past-deadline ()
  (when (string-match-p org-deadline-regexp (buffer-substring-no-properties (point-at-bol)
                                                                            (point-at-eol)))
    (forward-line 1)))

;; (defun lattie--skip-backwards-past-property-drawer ()
;;   (when (save-excursion (when (eq (char-before) ?\n)
;;                           (forward-char -1))
;;                         (looking-back org-property-end-re (point-at-bol)))
;;     (re-search-backward org-property-start-re)
;;     (forward-char -1)))

(defun lattie--skip-backwards-past-deadline ()
  (when (save-excursion (when (eq (char-before) ?\n)
                          (forward-char -1))
                        (string-match-p org-deadline-regexp (buffer-substring-no-properties (point-at-bol)
                                                                                            (point-at-eol))))
    (forward-line -1)))

(defun special-lattie-grave ()
  (interactive)
  (expand-abbrev)
  (cond ((org-in-src-block-p t)
         (org-self-insert-command 1))
        ((lattie--math-p)
         (progn (unless (or (memq (char-before) '(?\s ?\t ?\n ?\{ ?\( ?\[ ?$ ?\"))
                            (equal (this-command-keys-vector)
                                   [?\']))
                  (insert-char ?\s))
                (if (equal (this-command-keys-vector)
                           [?\`])
                    (let ((faces (get-text-property (point) 'face)))
                      (put-text-property (1- (point))
                                         (progn (call-interactively #'cdlatex-math-symbol)
                                                (when (equal (buffer-substring-no-properties (- (point) 7)
                                                                                             (point))
                                                             " \\prime")
                                                  (forward-char -7)
                                                  (delete-char 1)
                                                  (forward-char 6))
                                                (point))
                                         'font-lock-face
                                         (cl-pushnew 'font-latex-math-face faces :test #'eq)))
                  (call-interactively #'cdlatex-math-modify))))
        ((lattie--math-shorthand-p)
         (if (equal (this-command-keys-vector)
                    [?\`])
             (insert-char ?\`)
           (insert-char ?\')))
        (t (org-self-insert-command 1))))

(defun special-lattie-underscore-caret ()
  (interactive)
  (expand-abbrev)
  (cond ((lattie--math-shorthand-p)
         (insert (this-command-keys)
                 "{}")
         (forward-char -1))
        ((lattie--math-p)
         (when (eq (char-syntax (char-before))
                   ?\s)
           (skip-chars-backward "\t\s"))
         (let ((faces (get-text-property (point) 'face)))
           (put-text-property (1- (point))
                              (1+ (progn (insert (if (string-empty-p (this-command-keys))
                                                     (this-command-keys)
                                                   (aref (this-command-keys) 0))
                                                 "{}")
                                         (forward-char -1)
                                         (point)))
                              'font-lock-face
                              (cl-pushnew 'font-latex-math-face faces :test #'eq))))
        (t (lattie-self-insert-command 1))))

(defun special-lattie-backward (arg)
  (interactive "p")
  (if (lattie--special-p)
      (lattie-backward arg)
    (lattie-self-insert-command arg)))

(defun special-lattie-up (arg)
  (interactive "p")
  (cond ((lattie--special-p)
         (lattie-stay-special (lattie-up arg)))
        ;; ((and (eq major-mode 'org-mode)
        ;;       (org-at-heading-p))
        ;;  (org-next-visible-heading arg))
        ;; ((and (eq major-mode 'org-mode)
        ;;       (org-at-item-p))
        ;;  (lattie-dotimes arg (org-next-item)))
        (t (lattie-self-insert-command arg))))

(defun special-lattie-down (arg)
  (interactive "p")
  (cond ((lattie--special-p)
         (lattie-stay-special (lattie-down arg)))
        ;; ((and (eq major-mode 'org-mode)
        ;;       (org-at-heading-p))
        ;;  (org-next-visible-heading arg))
        ;; ((and (eq major-mode 'org-mode)
        ;;       (org-at-item-p))
        ;;  (lattie-dotimes arg (org-next-item)))
        (t (lattie-self-insert-command arg))))

(defun special-lattie-left (arg)
  (interactive "p")
  (cond ((lattie--special-p)
         (lattie-left arg))
        ;; ((and (eq major-mode 'org-mode)
        ;;       (org-at-heading-or-item-p))
        ;;  (org-up-element))
        (t (lattie-self-insert-command arg))))

(defun special-lattie-right (arg)
  (interactive "p")
  (cond ((lattie--special-p)
         (lattie-right arg))
        ;; ((and (eq major-mode 'org-mode)
        ;;       (org-at-heading-or-item-p))
        ;;  (org-down-element))
        (t (lattie-self-insert-command arg))))

;; FIXME Does not work for point at | in "$i$|)"
(defun lattie-delete-backward (arg)
  (interactive "p")
  (cond (mark-active
         (kill-region (mark) (point)))
        ((org-in-src-block-p t)
         (let ((backward-delete-char-untabify-method 'hungry))
           (backward-delete-char-untabify arg)))
        ((and (eq (char-after) ?$)
              (eq (char-before) ?$))
         (delete-char 1)
         (delete-char -1))
        ((and (looking-at "\\(\\\\\\{0,2\\}\\)\\(\)\\|\\]\\|\}\\)")
              (looking-back (concat (regexp-quote (match-string-no-properties 1))
                                    (regexp-quote (cond ((equal (match-string-no-properties 2) ")")
                                                         (identity "("))
                                                        ((equal (match-string-no-properties 2) "]")
                                                         (identity "["))
                                                        ((equal (match-string-no-properties 2) "}")
                                                         (identity "{")))))
                            (point-at-bol)
                            nil))
         (delete-region (- (point) (- (match-end 0) (match-beginning 0)))
                        (+ (point) (- (match-end 0) (match-beginning 0)))))
        ((and (not (lattie--math-p))
              (eq (char-before) (char-after))
              (memq (char-before) '(?* ?/ ?=)))
         (delete-region (1- (point)) (1+ (point))))
        ((and (eq (char-before) (char-before (1- (point))))
              (memq (char-before) '(?* ?/ ?=))
              (not (lattie--math-p)))
         (if (and (memq major-mode '(org-mode org-journal-mode))
                  (looking-back "^\\*+" (point-at-bol) nil))
             (org-delete-char -1)
           (delete-region (- (point) 2) (point))))
        ((looking-back "[_\\^]?\\\\*{[^}]*\\\\*}" (point-at-bol) t)
         (delete-region (match-beginning 0) (match-end 0)))
        ((looking-back "\\\\\\{0,2\\}\\[\\\\\\{0,2\\}\\]" (point-at-bol) t)
         (delete-region (match-beginning 0) (match-end 0)))
        ((looking-back "\\\\\\{0,2\\}\(\\\\\\{0,2\\}\)" (point-at-bol) t)
         (delete-region (match-beginning 0) (match-end 0)))
        ((lattie--right-p)
         (cond ((and              ;(message "%s" (match-string-no-properties 0))
                 (string-prefix-p "\\begin{" (substring-no-properties (match-string-no-properties 0) 1 nil))))
               ((and              ;(message "%s" (match-string-no-properties 0))
                 (string-prefix-p "\\end{" (substring-no-properties (match-string-no-properties 0) 1 nil)))
                (kill-region (save-match-data
                               (re-search-backward
                                (regexp-quote (concat "\\begin{"
                                                      (substring-no-properties (match-string-no-properties 0)
                                                                               6
                                                                               (- (match-end 0) (match-beginning 0) 1))
                                                      "}"))
                                nil))
                             (match-end 0)))
               (t (apply #'kill-region
                         (nreverse (list (point)
                                         (progn (lattie-left arg) (point))))))))
        ((and (looking-back "\\\\begin\{\\([^\s$\s-]+\\)\}" (point-at-bol) nil)
              (match-string-no-properties 1)
              (equal (regexp-quote (match-string-no-properties 1))
                     (and (looking-at "\n?\\\\end\{\\([^\s$\s-]+\\)\}")
                          (match-string-no-properties 1))))
         (delete-region (match-beginning 0)
                        (match-end 0))
         (delete-region (point)
                        (re-search-backward "\\\\begin\{\\([^\s$\s-]+\\)\}")))
        ((and (lattie--math-p)
              (lattie--looking-back-at-math-symbol-p))
         (delete-region (1+ (match-beginning 0)) (match-end 0)))
        ((memq major-mode '(org-mode org-journal-mode))
         (if-let ((context (and (memq major-mode '(org-mode org-journal-mode))
                                (org-element-context))))
             (cond ((and (eq (car context) 'item)
                         (org-element-property :checkbox context)
                         (< (- (point) (org-element-property :begin context)) 7))
                    (delete-region (point) (+ (org-element-property :begin context)
                                              (length (org-element-property :bullet context)))))
                   ((and (eq (car context) 'item)
                         (org-element-property :bullet context)
                         (= (point) (+ (org-element-property :begin context)
                                       (length (org-element-property :bullet context)))))
                    (delete-region (point) (org-element-property :begin context)))
                   ((and (eq (car context) 'headline)
                         (org-element-property :todo-keyword context)
                         (< (- (point)
                               (length (org-element-property :todo-keyword context))
                               (org-element-property :level context)
                               (org-element-property :begin context))
                            3))
                    (delete-region (point)
                                   (+ (org-element-property :begin context)
                                      (org-element-property :level context)
                                      1)))
                   (t (let ((pt (point)))
                        (if (not (eq 0 (skip-syntax-backward "\\s-" (point-at-bol 0))))
                            (let ((s (buffer-substring-no-properties (point) pt)))
                              (delete-region (point) pt)
                              (when (and (not (string-match-p "\n" s))
                                         (lattie--is-english-punctuation-p (char-before)))
                                (org-delete-char -1)))
                          (unless (eq arg 0) (delete-char (1+ (- arg)))
                                  (org-delete-char -1))))))
           (let ((pt (point)))
             (if (not (eq 0 (skip-syntax-backward "\\s-")))
                 (delete-region (point) pt)
               (unless (eq arg 0) (delete-char (1+ (- arg)))
                       (org-delete-char -1)))))
         (when (string-match-p "\\s-*:END:\\s-*"
                               (buffer-substring-no-properties
                                (point-at-bol)
                                (point)))
           (let ((s (s-trim (buffer-substring-no-properties (point) (point-at-eol)))))
             (delete-region (point) (point-at-eol))
             (re-search-backward org-property-start-re)
             (lattie--skip-backwards-past-deadline)
             (forward-char -1)
             (insert s)
             (forward-char (- (length s))))))
        (t (let ((pt (point)))
             (if (not (eq 0 (skip-syntax-backward "\\s-" (point-at-bol 0))))
                 (delete-region (point) pt)
               (unless (eq arg 0) (delete-char (1+ (- arg)))
                       (delete-char -1))))))
  (when (and (lattie--math-p)
             (looking-at "[_^]{}"))
    (delete-region (match-beginning 0) (match-end 0))))

(with-eval-after-load 'snippets
  (el-patch-defun +snippets/delete-backward-char (&optional field)
    "Prevents Yas from interfering with backspace deletion."
    (interactive)
    (let ((field (or field (and (overlayp yas--active-field-overlay)
                                (overlay-buffer yas--active-field-overlay)
                                (overlay-get yas--active-field-overlay 'yas--field)))))
      (unless (and (yas--field-p field)
                   (eq (point) (marker-position (yas--field-start field))))
        (el-patch-swap (call-interactively #'delete-backward-char)
                       (if (memq major-mode '(org-mode org-journal-mode))
                           (call-interactively #'lattie-delete-backward)
                         (call-interactively #'delete-backward-char)))))))

(defun lattie-flow ()
  (cond ((lattie--right-p)
         (cond ((string-prefix-p "\\begin{" (substring-no-properties (match-string-no-properties 0) 1 nil))
                (while (looking-at (concat "\\(\\s-*\\)" lattie-opening-environment-regexp-for-looking-at))
                  (delete-region (match-beginning 1) (match-end 1))
                  (insert-char ?\n)
                  (forward-char (- (match-end 0) (match-end 1))))
                (delete-region (point) (progn (skip-syntax-forward "\\s-") (point)))
                (insert-char ?\n 2)
                (forward-char -1))
               ((string-prefix-p "\\end{" (substring-no-properties (match-string-no-properties 0) 1 nil))
                (while (looking-back (concat lattie-closing-environment-regexp "\\s-*") (point-at-bol -1))
                  (re-search-backward lattie-closing-environment-regexp (point-at-bol -1) t)
                  (forward-char 1)
                  (delete-region (min (point)
                                      (progn (skip-chars-backward "\n")
                                             (+ 2 (point))))
                                 (point))
                  (if (looking-back lattie-opening-environment-regexp-for-looking-at (point-at-bol) t)
                      (progn (insert-char ?\n 2)
                             (forward-char -1))
                    (insert-char ?\n)
                    (forward-char -1))))
               (t (re-search-backward lattie-closing-math-regexp nil t)
                  (forward-char 1))))
        ((lattie--left-p)
         (cond ((string-prefix-p "\\begin{" (substring-no-properties (match-string-no-properties 0) 1 nil))
                ;; (message "left : %s" (match-string-no-properties 0))
                (forward-char (- (match-end 0) (match-beginning 0)))
                (while (looking-at (concat "\\(\\s-+\\)" lattie-opening-environment-regexp-for-looking-at))
                  (delete-region (match-beginning 1) (match-end 1))
                  (insert-char ?\n)
                  (forward-char (- (match-end 0) (match-end 1))))
                (delete-region (point) (progn (skip-syntax-forward "\\s-") (point)))
                (insert-char ?\n 2)
                (forward-char -1))
               (t (re-search-forward lattie-opening-math-regexp nil t))))))

(defun special-lattie-flow (arg)
  (interactive "p")
  (cond ((lattie--special-p)
         (lattie-flow))
        ((and (lattie--math-p)
              (looking-back "[^\\\\]\\\\}" (point-at-bol)))
         (forward-char -2))
        (t (lattie-self-insert-command arg))))

(defun special-lattie-newline-and-indent ()
  (interactive)
  ;; (when (lattie--right-p)
  ;;   (org-preview-latex-fragment))
  (when (fboundp 'org-cycle-hide-all-drawers) (org-cycle-hide-all-drawers))
  (cond ((org-in-src-block-p t)
         (let ((+org-exit-src-code-hook (remove 'ws-butler-trim-eob-lines
                                                +org-exit-src-code-hook)))
           (org-babel-do-in-edit-buffer
            (when-let* ((op (car (sp-point-in-empty-sexp)))
                        (pair (sp-get-pair (car (sp-point-in-empty-sexp))))
                        (insertion-specification (first (--first (equal (second it) "RET")
                                                                 (plist-get pair :post-handlers)))))
              (eval (sp--parse-insertion-spec insertion-specification)))
            (call-interactively #'newline-and-indent))))
        ((and (org-at-heading-p)
              (not (bolp)))
         (let ((s (s-trim (buffer-substring-no-properties (point)
                                                          (point-at-eol)))))
           (delete-region (point) (point-at-eol))
           (unless (eobp)
             (forward-char 1)
             (when (eq (point) (progn (lattie--skip-past-deadline)
                                      (lattie--skip-past-property-drawer)
                                      (point)))
               (forward-char -1)))
           ;; (lattie--skip-past-deadline)
           ;; (lattie--skip-past-property-drawer)
           (call-interactively #'org-return-indent)
           (insert s)
           (beginning-of-line)))
        ((org-list-get-item-begin)
         (org-meta-return))
        ((lattie--math-shorthand-p)
         (lattie--apply-math-shorthand-before-point)
         (unless (eq (char-before) ?\n)
           (call-interactively #'org-return-indent)))
        (t (call-interactively #'org-return-indent)))
  (when (looking-back "\\]\\s-*\n?\\s-*" (max (- (point-at-bol) 2) (point-min)))
    (put-text-property (1- (point))
                       (point)
                       'font-lock-face
                       nil)))

(defun special-lattie-forward (arg)
  (interactive "p")
  (if (lattie--special-p)
      (lattie-forward arg)
    (lattie-self-insert-command arg)))

(defun lattie-close-bracket (arg)
  (interactive "p")
  (expand-abbrev)
  (cond ((org-in-src-block-p t)
         (self-insert-command arg))
        ((and (looking-back "[^\\]\\\\" (1- (point-at-bol)) nil)
              (or (looking-at "\\s-")
                  (eobp)))
         (lattie-self-insert-command arg))
        ((when-let ((context (and (memq major-mode '(org-mode
                                                     org-journal-mode))
                                  (org-element-context))))
           (cond ((and (eq 'headline (car context))
                       (eq (point)
                           (+ (org-element-property :level context)
                              (org-element-property :begin context)
                              1)))
                  (insert "[ ] "))
                 ((or (eq (point)
                          (+ (or (org-list-get-item-begin) most-negative-fixnum)
                             (length (org-element-property :bullet context)))))
                  (insert "[ ] "))
                 (t (lattie-forward arg)))
           t))
        (t (lattie-forward arg))))

;; (let ((context (org-element-context)))
;;   (and (eq 'headline (car context))
;;        (list (org-element-property :level context)
;;              (org-element-property :begin context))))

(defun lattie--in-tikzcd-p ()
  (save-excursion
    (let ((limit (lattie--get-forward-limit)))
      (re-search-forward "\\end{tikzcd}" limit t))))

(defun lattie--ar-snipet-delete-empty-string (s)
  (let ((yas-inhibit-overlay-modification-protection t))
    (when (and (not yas-modified-p)
               yas-moving-away-p)
      (delete-region (- (point) 3)
                     (save-excursion (1- (search-forward "]" (point-at-eol) t)))))
    (if (and (s-starts-with? "(" s)
             (s-ends-with? ")" s))
        (s-wrap s "{" "}")
      s)))

;; (defun lattie-forward-or-eol (arg)
;;   (if (and (eq (point)
;;                (progn (lattie-forward arg)
;;                       (point))))
;;       (if (looking-back "\\\]" (max (- (point) 2) 0))
;;           (progn (end-of-line)
;;                  (special-lattie-newline-and-indent))
;;           (end-of-line))
;;     ""))

(defun lattie-open-bracket (arg)
  (interactive "p")
  (cond ((org-in-src-block-p t)
         (self-insert-command arg))
        ((looking-back "[^\\]\\\\" (1- (point-at-bol)) nil)
         (insert-char ?\[)
         (insert-char ?\\)
         (insert-char ?\])
         (let ((faces (get-text-property (point) 'face)))
           (put-text-property (- (point) 4)
                              (point)
                              'font-lock-face
                              (cl-pushnew 'font-latex-math-face faces :test #'eq)))
         (forward-char -2))
        (t (lattie-backward arg))))

(defun special-lattie-dollar ()
  (interactive)
  (expand-abbrev)
  (let ((lattie--special-p (lattie--special-p)))
    (cond ((and (not lattie--special-p)
                (not (lattie--math-p))
                (eq (char-before) ?\s)
                (eq (char-before (1- (point))) ?$))
           (delete-char -1)
           (forward-char -1)
           (insert "\\,"))
          ((or (and (lattie--math-p)
                    (not lattie--special-p))
               (and (eq (char-after) ?$)
                    (eq (char-before) ?$)))
           (insert-char ?\[)
           (insert-char ?\])
           (forward-char -1))
          (lattie--special-p
           (if (memq major-mode '(org-mode org-journal-mode))
               (org-end-of-line)
             (end-of-line)))
          ((looking-back "[^\\]\\\\" (max (1- (point-at-bol)) (point-min)))
           ())
          (t (when (eq (char-syntax (char-before))
                       ?w)
               (insert-char ?\s))
             (cdlatex-dollar)
             (let ((faces (get-text-property (point) 'face)))
               (put-text-property (max (1- (point)) (point-min))
                                  (min (1+ (point)) (point-max))
                                  'font-lock-face
                                  (cl-pushnew 'font-latex-math-face faces :test #'eq)))))))

(defun special-lattie-back-to-heading ()
  (interactive)
  (cond ((org-in-src-block-p t)
         (org-self-insert-command 1))
        ((and (lattie--math-p)
              (not (lattie--special-p)))
         (expand-abbrev)
         (org-cdlatex-underscore-caret)
         (let ((faces (get-text-property (point) 'face)))
           (put-text-property (max (save-excursion (forward-char -1)
                                                   (skip-chars-backward "^\s\t\n")
                                                   (point))
                                   (point-min))
                              (1+ (point))
                              'font-lock-face
                              (cl-pushnew 'font-latex-math-face faces :test #'eq))))
        ((or (org-at-heading-p)
             (lattie--special-p))
         (org-up-element))
        (t (lattie-self-insert-command 1))))

(defun lattie-insert-dollar ()
  (interactive)
  (if (lattie--math-p)
      ;; (lattie--insert-string-with-1-space-padding-maybe "\\plus")
      (lattie--insert-string-with-1-space-padding-maybe "+")
    (org-self-insert-command 1)))

(defun special-lattie-open-brace ()
  (interactive)
  (cond ((lattie--math-shorthand-p)
         (if (eq (char-before) ?`)
             (insert "{")
           (insert "{}")
           (forward-char -1)))
        ((if (eq (char-before) ?\\)
             (let ((faces (get-text-property (point) 'face)))
               (insert "{\\}")
               (forward-char -2)
               (put-text-property (- (point) 2)
                                  (+ (point) 2)
                                  'font-lock-face
                                  (cl-pushnew 'font-latex-math-face faces :test #'eq)))

           (insert "{}")
           (forward-char -1)))
        ;; ((looking-back "\\\\\\w*\\|[\\^\_]\\|\}" (point-at-bol) nil)
        ;;  (lattie-self-insert-command 1))
        ;; ((search-backward "{" (point-at-bol) t)
        ;;  (forward-char 1)
        ;;  (push-mark (match-end 0) t t)
        ;;  (search-forward-regexp "\\\\\\{0,2\\}\}" (point-at-eol) t)
        ;;  (goto-char (match-beginning 0))
        ;;  (delete-selection-mode 0))
        ))

(defun special-lattie-close-brace ()
  (interactive)
  (cond ((looking-at "\\\\\\{1,2\\}\}")
         (goto-char (match-end 0))
         (deactivate-mark))
        ;; ((eq (char-after) ?\])
        ;;  (forward-char 1))
        ((looking-at "[\)\}]+")
         (skip-chars-forward "\)\]\}"))
        ((lattie--math-p)
         (if (nth 3 (syntax-ppss))
             (progn (insert "{}")
                    (forward-char -1))
           (insert "\\{\\}")
           (forward-char -2)))
        (t (lattie-self-insert-command 1))))

(defun lattie-open-paren ()
  (interactive)
  (if (org-in-src-block-p t)
      (self-insert-command 1)
    (let ((lattie--math-p (lattie--math-p))
          (faces (get-text-property (point) 'face)))
      (when lattie--math-p
        (run-at-time 0 nil
                     (lambda (&rest _)
                       (put-text-property (save-excursion (forward-char -1)
                                                          (skip-chars-backward "^\s\t\n")
                                                          (point))
                                          (1+ (point))
                                          'font-lock-face
                                          (cl-pushnew 'font-latex-math-face faces :test #'eq)))))
      (expand-abbrev)
      (cond (lattie--math-p
             (progn (insert "()")
                    (forward-char -1)
                    (when (eq (char-before) ?\))
                      (forward-char -1))
                    (put-text-property (max (1- (point)) (point-min))
                                       (min (1+ (point)) (point-max))
                                       'font-lock-face
                                       'font-latex-math-face)))
            ;; 65-90, 97-122 are capitalized and uncapitalized letters
            ((and (or (>= 122 (char-before) 97)
                      (>= 90 (char-before) 65))
                  (memq (char-after) '(?\t ?\s ?\n)))
             (insert (concat "()" (if lattie--math-p "" "$")))
             (forward-char (- (prog1 (+ (if lattie--math-p 1 2)
                                        (skip-chars-backward "^\t\s\n"))
                                (insert (if lattie--math-p "" "$"))))))
            (t (lattie-self-insert-command 1))))))

(defun special-lattie-tab ()
  (interactive)
  (if (org-in-src-block-p t)
      (org-babel-do-in-edit-buffer
       (yas-expand)
       (call-interactively #'indent-for-tab-command))
    (let ((lattie--math-p (lattie--math-p))
          (faces (get-text-property (point) 'face)))
      (cond ;; ((expand-abbrev))
       ((or lattie--math-p
            (lattie--math-shorthand-p))
        (when lattie--math-p
          (run-at-time 0 nil
                       (lambda (&rest _)
                         (put-text-property (save-excursion (forward-char -1)
                                                            (skip-chars-backward "^\s\t\n")
                                                            (point))
                                            (1+ (point))
                                            'font-lock-face
                                            (cl-pushnew 'font-latex-math-face faces :test #'eq)))))
        (expand-abbrev)
        (cdlatex-tab)
        (when (and (not (eobp))
                   (eq (char-syntax (char-after)) ?\s))
          (skip-chars-forward "\s\t")))
       (t (org-cycle))))))

;;; Non-TeX tricks
(defun lattieville-table-previous-row-or-eol ()
  (interactive)
  (if (org-at-table-p)
      (call-interactively #'+org/table-previous-row)
    (call-interactively #'org-end-of-line)))

(defun lattie-ctrl-ret ()
  (interactive)
  (let ((direction 'below))
    (if (and (org-at-table-p)
             (eq 1 (- (org-table-end)
                      (point))))
        (let* ((context
                (save-excursion
                  (goto-char (org-table-begin))
                  (forward-char -1)
                  (when (bolp)
                    (back-to-indentation)
                    (forward-char))
                  (org-element-lineage
                   (org-element-context)
                   '(table table-row headline inlinetask item plain-list)
                   t)))
               (type (org-element-type context)))
          (cond ((memq type '(item plain-list))
                 (let ((marker (org-element-property :bullet context))
                       (pad (save-excursion
                              (org-beginning-of-item)
                              (back-to-indentation)
                              (- (point) (line-beginning-position)))))
                   (save-match-data
                     (pcase direction
                       (`below
                        (org-end-of-item)
                        (backward-char)
                        (end-of-line)
                        (if (and marker (string-match "\\([0-9]+\\)\\([).] *\\)" marker))
                            (progn (goto-char (plist-get (cadr context) :end))
                                   (let ((l (line-number-at-pos pt)))
                                     (org-insert-item)
                                     (when (= l (line-number-at-pos))
                                       (org-next-item)
                                       (org-end-of-line))))
                          (insert "\n" (make-string pad 32) (or marker ""))))
                       (`above
                        (org-beginning-of-item)
                        (if (and marker (string-match-p "[0-9]+[).]" marker))
                            (org-insert-item)
                          (insert (make-string pad 32) (or marker ""))
                          (save-excursion (insert "\n")))))))
                 (when (org-element-property :checkbox context)
                   (insert "[ ] ")))

                ((memq type '(table table-row))
                 (pcase direction
                   ('below (save-excursion (org-table-insert-row t))
                           (org-table-next-row))
                   ('above (save-excursion (org-shiftmetadown))
                           (+org/table-previous-row))))

                ((memq type '(headline inlinetask))
                 (let ((level (if (eq (org-element-type context) 'headline)
                                  (org-element-property :level context)
                                1)))
                   (pcase direction
                     (`below
                      (let ((at-eol (>= (point) (1- (line-end-position))))
                            org-insert-heading-respect-content)
                        (ignore org-insert-heading-respect-content)
                        (goto-char (line-end-position))
                        (org-end-of-subtree)
                        (insert (concat "\n"
                                        (when (= level 1)
                                          (if at-eol
                                              (ignore (cl-incf level))
                                            "\n"))
                                        (make-string level ?*)
                                        " "))))
                     (`above
                      (org-back-to-heading)
                      (insert (make-string level ?*) " ")
                      (save-excursion
                        (insert "\n")
                        (if (= level 1) (insert "\n")))))
                   (when-let* ((todo-keyword (org-element-property :todo-keyword context)))
                     (org-todo (or (car (+org-get-todo-keywords-for todo-keyword))
                                   'todo)))))

                (t (user-error "Not a valid list, heading or table")))
          (when (org-invisible-p)
            (org-show-hidden-entry)))
      (+org-insert-item 'below))))

;; (defun lattie--in-sub-or-superscript ()
;;   (let ((faces-at-point (get-text-property (1- (point)) 'face)))
;;     (or (and (listp faces-at-point)
;;              (or (memq 'font-latex-superscript-face faces-at-point)
;;                  (memq 'font-latex-subscript-face faces-at-point)))
;;         (memq faces-at-point '(font-latex-subscript-face
;;                                font-latex-superscript-face)))))

(defun lattie--inhibit-in-math-a (&rest _)
  (not (or (not (memq major-mode '(org-mode org-journal-mode)))
           (lattie--math-p)
           (lattie--math-shorthand-p))))
(advice-add #'company-org-roam :before-while #'lattie--inhibit-in-math-a)
(advice-add #'company-yasnippet :before-while #'lattie--inhibit-in-math-a)
(advice-add #'company-dabbrev :before-while #'lattie--inhibit-in-math-a)

(defun special-lattie-space ()
  (interactive)
  (lattie--auto-capitalize)
  (cond ((lattie--math-p)
         (when-let ((beg (cl-third (abbrev--before-point))))
           (expand-abbrev)
           (put-text-property beg
                              (point)
                              'font-lock-face
                              'font-latex-math-face)))
        (t (expand-abbrev)))
  (cond ((org-in-src-block-p t)
         (let ((+org-exit-src-code-hook (remove 'ws-butler-trim-eob-lines
                                                +org-exit-src-code-hook)))
           (if (or (memq (char-before) alphanumerics)
                   (memq (char-after) alphanumerics))
               (call-interactively #'self-insert-command)
             (org-babel-do-in-edit-buffer
              (when-let* ((op (car (sp-point-in-empty-sexp)))
                          (pair (sp-get-pair (car (sp-point-in-empty-sexp))))
                          (insertion-specification (first (--first (equal (second it) "SPC")
                                                                   (plist-get pair :post-handlers)))))
                (eval (sp--parse-insertion-spec insertion-specification)))
              (call-interactively #'self-insert-command)))))
        ((lattie--right-p)
         (lattie-self-insert-command 1)
         (run-at-time 0 nil
                      (lambda () (put-text-property (- (point) 1)
                                                    (1+ (point))
                                                    'font-lock-face
                                                    nil))))
        ((or (and (eq (char-before) ?$)
                  (not (lattie--at-right-side-of-closing-expression?)))
             ;; (eq (char-before) ?,)
             ;; Inside sexp?
             (eq (char-before (second (syntax-ppss))) ?,)
             (not (lattie--math-shorthand-p)))
         (if (and (eq (char-after) ?$)
                  (not (lattie--at-left-side-of-opening-expression?)))
             (let ((faces (get-text-property (point) 'face)))
               (lattie-self-insert-command 1)
               (put-text-property (save-excursion (1+ (re-search-backward lattie-dollar-regexp nil t)))
                                  (1+ (point))
                                  'font-lock-face
                                  '(font-latex-math-face)))
           (lattie-self-insert-command 1)
           (put-text-property (+ (point) (save-excursion
                                           (skip-chars-backward "\t\s\n")))
                              (point)
                              'font-lock-face
                              nil)))
        ((and (eq (char-before) ?\`)
              (lattie--math-shorthand-p))
         (insert (this-command-keys)))
        (t (lattie--apply-math-shorthand-before-point 'self-insert))))

(defun lattie-split-string (string &optional separators omit-nulls keep-sep)
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
    (nreverse list)))

(defun lattie--apply-math-shorthand-before-point (&optional self-insert-p)
  (let* ((s (buffer-substring-no-properties (+ (match-beginning 0) 2)
                                            (match-end 0)))
         (op (if (s-prefix-p "," s)
                 (if (eq (char-after (match-beginning 0))
                         ?\n)
                     "\\["
                   "\n\\[")
               "$"))
         (cl (if (equal op "$")
                 "$"
               "\\]\n")))
    (delete-region (1+ (match-beginning 0)) (match-end 0))
    (when (s-prefix-p "," s)
      (setq s (substring-no-properties s 1)))
    (put-text-property (point)
                       (progn (insert op
                                      (lattie--expand-math-apply s)
                                      (if (or (equal op "$")
                                              (not self-insert-p)
                                              (equal (and (> (length (this-command-keys-vector)) 0)
                                                          (aref (this-command-keys-vector) 0))
                                                     ?\s))
                                          ""
                                        (this-command-keys))
                                      cl)
                              (point))
                       'font-lock-face
                       (list 'font-latex-math-face))
    (when (and self-insert-p
               (equal op "$"))
      (lattie-self-insert-command 1))))

(defun lattie--math-shorthand-p ()
  (looking-back "[\t\s\n],[^\s\t\n]*" (1- (point-at-bol)) t))

;; (progn (lattie--math-shorthand-p)
;;        (lattie--apply-math-shorthand-before-point))

(defvar lattie-arrows)
(setq lattie-arrows
      '((">->" . "\\hookrightarrow")
        ("->" . "")
        (">=" . "\\geq")
        ("<=" . "\\leq")
        ("|->" . "\\mapsto")
        ("~-" . "\\simeq")
        ("~=" . "\\cong")))

(defun lattie--arrow-regexp ()
  (->> lattie-arrows
       (-map #'car)
       (-map #'regexp-quote)
       (s-join "\\|")))

(defun lattie--expand-math-apply (s)
  (->> (lattie-split-string
        s
        (concat (lattie--arrow-regexp)
                "\\|`+.\\|[a-zA-Z0-9]'+.\\|"
                "\\([_^] *\\([^\n\\{}]\\|\
\\\\\\([a-zA-Z@]+\\|[^ \t\n]\\)\\|\\({\\)\\)\\)"
                "\\|[^a-zA-Z0-9\\]")
        t t)
       (-map #'lattie--expand-math)
       (s-join " ")
       (s-replace " : " ": ")
       (s-replace " _ " "_")
       (s-replace " ^ " "^")
       (s-replace "( " "(")
       (s-replace " (" "(")
       (s-replace " )" ")")
       ;; (s-replace "{ " "\\{")
       ;; (s-replace "{ " "{")
       ;; (s-replace " }" "}")
       ;; FIXME This does not work with nested braces like {0, {0 , 1}}
       (replace-regexp-in-string "\\(?: \\|^\\){ \\([^{}]+\\) }\\(?: \\|$\\)"
                                 "\\\\{\\1\\\\}")
       (s-replace "{ " "{")
       (s-replace " }" "}")
       ;; (s-replace " }" "\\}")
       (s-replace " ," ",")
       (s-replace " _" "_")
       (s-replace " / " "/")
       (s-replace " / " "/")
       (s-replace "/ " "/")
       (s-replace " ^" "^")
       (s-replace " \\prime" "\\prime")))

(defun lattie--grave-shorthand (s)
  (eq (aref s 0) ?\`))

(defun lattie-yas-next-field-or-maybe-expand (oldfun &rest args)
  (interactive)
  (expand-abbrev)
  (if (and (memq major-mode '(org-mode org-journal-mode))
           (lattie--math-p))
      (cond ((memq (char-after) '(?} ?\)))
             (org-try-cdlatex-tab))
            ((looking-at "\\\\}")
             (forward-char 2))
            (t (apply oldfun args)))
    (apply oldfun args)))

(advice-add #'yas-next-field-or-maybe-expand
            :around
            #'lattie-yas-next-field-or-maybe-expand)

(defun lattie--quote-shorthand (s)
  (and (>= (length s) 3)
       (eq (aref s 1) ?\')))

(defun lattie--arrow-p (s)
  (alist-get s lattie-arrows
             nil nil #'equal))

;; (lattie--expand-math-apply "a``dB")

(defun lattie--expand-math (s)
  (pcase s
    ("+" "\\plus")
    ((pred lattie--arrow-p)
     (alist-get s lattie-arrows
                nil nil
                #'equal))
    ((pred lattie--grave-shorthand)
     (nth (1- (length s)) (assoc (aref s (1- (length s))) cdlatex-math-symbol-alist-comb)))
    ((pred lattie--quote-shorthand)
     (let* ((mathcmd-with-rules (assoc (aref s (1- (length s))) cdlatex-math-modify-alist-comb))
            (mathcmd (nth 1 mathcmd-with-rules))
            (type (nth 3 mathcmd-with-rules)))
       (if type
           (concat mathcmd "{" (char-to-string (aref s 0)) "}")
         (concat "{" mathcmd " " (char-to-string (aref s 0)) "}"))))
    (_ (or (abbrev-expansion s)
           s))))

;; (lattie--expand-math "a")

;; (lattie--expand-math-apply "a<=b")

(defun special-lattie-punctuation ()
  (interactive)
  (if (and (eq (char-after) ?$)
           (not (lattie--at-left-side-of-opening-expression?)))
      (unless (and (eq (char-before) ?$)
                   (not (lattie--at-right-side-of-closing-expression?)))
        (when (looking-back "[\s\t\n],[a-zA-Z0-9,]+" (point-at-bol) nil)
          (insert "$"
                  (buffer-substring (+ (match-beginning 0) 2)
                                    (match-end 0))
                  "$")
          (delete-region (1+ (match-beginning 0)) (match-end 0))))
    (if (and (eq (char-before) ?$)
             (lattie--at-right-side-of-closing-expression?))
        (progn (lattie-self-insert-command 1)
               (put-text-property (1- (point))
                                  (point)
                                  'font-lock-face
                                  nil))
      (lattie-self-insert-command 1))))

(defun lattie--auto-capitalize ()
  (save-excursion
    (cond ((and (memq major-mode '(org-mode org-journal-mode))
                (bound-and-true-p auto-capitalize-mode)
                (member (progn (beginning-of-line)
                               (and (looking-at "\\s-*_\\([A-Z][a-z\s]+\\)_ ")
                                    (match-string-no-properties 1)))
                        '("Proof"
                          "Proposition"
                          "Theorem"
                          "Definition"
                          "Lemma"
                          "Idea"
                          "Brain dump"
                          "Remark"
                          "Construction"
                          "Corollary")))
           (goto-char (match-end 0))
           (upcase-char 1))
          ;; ((and (> (point-max) 1)
          ;;       (member (buffer-substring-no-properties (point-at-bol)
          ;;                                               (min (+ 2 (point-at-bol)) (point-max)))
          ;;               '("- " "+ ")))
          ;;  (goto-char (+ (point-at-bol) 2))
          ;;  (upcase-char 1))
          )))


(with-eval-after-load 'auto-capitalize
  (defun lattie--auto-capitalize-predicate ()
    (and (or (not (memq major-mode '(org-mode org-journal-mode)))
             (and (not (lattie--math-shorthand-p))
                  (not (lattie--math-p))
                  (not (equal (buffer-substring-no-properties (max (point-min)
                                                                   (- (point) 3))
                                                              (point))
                              "$i$"))))
         (auto-capitalize-default-predicate-function)))

  (setq auto-capitalize-predicate
        #'lattie--auto-capitalize-predicate))

;; (defun special-lattie-minus ()
;;   (interactive)
;;   (when (looking-back ",[a-zA-Z0-9]+" (point-at-bol) nil)
;;     (insert "$"
;;             (buffer-substring (1+ (match-beginning 0)) (match-end 0))
;;             "$")
;;     (delete-region (match-beginning 0) (match-end 0)))
;;   (if (fboundp 'typo-cycle-dashes)
;;       (typo-cycle-dashes 1)
;;     (lattie-self-insert-command 1)))

(defun special-lattie-toggle-latex-fragment ()
  (interactive)
  (if (and (or (lattie--special-p)
               (and (> (point) 1)
                    (prog2 (forward-char -1)
                        (and (lattie--special-p)
                             (not (looking-back lattie-opening-environment-regexp)))
                      (forward-char 1))))
           (not (or (eq (char-before (1- (point))) ?$)
                    (and (eq (char-before) ?$)
                         (eq (char-after) ?$)))))
      (progn (org-toggle-latex-fragment)
             (when (and (fboundp 'exwm-randr-refresh)
                        (not (eq last-command
                                 #'special-lattie-toggle-latex-fragment)))
               (run-at-time 0.1 nil #'exwm-randr-refresh)))
    (lattie-self-insert-command 1)))

(defun special-lattie-digit ()
  (interactive)
  (if (or (lattie--special-p)
          ;; (and (eq major-mode 'org-mode)
          ;;      (org-at-heading-or-item-p))
          )
      (call-interactively #'digit-argument)
    (lattie-self-insert-command 1)))

(defun special-lattie-digit-or-bol (arg)
  (interactive "P")
  (if (or (lattie--special-p)
          ;; (and (eq major-mode 'org-mode)
          ;;      (org-at-heading-or-item-p))
          )
      (if arg
          (call-interactively #'digit-argument)
        (if (eq major-mode '(org-mode org-journal-mode))
            (org-beginning-of-line)
          (back-to-indentation)))
    (lattie-self-insert-command (or arg 1))))

(defun special-lattie-compile ()
  (interactive)
  (if (lattie--special-p)
      (ignore-errors (org-export-dispatch))
    (lattie-self-insert-command 1)))

(with-eval-after-load 'transform
  (defun lattie-transform-char-before-previous ()
    "Transform char before point.

If previous char is “/” or “_”, apply ‘transform-accent-previous-char’
instead."
    (interactive)
    (if (member (char-before (1- (point))) (mapcar #'car transform-accent-list))
        (transform-accent-previous-char)
      (if-let ((c (transform--get-variant-list (char-before (1- (point))))))
          (let* ((index (car c))
                 (variant-list (cdr c))
                 (step-fn (transform--make-step-fn variant-list index))
                 (map (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "C-n")
                          (lambda () (interactive) (save-excursion (forward-char -1)
                                                                   (funcall step-fn 1))))
                        (define-key map (kbd "C-p")
                          (lambda () (interactive) (save-excursion (forward-char -1)
                                                                   (funcall step-fn -1))))
                        (when (not (string-empty-p (this-command-keys)))
                          (define-key map (this-command-keys)
                            (lambda () (interactive) (save-excursion (forward-char -1)
                                                                     (funcall step-fn 1)))))
                        map)))
            (save-excursion (forward-char -1)
                            (funcall step-fn 1))
            (set-transient-map map t))
        (user-error "No variant found"))))

  (defun lattie-org-todo (&optional arg)
    (interactive "P")
    (if (lattie--math-p)
        (if (eq (char-before) ?\s)
            (lattie-transform-char-before-previous)
          (transform-previous-char))
      (funcall #'org-todo arg))))

(defun lattie-fill-region-as-paragraph (from to &optional justify
                                             nosqueeze squeeze-after)
  "Fill the region as one paragraph.
It removes any paragraph breaks in the region and extra newlines at the end,
indents and fills lines between the margins given by the
`current-left-margin' and `current-fill-column' functions.
\(In most cases, the variable `fill-column' controls the width.)
It leaves point at the beginning of the line following the paragraph.

Normally performs justification according to the `current-justification'
function, but with a prefix arg, does full justification instead.

From a program, optional third arg JUSTIFY can specify any type of
justification.  Fourth arg NOSQUEEZE non-nil means not to make spaces
between words canonical before filling.  Fifth arg SQUEEZE-AFTER, if non-nil,
means don't canonicalize spaces before that position.

Return the `fill-prefix' used for filling.

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (region-beginning) (region-end)
                       (if current-prefix-arg 'full))))
  (unless (memq justify '(t nil none full center left right))
    (setq justify 'full))

  ;; Make sure "to" is the endpoint.
  (goto-char (min from to))
  (setq to (max from to))
  ;; Ignore blank lines at beginning of region.
  (skip-chars-forward " \t\n")

  (let ((from-plus-indent (point))
        (oneleft nil))

    (beginning-of-line)
    ;; We used to round up to whole line, but that prevents us from
    ;; correctly handling filling of mixed code-and-comment where we do want
    ;; to fill the comment but not the code.  So only use (point) if it's
    ;; further than `from', which means that `from' is followed by some
    ;; number of empty lines.
    (setq from (max (point) from))

    ;; Delete all but one soft newline at end of region.
    ;; And leave TO before that one.
    (goto-char to)
    (while (and (> (point) from) (eq ?\n (char-after (1- (point)))))
      (if (and oneleft
               (not (and use-hard-newlines
                         (get-text-property (1- (point)) 'hard))))
          (delete-char -1)
        (backward-char 1)
        (setq oneleft t)))
    (setq to (copy-marker (point) t))
    ;; ;; If there was no newline, and there is text in the paragraph, then
    ;; ;; create a newline.
    ;; (if (and (not oneleft) (> to from-plus-indent))
    ;; 	(newline))
    (goto-char from-plus-indent))

  (if (not (> to (point)))
      nil ;; There is no paragraph, only whitespace: exit now.

    (or justify (setq justify (current-justification)))

    ;; Don't let Adaptive Fill mode alter the fill prefix permanently.
    (let ((fill-prefix fill-prefix))
      ;; Figure out how this paragraph is indented, if desired.
      (when (and adaptive-fill-mode
                 (or (null fill-prefix) (string= fill-prefix "")))
        (setq fill-prefix (fill-context-prefix from to))
        ;; Ignore a white-space only fill-prefix
        ;; if we indent-according-to-mode.
        (when (and fill-prefix fill-indent-according-to-mode
                   (string-match "\\`[ \t]*\\'" fill-prefix))
          (setq fill-prefix nil)))

      (goto-char from)
      (beginning-of-line)

      (if (not justify)               ; filling disabled: just check indentation
          (progn
            (goto-char from)
            (while (< (point) to)
              (if (and (not (eolp))
                       (< (current-indentation) (current-left-margin)))
                  (fill-indent-to-left-margin))
              (forward-line 1)))

        (if use-hard-newlines
            (remove-list-of-text-properties from to '(hard)))
        ;; Make sure first line is indented (at least) to left margin...
        (if (or (memq justify '(right center))
                (< (current-indentation) (current-left-margin)))
            (fill-indent-to-left-margin))
        ;; Delete the fill-prefix from every line.
        (fill-delete-prefix from to fill-prefix)
        (setq from (point))

        ;; FROM, and point, are now before the text to fill,
        ;; but after any fill prefix on the first line.

        (fill-delete-newlines from to justify nosqueeze squeeze-after)

        ;; This is the actual filling loop.
        (goto-char from)
        (let (linebeg)
          (while (< (point) to)
            (setq linebeg (point))
            (move-to-column (current-fill-column))
            (when (and (bound-and-true-p org-cdlatex-mode)
                       (lattie--math-p))
              (lattie-forward 1 t))
            (if (when (< (point) to)
                  ;; Find the position where we'll break the line.
                  ;; Use an immediately following space, if any.
                  ;; However, note that `move-to-column' may overshoot
                  ;; if there are wide characters (Bug#3234).
                  (unless (> (current-column) (current-fill-column))
                    (forward-char 1))
                  (fill-move-to-break-point linebeg)
                  ;; Check again to see if we got to the end of
                  ;; the paragraph.
                  (skip-chars-forward " \t")
                  (< (point) to))
                ;; Found a place to cut.
                (progn
                  (when (and (bound-and-true-p org-cdlatex-mode)
                             (lattie--math-p))
                    (lattie-forward 1 t))
                  (fill-newline)
                  (when justify
                    ;; Justify the line just ended, if desired.
                    (save-excursion
                      (forward-line -1)
                      (justify-current-line justify nil t))))

              (goto-char to)
              ;; Justify this last line, if desired.
              (if justify (justify-current-line justify t t))))))
      ;; Leave point after final newline.
      (goto-char to)
      (unless (eobp) (forward-char 1))
      ;; Return the fill-prefix we used
      fill-prefix)))

(defun lattie-do-auto-fill ()
  "The default value for `normal-auto-fill-function'.
This is the default auto-fill function, some major modes use a different one.
Returns t if it really did any work."
  (let (fc justify give-up
           (fill-prefix fill-prefix))
    (if (or (not (setq justify (current-justification)))
            (null (setq fc (current-fill-column)))
            (and (eq justify 'left)
                 (<= (current-column) fc))
            (and auto-fill-inhibit-regexp
                 (save-excursion (beginning-of-line)
                                 (looking-at auto-fill-inhibit-regexp))))
        nil ;; Auto-filling not required
      (if (memq justify '(full center right))
          (save-excursion (unjustify-current-line)))

      ;; Choose a fill-prefix automatically.
      (when (and adaptive-fill-mode
                 (or (null fill-prefix) (string= fill-prefix "")))
        (let ((prefix
               (fill-context-prefix
                (save-excursion (fill-forward-paragraph -1) (point))
                (save-excursion (fill-forward-paragraph 1) (point)))))
          (and prefix (not (equal prefix ""))
               ;; Use auto-indentation rather than a guessed empty prefix.
               (not (and fill-indent-according-to-mode
                         (string-match "\\`[ \t]*\\'" prefix)))
               (setq fill-prefix prefix))))

      (while (and (not give-up) (> (current-column) fc))
        ;; Determine where to split the line.
        (let ((fill-point
               (save-excursion
                 (beginning-of-line)
                 ;; Don't split earlier in the line than the length of the
                 ;; fill prefix, since the resulting line would be longer.
                 (when fill-prefix
                   (move-to-column (string-width fill-prefix)))
                 (let ((after-prefix (point)))
                   (move-to-column (1+ fc))
                   (fill-move-to-break-point after-prefix)
                   (when (and (bound-and-true-p org-cdlatex-mode)
                              (lattie--math-p))
                     (lattie-forward 1 t))
                   (point)))))

          ;; See whether the place we found is any good.
          (if (save-excursion
                (goto-char fill-point)
                (or (bolp)
                    ;; There is no use breaking at end of line.
                    (save-excursion (skip-chars-forward " ") (eolp))
                    ;; Don't split right after a comment starter
                    ;; since we would just make another comment starter.
                    (and comment-start-skip
                         (let ((limit (point)))
                           (beginning-of-line)
                           (and (re-search-forward comment-start-skip
                                                   limit t)
                                (eq (point) limit))))))
              ;; No good place to break => stop trying.
              (setq give-up t)
            ;; Ok, we have a useful place to break the line.  Do it.
            (let ((prev-column (current-column)))
              ;; If point is at the fill-point, do not `save-excursion'.
              ;; Otherwise, if a comment prefix or fill-prefix is inserted,
              ;; point will end up before it rather than after it.
              (if (save-excursion
                    (skip-chars-backward " \t")
                    (= (point) fill-point))
                  (default-indent-new-line t)
                (save-excursion
                  (goto-char fill-point)
                  (default-indent-new-line t)))
              ;; Now do justification, if required
              (if (not (eq justify 'left))
                  (save-excursion
                    (end-of-line 0)
                    (justify-current-line justify nil t)))
              ;; If making the new line didn't reduce the hpos of
              ;; the end of the line, then give up now;
              ;; trying again will not help.
              (if (>= (current-column) prev-column)
                  (setq give-up t))))))
      ;; Justify last line.
      (justify-current-line justify t t)
      t)))

(advice-add #'fill-region-as-paragraph :override #'lattie-fill-region-as-paragraph)
(advice-add #'do-auto-fill :override #'lattie-do-auto-fill)

(with-eval-after-load 'evil-org
  (evil-define-key* 'insert evil-org-mode-map "<return>" #'special-lattie-newline-and-indent))

(provide 'lattie)


;; Local Variables:
;; eval: (add-hook! 'after-save-hook :local (byte-compile-file (buffer-file-name (current-buffer)) t))
;; End:
