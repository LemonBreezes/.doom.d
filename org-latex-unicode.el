;;; ~/.doom.d/org-latex-unicode.el -*- lexical-binding: t; -*-
(require 'dash)

;; Old method with DeclareUnicodeCharacter
;; (defun lattie-unicode--latex-header-entry (unicode-abbrev-sequence)
;;   (declare (pure t) (side-effect-free t))
;;   (let ((hex (->> unicode-abbrev-sequence
;;                 (car)
;;                 (format "%x")
;;                 (s-upcase)))
;;         (math-command (second unicode-abbrev-sequence)))
;;     (when (< (length hex) 5)
;;       ;; Pad hex with 0's to 5 digits
;;       (setq hex (concat (make-string (- 5 (length hex))
;;                                      ?0)
;;                         hex)))
;;     (concat "\\DeclareUnicodeCharacter{" hex "}{" math-command "}")))

(defun lattie-unicode--latex-header-entry (unicode-abbrev-sequence)
  (declare (pure t) (side-effect-free t))
  (let ((char (char-to-string (car unicode-abbrev-sequence)))
        (math-command (second unicode-abbrev-sequence)))
    (concat "\\newunicodechar" "{" char "}{" math-command "}")))

;; (lattie-unicode--latex-header-entry '(?𝒜 "\\mathscr{A}" "sa"))

(defun lattie-unicode--abbrev-entry (unicode-abbrev-sequence)
  (declare (pure t) (side-effect-free t))
  (when (< 2 (length unicode-abbrev-sequence))
    (let ((abbrev-output (->> unicode-abbrev-sequence
                              (car)
                              (char-to-string)))
          ;; Only list valid abbrev entries (ones with only alphabetical characters)
          (abbrev-inputs (nthcdr 2 unicode-abbrev-sequence)))
      (cl-loop for abbrev-input in abbrev-inputs
               collect (list abbrev-input abbrev-output nil 0)))))

;; (lattie-unicode--abbrev-entry (third lattie-unicode-abbrev-sequences))
;; (lattie-unicode--abbrev-entry '(? "\\rightarrow" "ar"))

(defun lattie-unicode--abbrev-table (unicode-abbrev-sequences)
  (declare (pure t) (side-effect-free t))
  (-flatten-n 1 (cl-loop for sequence in unicode-abbrev-sequences
                         collect (lattie-unicode--abbrev-entry sequence))))

;; (lattie-unicode--abbrev-table
;;  '((?≤ "\\leq" "leq")
;;    (?∗ "*")
;;    (?𝒜 "\\mathscr{A}" "sa")
;;    (?𝒞 "\\mathscr{C}" "sc")
;;    (?𝒟 "\\mathscr{D}" "sd")
;;    (? "\\rightarrow" "ar")
;;    (?⋄ "\\diamond" "dmnd" "diamond")
;;    (?𝛼 "\\alpha" "ga")))

(defun lattie--math-or-math-shorthand-p ()
  (or (lattie--math-p)
      (lattie--math-shorthand-p)))

(defun lattie-not-math-shorthand-nor-math-p ()
  (not (lattie--not-math-shorthand-p)))

(defun lattie-unicode--create-abbrev-tables (unicode-abbrev-sequences &optional extra-abbrevs)
  (when (boundp 'math-abbrev-table)
    (clear-abbrev-table math-abbrev-table))
  (when (boundp 'not-math-abbrev-table)
    (clear-abbrev-table not-math-abbrev-table))
  (define-abbrev-table 'math-abbrev-table
    (nconc (lattie-unicode--abbrev-table unicode-abbrev-sequences)
           (--map (list (first it) (second it) nil 0)
                  extra-abbrevs))
    :enable-function #'lattie--math-or-math-shorthand-p
    :regexp "\\(?:[^a-zA-Z0-9-<>|\\]+\\)\\([a-zA-Z0-9-<>=|]+\\)"
    :case-fixed t)
  (define-abbrev-table 'not-math-abbrev-table
    (--map (list (first it) (third it) nil 0)
           (--filter (> (length it) 2)
                     extra-abbrevs))
    :enable-function #'lattie-not-math-shorthand-nor-math-p
    :regexp "\\(?:[^a-zA-Z0-9-<>|\\]+\\)\\([a-zA-Z0-9-<>=|]+\\)"
    :case-fixed t)
  (if (assoc 'org-cdlatex-mode abbrev-minor-mode-table-alist)
      (setf (cdr (assoc 'org-cdlatex-mode abbrev-minor-mode-table-alist))
            (list math-abbrev-table
                  not-math-abbrev-table))
    (setq abbrev-minor-mode-table-alist
          (cons (cons 'org-cdlatex-mode (list math-abbrev-table
                                              not-math-abbrev-table))
                abbrev-minor-mode-table-alist))))

;; (lattie-unicode--special-abbrev-entries '(? "\\rightarrow" "ar" "->"))

(defun lattie-unicode--expand-abbrev ()
  (if (and (memq major-mode '(org-mode
                              org-journal-mode))
           (not (memq (char-before) alphabet)))
      (abbrev--default-expand)))

(defun lattie-unicode--latex-header (unicode-abbrev-sequences &optional old-latex-header extra-latex-header)
  (declare (pure t) (side-effect-free t))
  (concat (or old-latex-header "")
          (string-join (-map #'lattie-unicode--latex-header-entry unicode-abbrev-sequences)
                       "\n")
          extra-latex-header))

;; (lattie-unicode--latex-header lattie-unicode-abbrev-sequences org-format-latex-header)

(defvar lattie--latex-header-file (expand-file-name "unicode-latex.tex" doom-private-dir))
(defvar lattie--org-format-latex-header-backup org-format-latex-header)

(defun lattie-unicode--update-header-and-abbrevs (unicode-abbrev-sequences
                                                  &optional extra-abbrevs
                                                  extra-latex-header)
  "This function Creates new LaTeX header for Org mode from
backup and creates an Abbrev table for Org CDLaTeX mode."
  (setq org-format-latex-header
        (lattie-unicode--latex-header unicode-abbrev-sequences
                                      lattie--org-format-latex-header-backup
                                      extra-latex-header))
  (with-temp-file lattie--latex-header-file
    (erase-buffer)
    (insert (lattie-unicode--latex-header unicode-abbrev-sequences
                                          nil
                                          extra-latex-header)))
  (lattie-unicode--create-abbrev-tables unicode-abbrev-sequences
                                        extra-abbrevs))

(lattie-unicode--update-header-and-abbrevs
 '((?≤ "\\leq" "leq")
   (?≥ "\\geq" "geq")
   (?≠ "\\neq" "neq")
   (?≅ "\\cong" "iso" "cong")
   (?≃ "\\simeq" "heq" "simeq")
   (?≣ "\\equiv" "equiv")
   (?⟺ "\\Leftrightarrow" "iff")
   (?∗ "*")

   (?𝒜 "\\mathscr{A}" "sa")
   (?𝒞 "\\mathscr{C}" "sc")
   (?𝒟 "\\mathscr{D}" "sd")
   (?𝒢 "\\mathscr{G}" "sg")
   (?𝒥 "\\mathscr{J}" "sj")
   (?𝒦 "\\mathscr{K}" "sk")
   (?𝒩 "\\mathscr{N}" "sn")
   (?𝒪 "\\mathscr{O}" "so")
   (?𝒫 "\\mathscr{P}" "sp")
   (?𝒬 "\\mathscr{Q}" "sq")
   (?𝒮 "\\mathscr{S}" "ss")
   (?𝒯 "\\mathscr{T}" "st")
   (?𝒰 "\\mathscr{U}" "su")
   (?𝒱 "\\mathscr{V}" "sv")
   (?𝒲 "\\mathscr{W}" "sw")
   (?𝒳 "\\mathscr{X}" "sx")
   (?𝒴 "\\mathscr{Y}" "sy")
   (?𝒵 "\\mathscr{Z}" "sz")

   (?𝔸 "\\mathbb{A}" "wa")
   (?𝔹 "\\mathbb{B}" "wb")
   (?ℂ "\\mathbb{C}" "wc")
   (?𝔻 "\\mathbb{D}" "wd")
   (?𝔼 "\\mathbb{E}" "we")
   (?𝔽 "\\mathbb{F}" "wf")
   (?𝔾 "\\mathbb{G}" "wg")
   (?ℍ "\\mathbb{H}" "wh")
   (?𝕀 "\\mathbb{I}" "wi")
   (?𝕁 "\\mathbb{J}" "wj")
   (?𝕂 "\\mathbb{K}" "wk")
   (?𝕃 "\\mathbb{L}" "wl")
   (?𝕄 "\\mathbb{M}" "wm")
   (?ℕ "\\mathbb{N}" "wn")
   (?𝕆 "\\mathbb{O}" "wo")
   (?ℙ "\\mathbb{P}" "wp")
   (?ℚ "\\mathbb{Q}" "wq")
   (?ℝ "\\mathbb{R}" "wr")
   (?𝕊 "\\mathbb{S}" "ws")
   (?𝕋 "\\mathbb{T}" "wt")
   (?𝕌 "\\mathbb{U}" "wu")
   (?𝕍 "\\mathbb{V}" "wv")
   (?𝕎 "\\mathbb{W}" "ww")
   (?𝕏 "\\mathbb{X}" "wx")
   (?𝕐 "\\mathbb{Y}" "wy")
   (?ℤ "\\mathbb{Z}" "wz")

   (?𝐀 "\\mathbf{A}" "ba")
   (?𝐁 "\\mathbf{B}" "bb")
   (?𝐂 "\\mathbf{C}" "bc")
   (?𝐃 "\\mathbf{D}" "bd")
   (?𝐄 "\\mathbf{E}" "be")
   (?𝐅 "\\mathbf{F}" "bf")
   (?𝐆 "\\mathbf{G}" "bg")
   (?𝐇 "\\mathbf{H}" "bh")
   (?𝐈 "\\mathbf{I}" "bi")
   (?𝐉 "\\mathbf{J}" "bj")
   (?𝐊 "\\mathbf{K}" "bk")
   (?𝐋 "\\mathbf{L}" "bl")
   (?𝐌 "\\mathbf{M}" "bm")
   (?𝐍 "\\mathbf{N}" "bn")
   (?𝐎 "\\mathbf{O}" "bo")
   (?𝐏 "\\mathbf{P}" "bp")
   (?𝐐 "\\mathbf{Q}" "bq")
   (?𝐑 "\\mathbf{R}" "br")
   (?𝐒 "\\mathbf{S}" "bs")
   (?𝐓 "\\mathbf{T}" "bt")
   (?𝐔 "\\mathbf{U}" "bu")
   (?𝐕 "\\mathbf{V}" "bv")
   (?𝐖 "\\mathbf{W}" "bw")
   (?𝐗 "\\mathbf{X}" "bx")
   (?𝐘 "\\mathbf{Y}" "by")
   (?𝐙 "\\mathbf{Z}" "bz")

   (?𝔄 "\\mathfrak{A}" "fa")
   (?𝔅 "\\mathfrak{B}" "fb")
   (?ℭ "\\mathfrak{C}" "fc")
   (?𝔇 "\\mathfrak{D}" "fd")
   (?𝔈 "\\mathfrak{E}" "fe")
   (?𝔉 "\\mathfrak{F}" "ff")
   (?𝔊 "\\mathfrak{G}" "fg")
   (?𝔍 "\\mathfrak{J}" "fj")
   (?𝔎 "\\mathfrak{K}" "fk")
   (?𝔏 "\\mathfrak{L}" "fl")
   (?𝔐 "\\mathfrak{M}" "fm")
   (?𝔑 "\\mathfrak{N}" "fn")
   (?𝔒 "\\mathfrak{O}" "fo")
   (?𝔓 "\\mathfrak{P}" "fp")
   (?𝔔 "\\mathfrak{Q}" "fq")
   (?𝔖 "\\mathfrak{S}" "fs")
   (?𝔗 "\\mathfrak{T}" "ft")
   (?𝔘 "\\mathfrak{U}" "fu")
   (?𝔙 "\\mathfrak{V}" "fv")
   (?𝔚 "\\mathfrak{W}" "fw")
   (?𝔛 "\\mathfrak{X}" "fx")
   (?𝔜 "\\mathfrak{Y}" "fy")
   (?𝔞 "\\mathfrak{a}" "fA")
   (?𝔟 "\\mathfrak{b}" "fB")
   (?𝔠 "\\mathfrak{c}" "fC")
   (?𝔡 "\\mathfrak{d}" "fD")
   (?𝔢 "\\mathfrak{e}" "fE")
   (?𝔣 "\\mathfrak{f}" "fF")
   (?𝔤 "\\mathfrak{g}" "fG")
   (?𝔥 "\\mathfrak{h}" "fH")
   (?𝔦 "\\mathfrak{i}" "fI")
   (?𝔧 "\\mathfrak{j}" "fJ")
   (?𝔨 "\\mathfrak{k}" "fK")
   (?𝔩 "\\mathfrak{l}" "fL")
   (?𝔪 "\\mathfrak{m}" "fM")
   (?𝔫 "\\mathfrak{n}" "fN")
   (?𝔬 "\\mathfrak{o}" "fO")
   (?𝔭 "\\mathfrak{p}" "fP")
   (?𝔮 "\\mathfrak{q}" "fQ")
   (?𝔯 "\\mathfrak{r}" "fR")
   (?𝔰 "\\mathfrak{s}" "fS")
   (?𝔱 "\\mathfrak{t}" "fT")
   (?𝔲 "\\mathfrak{u}" "fU")
   (?𝔳 "\\mathfrak{v}" "fV")
   (?𝔴 "\\mathfrak{w}" "fW")
   (?𝔵 "\\mathfrak{x}" "fX")
   (?𝔶 "\\mathfrak{y}" "fY")
   (?𝔷 "\\mathfrak{z}" "fZ")

   (?𝓐 "\\mathcal{A}" "ca")
   (?𝓑 "\\mathcal{B}" "cb")
   (?𝓒 "\\mathcal{C}" "cc")
   (?𝓓 "\\mathcal{D}" "cd")
   (?𝓔 "\\mathcal{E}" "ce")
   (?𝓕 "\\mathcal{F}" "cf")
   (?𝓖 "\\mathcal{G}" "cg")
   (?𝓗 "\\mathcal{H}" "ch")
   (?𝓘 "\\mathcal{I}" "ci")
   (?𝓙 "\\mathcal{J}" "cj")
   (?𝓚 "\\mathcal{K}" "ck")
   (?𝓛 "\\mathcal{L}" "cl")
   (?𝓜 "\\mathcal{M}" "cm")
   (?𝓝 "\\mathcal{N}" "cn")
   (?𝓞 "\\mathcal{O}" "co")
   (?𝓟 "\\mathcal{P}" "cp")
   (?𝓠 "\\mathcal{Q}" "cq")
   (?𝓡 "\\mathcal{R}" "cr")
   (?𝓢 "\\mathcal{S}" "cs")
   (?𝓣 "\\mathcal{T}" "ct")
   (?𝓤 "\\mathcal{U}" "cu")
   (?𝓥 "\\mathcal{V}" "cv")
   (?𝓦 "\\mathcal{W}" "cw")
   (?𝓧 "\\mathcal{X}" "cx")
   (?𝓨 "\\mathcal{Y}" "cy")
   (?𝓩 "\\mathcal{Z}" "cz")

   (?𝛼 "\\alpha" "ga")
   (?𝛽 "\\beta" "gb")
   (?𝛿 "\\delta" "gd")
   (?𝜀 "\\varepsilon" "ge")
   (?𝜙 "\\phi" "gf")
   (?𝛾 "\\gamma" "gg")
   (?𝜂 "\\eta" "gh")
   (?𝜅 "\\kappa" "gk")
   (?λ "\\lambda" "gl")
   (?𝜇 "\\mu" "gm")
   (?𝜈 "\\nu" "gn")
   (?𝜔 "\\omega" "go")
   (?π "\\pi" "gp")
   (?𝜃 "\\theta" "gq")
   (?𝜌 "\\rho" "gr")
   (?𝜎 "\\sigma" "gs")
   (?𝜏 "\\tau" "gt")
   (?𝜐 "\\upsilon" "gu")
   (?𝜉 "\\xi" "gw")
   (?𝜒 "\\chi" "gx")
   (?𝛹 "\\psi" "gy")
   (?𝜁 "\\zeta" "gz")

   (?Δ "\\Delta" "gD")
   (?Φ "\\Phi" "gF")
   (?Γ "\\Gamma" "gG")
   (?Λ "\\Lambda" "gL")
   (?Ω "\\Omega" "gO")
   (?Π "\\Pi" "gP")
   (?Θ "\\Theta" "gQ")
   (?Σ "\\Sigma" "gS")
   (?Υ "\\Upsilon" "gU")
   (?Ξ "\\Xi" "gW")
   (?Ψ "\\Psi" "gY")

   (?∞ "\\infty" "infty")
   (?𝜕 "\\partial" "ggd" "pp")
   (?𝜖 "\\epsilon" "gge")
   (?𝜑 "\\varphi" "ggf")

   (?ℓ "\\ell" "ell")
   (?∫ "\\int" "int")
   (?⨖ "\\sqint" "sqint")
   (?∮ "\\oint" "oint")
   (?⨑ "\\varointclockwise" "ointclockwise")
   (?∱ "\\varointctrclockwise" "ointclockwise")
   (?∬ "\\iint" "iint")
   (?∭ "\\iiint" "iiint")
   (?⨌ "\\iiint" "iiiint")
   (?∈ "\\in" "in")
   (?⊗ "\\otimes" "ot")
   (?× "\\times" "ox")
   (?∘ "\\circ" "oc")
   (?⮽ "\\boxtimes" "boxtimes" "boxprod")
   (?⧄ "\\squarediv" "lift" "lifts")
   (?⋯ "\\cdots" "cdots")
   (?… "\\dots" "dots")
   (?· "\\cdot" "cdot")
   (?⋄ "\\diamond" "dmnd" "diamond")
   (?ꞏ "\\bullet" "blt" "bullet")
   (?∅ "\\varnothing" "empty")
   (?⊆ "\\subseteq" "sse")
   (?∐ "\\coprod" "coprod")
   (?⨿ "\\amalg" "amalg" "cop")
   (?∀ "\\forall" "forall" "fa")
   (?♭ "\\flat" "flat" "flt")
   (?♯ "\\sharp" "sharp" "shrp")
   (?⋔ "\\pitchfork" "cotens" "cotns" "ctns" "cotensor" "pitchfork")
   (?∪ "\\cup" "union" "cup")

   (?▷ "\\vartriangleright" "|>")
   (? "\\rightarrow" "->")
   (? "\\longrightarrow" "-->")
   (?⟹ "\\Rightarrow" "=>")
   (? "\\twoheadrightarrow" "->>")
   (? "\\rightarrowtail" ">->")
   (?⟼ "\\mapsto" "|->"))
 '(("geq" "\\geq")
   ("sk" "\\Sk")
   ("ob" "\\Ob")
   ("set" "\\Set")
   ;; ("sset" "\\sSet")
   ("hom" "\\Hom")
   ("uhom" "\\uHom")
   ("ima" "\\Ima")
   ("id" "\\id")
   ("ar" "\\ar")
   ("psh" "\\Psh")
   ("rfib" "\\RFib")
   ("hc" "\\hc")
   ("path" "\\Path")
   ("grp" "\\Group")
   ("cat" "\\Cat")
   ("N" "\\N")
   ("and" "\\textnormal{ and }")
   ("if" "\\textnormal{ if }")
   ("for" "\\textnormal{ for }")
   ("scat" "\\Cat_{Δ}" "simplicial category")
   ("sset" "\\Set_{Δ}" "simplicial set")
   )
 (string-join '("\\DeclareMathOperator{\\Hom}{Hom}"
                "\\DeclareMathOperator{\\sSet}{sSet}"
                "\\DeclareMathOperator{\\Ob}{Ob}"
                "\\DeclareMathOperator{\\Cat}{Cat}"
                "\\DeclareMathOperator{\\Set}{Set}"
                "\\DeclareMathOperator{\\Sk}{Sk}"
                "\\DeclareMathOperator{\\N}{N}"
                "\\DeclareMathOperator{\\Ima}{Im}"
                "\\DeclareMathOperator{\\id}{id}"
                "\\DeclareMathOperator{\\RFib}{RFib}"
                "\\DeclareMathOperator{\\Psh}{Psh}"
                "\\DeclareMathOperator{\\hc}{hc}"
                "\\DeclareMathOperator{\\Path}{Path}"
                "\\DeclareMathOperator{\\Group}{Group}"
                "\\DeclareMathOperator{\\uHom}{\\underline{Hom\\kern-.05em}\\kern.1em\}"
                "\\newlength\\squareheight
  \\setlength\\squareheight{6.75pt}"
                "\\newcommand\\squareslash{\\tikz{\\draw (0,0) rectangle (\\squareheight,\\squareheight);\\draw(0,0) -- (\\squareheight,\\squareheight)}}
  \\DeclareMathOperator\\squarediv{\\>\\squareslash\\>}"
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
                "\\makeatother")
              "\n"))

;; (lattie-unicode--create-abbrev-tables
;;  '((?≤ "\\leq" "leq")
;;    (?≥ "\\geq" "geq")
;;    (?≠ "\\neq" "neq")
;;    (?≅ "\\cong" "iso" "cong")
;;    (?≃ "\\simeq" "heq" "simeq")
;;    (?≣ "\\equiv" "equiv")
;;    (?⟺ "\\Leftrightarrow" "iff")
;;    (?∗ "*")

;;    (?𝒜 "\\mathscr{A}" "sa")
;;    (?𝒞 "\\mathscr{C}" "sc")
;;    (?𝒟 "\\mathscr{D}" "sd")
;;    (?𝒢 "\\mathscr{G}" "sg")
;;    (?𝒥 "\\mathscr{J}" "sj")
;;    (?𝒦 "\\mathscr{K}" "sk")
;;    (?𝒩 "\\mathscr{N}" "sn")
;;    (?𝒪 "\\mathscr{O}" "so")
;;    (?𝒫 "\\mathscr{P}" "sp")
;;    (?𝒬 "\\mathscr{Q}" "sq")
;;    (?𝒮 "\\mathscr{S}" "ss")
;;    (?𝒯 "\\mathscr{T}" "st")
;;    (?𝒰 "\\mathscr{U}" "su")
;;    (?𝒱 "\\mathscr{V}" "sv")
;;    (?𝒲 "\\mathscr{W}" "sw")
;;    (?𝒳 "\\mathscr{X}" "sx")
;;    (?𝒴 "\\mathscr{Y}" "sy")
;;    (?𝒵 "\\mathscr{Z}" "sz")

;;    (?𝔸 "\\mathbb{A}" "wa")
;;    (?𝔹 "\\mathbb{B}" "wb")
;;    (?ℂ "\\mathbb{C}" "wc")
;;    (?𝔻 "\\mathbb{D}" "wd")
;;    (?𝔼 "\\mathbb{E}" "we")
;;    (?𝔽 "\\mathbb{F}" "wf")
;;    (?𝔾 "\\mathbb{G}" "wg")
;;    (?ℍ "\\mathbb{H}" "wh")
;;    (?𝕀 "\\mathbb{I}" "wi")
;;    (?𝕁 "\\mathbb{J}" "wj")
;;    (?𝕂 "\\mathbb{K}" "wk")
;;    (?𝕃 "\\mathbb{L}" "wl")
;;    (?𝕄 "\\mathbb{M}" "wm")
;;    (?ℕ "\\mathbb{N}" "wn")
;;    (?𝕆 "\\mathbb{O}" "wo")
;;    (?ℙ "\\mathbb{P}" "wp")
;;    (?ℚ "\\mathbb{Q}" "wq")
;;    (?ℝ "\\mathbb{R}" "wr")
;;    (?𝕊 "\\mathbb{S}" "ws")
;;    (?𝕋 "\\mathbb{T}" "wt")
;;    (?𝕌 "\\mathbb{U}" "wu")
;;    (?𝕍 "\\mathbb{V}" "wv")
;;    (?𝕎 "\\mathbb{W}" "ww")
;;    (?𝕏 "\\mathbb{X}" "wx")
;;    (?𝕐 "\\mathbb{Y}" "wy")
;;    (?ℤ "\\mathbb{Z}" "wz")

;;    (?𝐀 "\\mathbf{A}" "ba")
;;    (?𝐁 "\\mathbf{B}" "bb")
;;    (?𝐂 "\\mathbf{C}" "bc")
;;    (?𝐃 "\\mathbf{D}" "bd")
;;    (?𝐄 "\\mathbf{E}" "be")
;;    (?𝐅 "\\mathbf{F}" "bf")
;;    (?𝐆 "\\mathbf{G}" "bg")
;;    (?𝐇 "\\mathbf{H}" "bh")
;;    (?𝐈 "\\mathbf{I}" "bi")
;;    (?𝐉 "\\mathbf{J}" "bj")
;;    (?𝐊 "\\mathbf{K}" "bk")
;;    (?𝐋 "\\mathbf{L}" "bl")
;;    (?𝐌 "\\mathbf{M}" "bm")
;;    (?𝐍 "\\mathbf{N}" "bn")
;;    (?𝐎 "\\mathbf{O}" "bo")
;;    (?𝐏 "\\mathbf{P}" "bp")
;;    (?𝐐 "\\mathbf{Q}" "bq")
;;    (?𝐑 "\\mathbf{R}" "br")
;;    (?𝐒 "\\mathbf{S}" "bs")
;;    (?𝐓 "\\mathbf{T}" "bt")
;;    (?𝐔 "\\mathbf{U}" "bu")
;;    (?𝐕 "\\mathbf{V}" "bv")
;;    (?𝐖 "\\mathbf{W}" "bw")
;;    (?𝐗 "\\mathbf{X}" "bx")
;;    (?𝐘 "\\mathbf{Y}" "by")
;;    (?𝐙 "\\mathbf{Z}" "bz")

;;    ;; (?𝔄 "\\mathfrak{A}" "fa")
;;    (?𝔅 "\\mathfrak{B}" "fb")
;;    (?𝔇 "\\mathfrak{D}" "fd")
;;    (?𝔈 "\\mathfrak{E}" "fe")
;;    (?𝔉 "\\mathfrak{F}" "ff")
;;    (?𝔊 "\\mathfrak{G}" "fg")
;;    (?𝔍 "\\mathfrak{J}" "fj")
;;    (?𝔎 "\\mathfrak{K}" "fk")
;;    (?𝔏 "\\mathfrak{L}" "fl")
;;    (?𝔐 "\\mathfrak{M}" "fm")
;;    (?𝔑 "\\mathfrak{N}" "fn")
;;    (?𝔒 "\\mathfrak{O}" "fo")
;;    (?𝔓 "\\mathfrak{P}" "fp")
;;    (?𝔔 "\\mathfrak{Q}" "fq")
;;    (?𝔖 "\\mathfrak{S}" "fs")
;;    (?𝔗 "\\mathfrak{T}" "ft")
;;    (?𝔘 "\\mathfrak{U}" "fu")
;;    (?𝔙 "\\mathfrak{V}" "fv")
;;    (?𝔚 "\\mathfrak{W}" "fw")
;;    (?𝔛 "\\mathfrak{X}" "fx")
;;    (?𝔜 "\\mathfrak{Y}" "fy")
;;    (?𝔞 "\\mathfrak{a}" "fA")
;;    (?𝔟 "\\mathfrak{b}" "fB")
;;    (?𝔠 "\\mathfrak{c}" "fC")
;;    (?𝔡 "\\mathfrak{d}" "fD")
;;    (?𝔢 "\\mathfrak{e}" "fE")
;;    (?𝔣 "\\mathfrak{f}" "fF")
;;    (?𝔤 "\\mathfrak{g}" "fG")
;;    (?𝔥 "\\mathfrak{h}" "fH")
;;    (?𝔦 "\\mathfrak{i}" "fI")
;;    (?𝔧 "\\mathfrak{j}" "fJ")
;;    (?𝔨 "\\mathfrak{k}" "fK")
;;    (?𝔩 "\\mathfrak{l}" "fL")
;;    (?𝔪 "\\mathfrak{m}" "fM")
;;    (?𝔫 "\\mathfrak{n}" "fN")
;;    (?𝔬 "\\mathfrak{o}" "fO")
;;    (?𝔭 "\\mathfrak{p}" "fP")
;;    (?𝔮 "\\mathfrak{q}" "fQ")
;;    (?𝔯 "\\mathfrak{r}" "fR")
;;    (?𝔰 "\\mathfrak{s}" "fS")
;;    (?𝔱 "\\mathfrak{t}" "fT")
;;    (?𝔲 "\\mathfrak{u}" "fU")
;;    (?𝔳 "\\mathfrak{v}" "fV")
;;    (?𝔴 "\\mathfrak{w}" "fW")
;;    (?𝔵 "\\mathfrak{x}" "fX")
;;    (?𝔶 "\\mathfrak{y}" "fY")
;;    (?𝔷 "\\mathfrak{z}" "fZ")

;;    (?𝓐 "\\mathcal{A}" "ca")
;;    (?𝓑 "\\mathcal{B}" "cb")
;;    (?𝓒 "\\mathcal{C}" "cc")
;;    (?𝓓 "\\mathcal{D}" "cd")
;;    (?𝓔 "\\mathcal{E}" "ce")
;;    (?𝓕 "\\mathcal{F}" "cf")
;;    (?𝓖 "\\mathcal{G}" "cg")
;;    (?𝓗 "\\mathcal{H}" "ch")
;;    (?𝓘 "\\mathcal{I}" "ci")
;;    (?𝓙 "\\mathcal{J}" "cj")
;;    (?𝓚 "\\mathcal{K}" "ck")
;;    (?𝓛 "\\mathcal{L}" "cl")
;;    (?𝓜 "\\mathcal{M}" "cm")
;;    (?𝓝 "\\mathcal{N}" "cn")
;;    (?𝓞 "\\mathcal{O}" "co")
;;    (?𝓟 "\\mathcal{P}" "cp")
;;    (?𝓠 "\\mathcal{Q}" "cq")
;;    (?𝓡 "\\mathcal{R}" "cr")
;;    (?𝓢 "\\mathcal{S}" "cs")
;;    (?𝓣 "\\mathcal{T}" "ct")
;;    (?𝓤 "\\mathcal{U}" "cu")
;;    (?𝓥 "\\mathcal{V}" "cv")
;;    (?𝓦 "\\mathcal{W}" "cw")
;;    (?𝓧 "\\mathcal{X}" "cx")
;;    (?𝓨 "\\mathcal{Y}" "cy")
;;    (?𝓩 "\\mathcal{Z}" "cz")

;;    (?𝛼 "\\alpha" "ga")
;;    (?𝛽 "\\beta" "gb")
;;    (?𝛿 "\\delta" "gd")
;;    (?𝜀 "\\varepsilon" "ge")
;;    (?𝜙 "\\phi" "gf")
;;    (?𝛾 "\\gamma" "gg")
;;    (?𝜂 "\\eta" "gh")
;;    (?𝜅 "\\kappa" "gk")
;;    (?λ "\\lambda" "gl")
;;    (?𝜇 "\\mu" "gm")
;;    (?𝜈 "\\nu" "gn")
;;    (?𝜔 "\\omega" "go")
;;    (?π "\\pi" "gp")
;;    (?𝜃 "\\theta" "gq")
;;    (?𝜌 "\\rho" "gr")
;;    (?𝜎 "\\sigma" "gs")
;;    (?𝜏 "\\tau" "gt")
;;    (?𝜐 "\\upsilon" "gu")
;;    (?𝜉 "\\xi" "gw")
;;    (?𝜒 "\\chi" "gx")
;;    (?𝛹 "\\psi" "gy")
;;    (?𝜁 "\\zeta" "gz")

;;    (?Δ "\\Delta" "gD")
;;    (?Φ "\\Phi" "gF")
;;    (?Γ "\\Gamma" "gG")
;;    (?Λ "\\Lambda" "gL")
;;    (?Ω "\\Omega" "gO")
;;    (?Π "\\Pi" "gP")
;;    (?Θ "\\Theta" "gQ")
;;    (?Σ "\\Sigma" "gS")
;;    (?Υ "\\Upsilon" "gU")
;;    (?Ξ "\\Xi" "gW")
;;    (?Ψ "\\Psi" "gY")

;;    (?𝜕 "\\partial" "ggd" "pp")
;;    (?𝜖 "\\epsilon" "gge")
;;    (?𝜑 "\\varphi" "ggf")

;;    (?ℓ "\\ell" "ell")
;;    (?∫ "\\int" "int")
;;    (?⨖ "\\sqint" "sqint")
;;    (?∮ "\\oint" "oint")
;;    (?⨑ "\\varointclockwise" "ointclockwise")
;;    (?∱ "\\varointctrclockwise" "ointclockwise")
;;    (?∬ "\\iint" "iint")
;;    (?∭ "\\iiint" "iiint")
;;    (?⨌ "\\iiint" "iiiint")
;;    (?∈ "\\in" "in")
;;    (?⊗ "\\otimes" "ot")
;;    (?× "\\times" "ox")
;;    (?∘ "\\circ" "oc")
;;    (?⮽ "\\boxtimes" "boxtimes" "boxprod")
;;    (?⧄ "\\squarediv" "lift" "lifts")
;;    (?⋯ "\\cdots" "cdots")
;;    (?… "\\dots" "dots")
;;    (?· "\\cdot" "cdot")
;;    (?⋄ "\\diamond" "dmnd" "diamond")
;;    (?ꞏ "\\bullet" "blt" "bullet")
;;    (?∅ "\\varnothing" "empty")
;;    (?⊆ "\\subseteq" "sse")
;;    (?∐ "\\coprod" "coprod")
;;    (?⨿ "\\amalg" "amalg" "cop")
;;    (?∀ "\\forall" "forall" "fa")
;;    (?♭ "\\flat" "flat" "flt")
;;    (?♯ "\\sharp" "sharp" "shrp")
;;    (?⋔ "\\pitchfork" "cotens" "cotns" "ctns" "cotensor" "pitchfork")
;;    (?∪ "\\cup" "union" "cup")

;;    (?▷ "\\vartriangleright" "|>")
;;    (? "\\rightarrow" "->")
;;    (? "\\longrightarrow" "-->")
;;    (?⟹ "\\Rightarrow" "=>")
;;    (? "\\twoheadrightarrow" "->>")
;;    (? "\\rightarrowtail" ">->")
;;    (?⟼ "\\mapsto" "|->"))
;;  '(("geq" "\\geq")
;;    ("sk" "\\Sk")
;;    ("ob" "\\Ob")
;;    ("set" "\\Set")
;;    ;; ("sset" "\\sSet")
;;    ("hom" "\\Hom")
;;    ("uhom" "\\uHom")
;;    ("ima" "\\Ima")
;;    ("id" "\\id")
;;    ("ar" "\\ar")
;;    ("psh" "\\Psh")
;;    ("rfib" "\\RFib")
;;    ("hc" "\\hc")
;;    ("path" "\\Path")
;;    ("grp" "\\Group")
;;    ("cat" "\\Cat")
;;    ("and" "\\textnormal{ and }")
;;    ("if" "\\textnormal{ if }")
;;    ("for" "\\textnormal{ for }")
;;    ("scat" "\\Cat_{Δ}" "simplicial category")
;;    ("sset" "\\Set_{Δ}" "simplicial set")
;;    ))

;; Local Variables:
;; eval: (add-hook! 'after-save-hook :local (byte-compile-file (buffer-file-name (current-buffer)) t))
;; End:
