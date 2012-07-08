(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(add-to-list 'load-path '("~/.emacs.d"))
(require 'fill-column-indicator)

(if (= emacs-major-version 20)
(set-scroll-bar-mode 'right))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;(setq load-path
;    (append
;    (list "~/.emacs.d"
;            "~/.emacs.d/remember"
;    )
;    load-path
;))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Required packages...

;(require 'filladapt)
(require 'paren)	
;(require 'scroll-in-place)
;(require 'crypt++)
(require 'uniquify)
;(require 'flobl)
(require 'calendar)
;(require 'php-mode)
;(require 'fill-column-indicator)

;; cc-mode tweaks and fixes

(require 'cc-mode)
(c-initialize-cc-mode)

;; ;; c-paren-re and c-identifier-re were helper macros that got removed
;; ;; from cc-mode, as noted in the changelog for 2002-09-10.  If you add
;; ;; these to csharp-mode.el, things seem to work ok.

;; ;; Helpers for building regexps.
;; (defmacro c-paren-re (re) `(concat "\\(" ,re "\\)"))
;; (defmacro c-identifier-re (re) `(concat "\\[^_]"))
;; (defconst c-protection-key "\\<\\(public\\|protected\\|private\\)\\>") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General variable definitions, etc...

(setq nobreak-char-display t)
                            
;(setq default-input-method "MacOSX")
;(set-input-method "MacOSX")

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

;(when (eq 0 (string-match "*-apple-darwin*" system-configuration))
;  (setq x-select-enable-clipboard t))
;(setq interprogram-cut-function 'own-clipboard
;     interprogram-paste-function 'get-clipboard-foreign)
;(mac-key-mode 1)
;(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
                    
;; fonts and keyboard encoding
;(set-keyboard-coding-system 'mac-roman)

;; set utf as default save encoding for files
(set-language-environment "utf-8")
(set-coding-priority (list 'coding-category-utf-8))
(prefer-coding-system 'utf-8)
    
(column-number-mode 1)
(setq scroll-margin 0)
(setq scroll-step 0)
(setq scroll-conservatively 100)

(set-background-color "black")
(set-foreground-color "gray85")
(set-mouse-color "white")
(set-cursor-color "white")
(setq frame-title-format "%b       %f")

(setq default-major-mode 'text-mode)

(setq vc-follow-symlinks nil)
(setq next-line-add-newlines nil)       ; No newlines added at bottom of doc
(setq search-highlight t)               ; hilite in isearch
(setq visible-bell t)
(setq truncate-partial-width-windows nil)
(setq mode-line-inverse-video t)
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil) ; don't want \t used for indentation
(setq-default tab-width 4)
(set-face-background (quote region) "grey32")
(set-face-foreground (quote region) "white")
(set-face-background (quote highlight) "grey32")
(set-face-foreground (quote highlight) "white")

(global-set-key "\C-xp" '(lambda () (interactive) (other-window -1)))

(global-unset-key [C-return])
(global-set-key [C-return] 'split-line)

(global-set-key (kbd "C-c C-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-region)

(global-unset-key (kbd "M-%"))
(global-set-key   (kbd "M-%") 'replace-regexp)

(global-unset-key (kbd "M-C-q"))
(global-set-key   (kbd "M-C-q") 'unfill-paragraph)

;;(global-set-key [C-down-mouse-1] (function mouse-buffer-menu))

;;; http://alanb.com/hacks/dot-emacs.txt
(global-set-key (kbd "C-z") 'nil) ; C-z don't iconify
(global-set-key (kbd "M-3") 'self-insert-command)

;; if running in tty, fix backspace and change help key to ESC-?
;(cond 
; ((eq (device-type) 'tty)
;  (global-set-key "\^h" 'delete-backward-char) ;; fix backspace
;  (global-set-key "\e?" 'help)                 ;; ESC-? is help
;))

;; http://www.delorie.com/gnu/docs/emacs-lisp-intro/emacs-lisp-intro_toc.html

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph       
;;; Takes a multi-line paragraph and makes it into a single line of text.       
(defun unfill-paragraph ()
(interactive)
(let ((fill-column (point-max)))
(fill-paragraph nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Move to top/bottom of window (1/21/98)
;;; sanitized for powerbook keyboard (require fn for pgup/pgdn/home/end)

(defun line-to-top-of-window ()
"Move the line that the point is on to the top of window."
(interactive) 
(recenter 0))

(defun line-to-bottom-of-window () ;; me, after above and below
"Move the line that the point is on to the bottom of window."
(interactive) 
(recenter (frame-height)))

(defun warp-to-top-of-window ()
"Move the point to line 0"
(interactive) 
(move-to-window-line 0))

(defun warp-to-bottom-of-window ()
"Move the point to line (frame-height)"
(interactive)
(move-to-window-line -1))

;| point-to  | previous   | next        |
;|-----------+------------+-------------|
;| char      | <left>     | <right>     |
;| word      | C/M-<left> | C/M-<right> |
;| line      | <up>       | <down>      |
;| paragraph | C-<up>     | C-<down>    |

;| point-to | start  | end      |
;|----------+--------+----------|
;| line     | C-a    | C-e      |
;| sentence | M-a    | M-e      |
;| screen   | M-<up> | M-<down> |
;| file     | M-\<   | M-\>     |

;| window-to | key        |
;|-----------+------------|
;| top       | C-M-<down> |
;| bottom    | C-M-<up>   |

;| centre-current |     |
;|----------------+-----|
;| point          | M-r |
;| window         | C-l |

;;
;; for poxy macbook keyboard with only the arrow keys
(global-set-key (kbd "M-<up>") 'warp-to-top-of-window)
(global-set-key (kbd "M-<down>") 'warp-to-bottom-of-window)
(global-set-key (kbd "C-M-<down>") 'line-to-top-of-window)
(global-set-key (kbd "C-M-<up>") 'line-to-bottom-of-window)

;;
;; for a sensible pc keyboard with pgup|pgdn|home|end
;; 
(global-set-key [C-prior] 'warp-to-top-of-window)
(global-set-key [C-next] 'warp-to-bottom-of-window)
(global-set-key [C-home] 'line-to-top-of-window)
(global-set-key [C-end] 'line-to-bottom-of-window)
(global-set-key [home] 'beginning-of-buffer) ; also M-<
(global-set-key [end] 'end-of-buffer) ; also M->

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Show time

;(setq display-time-day-and-date t)
;(setq display-time-mail-file "~/.mail")
;(display-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dave's keys

;(fset 'simple-cite          [(control x) r t > space return])
;(fset 'simple-cite-and-fill [(control x) r t > space return (meta q)])
;(fset 'simple-indent        [(control x) r t space space return])

;(global-set-key [f3]  'save-buffer)
;(global-set-key [f4]  'find-file)
;(global-set-key [f5]  'simple-cite)           ;F5 cite selected region with "> "
;(global-set-key [f6]  'simple-cite-and-fill)  ;F6 cite like f5, then fill
;(global-set-key [f7]  'simple-indent)         ;F7 indent region 2 spaces
;(global-set-key [f9]  'other-window)
;(global-set-key [f10] 'other-frame)
;(global-set-key [f11] 'other-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sensible buffer names 

(setq mnemonic-buffer-names t)
(setq minimum-buffer-name-dir-content 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; prb's abbrev stuff

(global-unset-key [C-tab])
(global-set-key [C-tab] 'dabbrev-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; prb's mark-mode highlight stuff

(progn
(setq transient-mark-mode t)
(set-face-background 'region "steelblue3")
(set-face-foreground 'region nil)
(set-face-background 'highlight "steelblue3")
(set-face-foreground 'highlight nil)
t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paren matching

(show-paren-mode 1)
(setq show-paren-style 'expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Package specific stuff...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cyclic buffers

;(autoload 'cyclebuffer-forward "cyclebuffer" "cycle forward" t)
;(autoload 'cyclebuffer-backward "cyclebuffer" "cycle backward" t)
;(global-set-key (kbd "M-n") 'cyclebuffer-forward)
;(global-set-key (kbd "M-p") 'cyclebuffer-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Smart find-file

(autoload 'smart-find-file "smart-find" "A smarter find-file." t)
(global-set-key (kbd "C-x C-f") 'smart-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; prb's ispell stuff

(setq-default ispell-program-name "/usr/local/bin/aspell") 
(autoload 'ispell-minor-mode "ispell.el" t t)

(setq ispell-list-command "list")
(setq ispell-extra-args '("--reverse")) ; sug-mode=ultra"))
(setq ispell-local-dictionary "~/aspell.en.pws")
(make-face 'spelling-mistake)
(set-face-foreground 'spelling-mistake "white")
(set-face-background 'spelling-mistake "red")
(setq ispell-highlight-face 'spelling-mistake)
(setq ispell-dictionary "british")
;(setq-default ispell-local-dictionary "british")

;(make-face 'ispell)
;(set-face-foreground 'ispell "white")
;(set-face-background 'ispell "red")

(defun ispell-check-paragraph ()
"Spell check each word in a paragraph"
(interactive "*")
(let ((ispell-minor-mode nil)
    (ispell-check-only nil)
    (ispell-quietly t)
    )

;    (save-excursion
;      (forward-paragraph) (setq end (point))
;      (forward-paragraph -1)
;      (while (re-search-forward "[ \t\n\r]" end t)
;        (save-restriction
;          (narrow-to-region (point-min) (point))
;          (ispell-word nil t))))

(save-excursion
    (forward-paragraph) (setq end (point))
    (forward-paragraph -1) (setq start (point))
    (ispell-region start end))

))

(defun fill-and-check ()
"Fill a paragraph and spell check"
(interactive)
(fill-paragraph nil)
(ispell-check-paragraph)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Align using regexps...

(autoload 'align-regexp "align-regexp.el" "" t)

(defun align-regexp-repeated (start stop regexp) 
"Like align-regexp, but repeated for multiple columns. See http://www.emacswiki.org/emacs/AlignCommands" 
(interactive "r\nsAlign regexp: ") 
(let ((spacing 1) 
    (old-buffer-size (buffer-size))) 
;; If our align regexp is just spaces, then we don't need any 
;; extra spacing. 
(when (string-match regexp " ") 
    (setq spacing 0)) 
(align-regexp start stop 
                ;; add space at beginning of regexp 
                (concat "\\([[:space:]]*\\)" regexp) 
                1 spacing t) 
;; modify stop because align-regexp will add/remove characters 
(align-regexp start (+ stop (- (buffer-size) old-buffer-size)) 
                ;; add space at end of regexp 
                (concat regexp "\\([[:space:]]*\\)") 
                1 spacing t))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; eldoc; shows documentation for lisp function under cursor 

(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(defconst eldoc-minor-mode-string "")
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Font lock & major mode stuff...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load "my-font-lock")

(autoload 'asm-mode      "asm-mode")
(autoload 'bibtex-mode   "bibtex")
(autoload 'bison-mode    "bison-mode" "" t)
(autoload 'middl-mode    "middl-mode" "Major mode for editing MIDDL files" t)
(autoload 'modula-3-mode "modula3" "m3 mode" t)
(autoload 'postscript    "postscript-mode" "" t)
(autoload 'python-mode   "python-mode" "Python editing mode." t)
; (autoload 'python "python-mode" "Python editing mode." t)
(autoload 'sh-mode       "sh-mode" "Major mode for editing shell scripts" t)
;; (autoload 'sml-mode      "sml-mode" "Major mode for editing SML." t)
(autoload 'tab-mode      "tabulature-mode")
(autoload 'follow-mode   "follow" nil t)
(autoload 'doctex-mode   "swiftex" "Major mode for LaTeX doc documents.")
(autoload 'csharp-mode   "csharp-mode" "Major mode for editing C# code." t)
(autoload 'awk-mode        "cc-mode" nil t)
(autoload 'ecmascript-mode "ecmascript-mode" nil t)
(autoload 'css-mode "css-mode" nil t)
(autoload 'js2-mode "js2" nil t)

(setq auto-mode-alist 
    (append
    (list (cons "\\.asm$"      'asm-mode)
            (cons "\\.awk$"	  'perl-mode)
            (cons "\\.gawk$"	  'perl-mode)
            (cons "\\.css$"	  'css-mode)
            (cons "\\.s$"        'asm-mode) 
            (cons "\\.S$"        'asm-mode) 
            (cons "\\.bib$"      'bibtex-mode)
            (cons "\\.java$"     'java-mode)
            (cons "\\.class$"    'java-mode)
            (cons "\\.cls$"      'doctex-mode)
            (cons "\\.dtx$"      'doctex-mode)
            (cons "\\.sto$"      'doctex-mode)
            (cons "\\.clo$"      'doctex-mode)
            (cons "\\.sty$"      'doctex-mode)
            (cons "\\.fdd$"      'doctex-mode)
            (cons "sources$"     'makefile-mode) 
            (cons "Makefile$"    'makefile-mode) 
            (cons "\\.mk$"       'makefile-mode) 
            (cons "\\.if$"       'middl-mode)
            (cons "\\.[im][g3]$" 'modula-3-mode)
            (cons "\\.ps$"       'postscript-mode)
            (cons "\\.py$"       'python-mode)
;;             (cons "\\.ml$"       'sml-mode)
            (cons "\\.sh$"       'sh-mode)
            (cons "\\.ns$"       'tcl-mode)
            (cons "\\.cc$"       'c++-mode)
            (cons "\\.hh$"       'c++-mode)
            (cons "\\.tc$"       'c++-mode)
            (cons "\\.th$"       'c++-mode)
            (cons "\\.idl$"      'c++-mode)
            (cons "\\.l$"        'c-mode)
            (cons "\\.ih$"       'c-mode)
            (cons "\\.y$"        'yacc-mode)
            (cons "\\.cs$"       'csharp-mode)
            (cons "\\.js$"       'js2-mode)
            (cons "\\.xml$"      'js2-mode)
            )
    auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ECMAScript support

(add-hook 
'ecmascript-mode-hook 
'(lambda ()
(fci-mode 1)
(setq c-basic-offset 4)
(c-set-style "java")
)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XML support

;(load "nxml-mode/rng-auto.el")
(push '("\\`<\\?xml" . nxml-mode) magic-mode-alist)
(add-hook 
 'nxml-mode-hook 
 '(lambda ()
    (fci-mode 1)
    (define-key nxml-mode-map "\C-c\C-c" 'comment-region)
    (define-key nxml-mode-map "\C-c\C-q" 'indent-region)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tuareg/typerex OCaml/F# support

;; (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.fs[ix]?" . tuareg-mode) auto-mode-alist))
;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
;; (autoload 'caml-types-show-type "caml-types" "Show type of expression / pattern at point." t)

(add-hook 'tuareg-mode-hook
          '(lambda ()
             (fci-mode 1)
;;              (setq tuareg-default-indent 2)
             (setq tuareg-lazy-= t) ; indent `=' like a standard keyword
             (setq tuareg-lazy-paren t) ; indent [({ like standard keywords
             (setq tuareg-in-indent 0) ; no indentation after `in' keywords
             (auto-fill-mode 1) ; turn on auto-fill minor mode
;;              (if (featurep 'sym-lock)   ; Sym-Lock customization only
;;                  (setq sym-lock-mouse-face-enabled nil))
;;                                         ; turn off special face under mouse
;;              (set-face-background caml-types-expr-face "slategray")
             ))

;; (eval-after-load 'caml-types
;;   (progn
;;     (face-spec-set 'caml-types-expr-face
;;                    '((((class color) (type tty)) (:inverse-video t))
;;                      (((class color) (background light)) (:background "gray73"))
;;                      (((class color) (background dark))  (:background "darkslategray"))))
;;     ))

(add-hook 'typerex-mode-hook
          (lambda ()
            (fci-mode 1)
            (auto-fill-mode 1)
            ))

;; Loading TypeRex mode for OCaml files
;; (add-to-list 'load-path "/Users/mort/.emacs.d")
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . typerex-mode))
(add-to-list 'auto-mode-alist '("\\.fs[ix]?" . typerex-mode))
(autoload 'typerex-mode "typerex" "Major mode for editing Caml code" t)

;; TypeRex mode configuration
(setq ocp-server-command "/usr/local/bin/ocp-wizard")
(setq typerex-in-indent 0)
(setq-default indent-tabs-mode nil)

;; Uncomment to enable typerex command menu by right click
(setq ocp-menu-trigger [mouse-3])

;; Uncomment make new syntax coloring look almost like Tuareg
(setq ocp-theme "tuareg_like")
;; Uncomment to disable new syntax coloring and use Tuareg
;(setq ocp-syntax-coloring nil)

;;;; Auto completion (experimental)
;;;; Don't use M-x invert-face default with auto-complete! (emacs -r is OK)
;;(add-to-list 'load-path "/Users/mort/.emacs.d/auto-complete-mode")
;;(setq ocp-auto-complete t)

;;;; Using <`> to complete whatever the context, and <C-`> for `
;;(setq auto-complete-keys 'ac-keys-backquote-backslash)
;;;; Options: nil (default), 'ac-keys-default-start-with-c-tab, 'ac-keys-two-dollar
;;;; Note: this overrides individual auto-complete key settings

;;;; I want immediate menu pop-up
;;(setq ac-auto-show-menu 0.1)
;;;; Short delay before showing help
;;(setq ac-quick-help-delay 0.3)
;;;; Number of characters required to start (nil to disable)
;;(setq ac-auto-start 0)

;;;; Uncomment to enable auto complete mode globally (independently of OCaml)
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "/Users/mort/.emacs.d/auto-complete-mode/ac-dict")
;;(ac-config-default)
;;(global-set-key (kbd "C-<tab>") 'auto-complete)

;; For debugging only
;;;;(setq ocp-debug t)
;;;;(setq ocp-profile t)
;;;;(setq ocp-dont-catch-errors t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; markdown mode

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (append (list (cons "\\.md" 'markdown-mode)
                    (cons "\\.markdown" 'markdown-mode) 
                    )
              auto-mode-alist))

(add-hook 'markdown-mode-hook 
          '(lambda ()
             (fci-mode 1)
             (local-unset-key (kbd "M-n"))
             (local-unset-key (kbd "M-p"))
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Swiftex TeX support

(autoload 'swiftex-mode "swiftex" "Major mode for LaTeX documents.")
(setq auto-mode-alist
      (append (list (cons "\\.bbl$" 'swiftex-mode)
                    (cons "\\.tex$" 'swiftex-mode)
                    (cons "\\.ltx$" 'swiftex-mode)
                    (cons "\\.aux$" 'swiftex-mode)
                    (cons "\\.ins$" 'swiftex-mode)
                    )
              auto-mode-alist))

(add-hook 
 'swiftex-mode-hook 
 '(lambda ()
    (fci-mode 1)
    (ispell-minor-mode)
    (define-key swiftex-mode-map "\M-q"     'fill-and-check)
    (define-key swiftex-mode-map "\C-c\C-c" 'comment-region)
    (define-key swiftex-mode-map "\C-cm"    'stx-emphasize)
;    (define-key swiftex-mode-map "\C-ce"    'stx-close-block-from-inside)
    (define-key swiftex-mode-map "\C-cB"    'stx-insert-block)
    (define-key swiftex-mode-map "\C-cb"    'stx-begin-block)
    (define-key swiftex-mode-map "\M-["     '(lambda () (interactive) (insert "{")))
    (define-key swiftex-mode-map "\M-]"     '(lambda () (interactive) (insert "}")))
    (define-key swiftex-mode-map "{"        'tex-insert-braces)
    (local-unset-key [C-return])
;;    (setq msb-menu-cond
;;          (cons '((eq major-mode 'swiftex-mode) 3030 "LaTeX Files (%d)")
;;                msb-menu-cond))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Java mode

(defun my-java-mode-hook ()
  (setq c-basic-offset 4)
  (setq c-set-style "java")
  (fci-mode 1)
  )
(add-hook 'java-mode-hook 'my-java-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Anders Java Font lock

;;   (cond (window-system
;; 	 (require 'andersl-java-font-lock)
;; 	 (turn-on-font-lock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CC-mode (called by c-mode, c++-mode and objc-mode)

(add-hook 'c-mode-common-hook 
          '(lambda () 
             (fci-mode 1)
             (setq c-basic-offset 4)
             (setq c-set-style "bsd")
;         (c-add-style "bsd" (c-basic-offset . 4) 't)
;	     (c-set-style "bsd")	;Close enough!
             ;; Case sensitive expansion/completion
             (set (make-local-variable 'dabbrev-case-fold-search) nil)
             (set (make-local-variable 'dabbrev-case-replace) nil)
             ;; This is C++ mode - but we want imenu to use C index patterns 
             (setq imenu-create-index-function 'imenu-example--create-c-index)
             (define-key c-mode-map [menu-bar vc-menu] (cons "RCS" vc-menu-map))
             (c-set-offset 'case-label '+)))
(setq next-line-add-newlines nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Python

(add-hook 'python-mode-hook
          '(lambda ()
             (font-lock-mode)
             (fci-mode 1)
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shell script support...

(setq interpreter-mode-alist 
      (cons '("sh" . sh-mode) interpreter-mode-alist))
(setq interpreter-mode-alist 
      (cons '("bash" . sh-mode) interpreter-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn on fill in any text mode

(add-hook 'text-mode-hook 
          '(lambda () 
             (fci-mode 1)
             (auto-fill-mode 1)
             (ispell-minor-mode)
             (turn-on-filladapt-mode)
             (local-set-key "\M-q" 'fill-and-check)
             (local-unset-key [C-return])
             )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Makefile mode support

(add-hook 'makefile-mode-hook 
          '(lambda ()
             (fci-mode 1)
             (local-unset-key (kbd "M-n"))
             (local-unset-key (kbd "M-p"))
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Javascript and Actionscript support


;; (autoload 'javascript-mode "javascript" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;; ;(add-to-list 'auto-mode-alist '("\\.as\\'" . javascript-mode))

;; ;; Actionscript stuff
;; (autoload 'actionscript-mode "actionscript-mode"
;;   "Major mode for editing ActionScript files." t)

;; (add-to-list 'auto-mode-alist '("\\.as[123]?$" . actionscript-mode))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;

;; ;; Configuration for the English and Welsh Bank Holidays.  Accounts
;; ;; for weekends.  Dates sourced from the UK Department of Trade and
;; ;; Industry (http://www.dti.gov.uk/er/bankhol.html).
;; ;;
;; ;; Suitable for a .emacs file.  Enable with a line like:
;; ;;
;; ;; (setq other-holidays (append english-and-welsh-bank-holidays
;; ;;                              other-holidays))
;; ;;
;; ;; or, to override US holidays, like:
;; ;;

;; (defun abs-easter (displayed-year)
;;   "Return the absolute date of Easter Sunday in DISPLAYED-YEAR.
;; Taken directly from `holiday-easter-etc' in holidays.el.  Perhaps
;; it would be better to have this function there."
;;   (let* ((century (1+ (/ displayed-year 100)))
;;          (shifted-epact	;; Age of moon for April 5...
;;           (% (+ 14 (* 11 (% displayed-year 19))	;;     ...by Nicaean rule
;;                 (- ;; ...corrected for the Gregorian century rule
;;                  (/ (* 3 century) 4))
;;                 (/ ;; ...corrected for Metonic cycle inaccuracy.
;;                  (+ 5 (* 8 century)) 25)
;;                 (* 30 century))	;;              Keeps value positive.
;;              30))
;;          (adjusted-epact ;;  Adjust for 29.5 day month.
;;           (if (or (= shifted-epact 0)
;;                   (and (= shifted-epact 1) (< 10 (% displayed-year 19))))
;;               (1+ shifted-epact)
;;             shifted-epact))
;;          (paschal-moon ;; Day after the full moon on or after March 21.
;;           (- (calendar-absolute-from-gregorian (list 4 19 displayed-year))
;;              adjusted-epact)))
;;     (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

;; (eval-when-compile
;;   ;; free vars - should these be defined by calendar?
;;   (defvar year)
;;   (defvar date))

;; (defun ewbh-xmas (day-in-dec)
;;   "Return the English and Welsh Bank Holiday for DAY-IN-DECember.
;; Expects DAY-IN-DEC to be either 25 or 26.  Accounts for weekends."
;;   (let* ((date `(12 ,day-in-dec ,year))
;;          (day (calendar-day-name date)))
;;     (cond
;;      ((equal day "Saturday")
;;       (calendar-gregorian-from-absolute
;;        (calendar-dayname-on-or-before
;;         1                               ; Monday
;;         (+ 7 (calendar-absolute-from-gregorian date)))))
;;      ;; Boxing Day on Sunday means Christmas was on Saturday
;;      ((equal day "Sunday")
;;       (calendar-gregorian-from-absolute
;;        (calendar-dayname-on-or-before
;;         2                               ; Tuesday
;;         (+ 7 (calendar-absolute-from-gregorian date)))))
;;      (t date))))

;; (defun ewbh-weekday (date)
;;   "Return the Monday following DATE if DATE falls on a weekend, else DATE.
;; DATE must be a (month day year) list."
;;   (let* ((abs-date (calendar-absolute-from-gregorian date))
;;          (day (calendar-day-name date)))
;;     (if (or (equal day "Saturday")
;;             (equal day "Sunday"))
;;         (calendar-gregorian-from-absolute
;;          (calendar-dayname-on-or-before
;;           1                             ; Monday
;;           (+ 7 abs-date)))
;;       date)))

;; (defvar english-and-welsh-bank-holidays
;;   '((holiday-sexp '(ewbh-weekday `(1 1 ,year))
;;                   "New Year's Day Bank Holiday")
;;     (holiday-sexp '(calendar-gregorian-from-absolute
;;                     (- (abs-easter year) 2))
;;                   "Good Friday Bank Holiday")
;;     (holiday-sexp '(calendar-gregorian-from-absolute
;;                     (1+ (abs-easter year)))
;;                   "Easter Monday Bank Holiday")
;;     (holiday-float 5 1 1 "Early May Bank Holiday")
;;     (holiday-float 5 1 -1 "Spring Bank Holiday")
;;     (holiday-float 8 1 -1 "Summer Bank Holiday")
;;     (holiday-sexp '(ewbh-xmas 25)
;;                   "Christmas Day Bank Holiday")
;;     (holiday-sexp '(ewbh-xmas 26)
;;                   "Boxing Day Bank Holiday"))
;;   "*Bank holidays for England and Wales, according to the UK Department
;; of Trade and Industry (http://www.dti.gov.uk/er/bankhol.html).")

;; (setq general-holidays english-and-welsh-bank-holidays)

(setq bahai-holidays nil)
(setq hebrew-holidays nil)

;;UK public holidays, and other UK notable dates.
(setq general-holidays
      '((holiday-fixed 1 1 "New Year's Day")
	(holiday-new-year-bank-holiday)
	(holiday-fixed 2 14 "Valentine's Day")
	(holiday-fixed 3 17 "St. Patrick's Day")
	(holiday-fixed 4 1 "April Fools' Day")
	(holiday-easter-etc -47 "Shrove Tuesday")
	(holiday-easter-etc -21 "Mother's Day")
	(holiday-easter-etc -2 "Good Friday")
	(holiday-easter-etc 0 "Easter Sunday")
	(holiday-easter-etc 1 "Easter Monday")
	(holiday-float 5 1 1 "Early May Bank Holiday")
	(holiday-float 5 1 -1 "Spring Bank Holiday")
	(holiday-float 6 0 3 "Father's Day")
	(holiday-float 8 1 -1 "Summer Bank Holiday")
	(holiday-fixed 10 31 "Halloween")
	(holiday-fixed 12 24 "Christmas Eve")
	(holiday-fixed 12 25 "Christmas Day")
	(holiday-fixed 12 26 "Boxing Day")
	(holiday-christmas-bank-holidays)
	(holiday-fixed 12 31 "New Year's Eve")))

;;Major US holidays
(setq other-holidays
      '((holiday-float 1 1 3 "Martin Luther King Day")
	(holiday-float 2 1 3 "President's Day")
	(holiday-float 5 1 -1 "Memorial Day")
	(holiday-fixed 7 4 "Independence Day")
	(holiday-float 9 1 1 "Labor Day")
	(holiday-float 10 1 2 "Columbus Day")
	(holiday-fixed 11 11 "Veteran's Day")
	(holiday-float 11 4 4 "Thanksgiving")))

;;N.B. It is assumed that 1 January is defined with holiday-fixed -
;;this function only returns any extra bank holiday that is allocated
;;(if any) to compensate for New Year's Day falling on a weekend.
;;
;;Where 1 January falls on a weekend, the following Monday is a bank
;;holiday.
(defun holiday-new-year-bank-holiday ()
  (let ((m displayed-month)
	(y displayed-year))
    (increment-calendar-month m y 1)
    (when (<= m 3)
      (let ((d (calendar-day-of-week (list 1 1 y))))
	(cond ((= d 6)
	       (list (list (list 1 3 y)
			   "New Year's Day Bank Holiday")))
	      ((= d 0)
	       (list (list (list 1 2 y)
			   "New Year's Day Bank Holiday"))))))))

;;N.B. It is assumed that 25th and 26th are defined with holiday-fixed -
;;this function only returns any extra bank holiday(s) that are
;;allocated (if any) to compensate for Christmas Day and/or Boxing Day
;;falling on a weekend.
;;
;;Christmas day is always 25 December; beyond that there is no
;;entirely consistent practice.  We proceed as follows:
;;
;;Traditionally, Boxing day was the first day after Christmas, not
;;including Sundays (i.e. if Christmas fell on a Saturday, Boxing Day
;;was Monday 27th) however we follow modern practice here and always
;;regard Boxing Day as 26 December (which, as noted above, is never
;;returned by this function).
;;
;;Generally the extra bank holiday is allocated on the first available
;;day that would otherwise have been a working day.  However in the
;;case where we need to allocate two additional bank holidays -
;;i.e. where Christmas Day falls on the Saturday, there is some
;;confusion as to how to proceed.  We allocate the Boxing Day Bank Holiday
;;to the Monday, since this is the historic date of Boxing Day in this
;;case, and allocate the Christmas Day Bank Holiday to the following day.
;;
;;This is consistent with the way that the 'substitute days' were
;;allocated in the list of bank holidays on the Department of Trade
;;and Industry in the recent past, although they don't use the any
;;specific names for these holidays.
;;
;;The latest list on the direct.gov.uk web site is not consistent with
;;this practice, however, allocating the substitute days for Christmas
;;Day and Boxing Day in the other order in 2010.  However this list
;;also manages to allocate them in order in 2011 (where Christmas Day
;;falls on a Sunday), therefore placing the substitute holiday for
;;Christmas Day _on_ Boxing Day, and then the substitute holiday for
;;Boxing Day on the following day.  I'm not at all sure this isn't a
;;mistake.
;;
;;In any case, this is largely academic as there is no dispute over
;;which days are public holidays, only what to call them - so unless
;;you care deeply just ignore the issue and use the function as
;;supplied.
(defun holiday-christmas-bank-holidays ()
  (let ((m displayed-month)
	(y displayed-year))
    (increment-calendar-month m y -1)
    (when (>= m 10)
      (let ((d (calendar-day-of-week (list 12 25 y))))
	(cond ((= d 5)
	       (list (list (list 12 28 y)
			   "Boxing Day Bank Holiday")))
	      ((= d 6)
	       (list (list (list 12 27 y)
			   "Boxing Day Bank Holiday")
		     (list (list 12 28 y)
			   "Christmas Day Bank Holiday")))
	      ((= d 0)
	       (list (list (list 12 27 y)
			   "Christmas Day Bank Holiday"))))))))

;;Comment out the Christian holidays that also have secular
;;significance in the UK (Shrove Tuesday, Good Friday, Easter Sunday,
;;Christmas) as EMACS doesn't remove duplicates holidays.  These
;;holidays are included in the UK redefinition of general-holidays
;;(where Chistmas is listed as Christmas Day).
(setq christian-holidays
      '((if all-christian-calendar-holidays
	    (holiday-fixed 1 6 "Epiphany"))
;	(holiday-easter-etc 0 "Easter Sunday")
;	(holiday-easter-etc -2 "Good Friday")
	(holiday-easter-etc -46 "Ash Wednesday")
	(if all-christian-calendar-holidays
	    (holiday-easter-etc -63 "Septuagesima Sunday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc -56 "Sexagesima Sunday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc -49 "Shrove Sunday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc -48 "Shrove Monday"))
;	(if all-christian-calendar-holidays
;	    (holiday-easter-etc -47 "Shrove Tuesday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc -14 "Passion Sunday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc -7 "Palm Sunday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc -3 "Maundy Thursday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc 35 "Rogation Sunday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc 39 "Ascension Day"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc 49 "Pentecost (Whitsunday)"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc 50 "Whitmonday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc 56 "Trinity Sunday"))
	(if all-christian-calendar-holidays
	    (holiday-easter-etc 60 "Corpus Christi"))
	(if all-christian-calendar-holidays
	    (holiday-greek-orthodox-easter))
	(if all-christian-calendar-holidays
	    (holiday-fixed 8 15 "Assumption"))
	(if all-christian-calendar-holidays
	    (holiday-advent 0 "Advent"))
;	(holiday-fixed 12 25 "Christmas")
	(if all-christian-calendar-holidays
	    (holiday-julian 12 25 "Eastern Orthodox Christmas"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Org-mode

;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)

(defun todo ()
   (interactive)
   (find-file "~/.todo/todo.org")
)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key mode-specific-map [?a] 'org-agenda)
 
(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\C-cx" 'org-todo-state-map) 
     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "*"
       #'(lambda nil (interactive) (org-todo "DOING")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING"))) 
     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

     (define-key org-agenda-keymap "\C-p" 'previous-line)))

(add-hook 'org-mode-hook 
         (lambda () (imenu-add-to-menubar "Imenu")))

(require 'remember) 
(add-hook 'remember-mode-hook 'org-remember-apply-template) 
(define-key global-map [(control meta ?r)] 'remember)
(setq org-blank-before-new-entry
 '((heading . t) (plain-list-item . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Source Depot support

;(load-library "sd")
;(sd-set-sd-executable "C:/Program Files/Development Tools/sd.exe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Random functions...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun reread-dot-emacs () 
  "Re-read ~/.emacs"
  (interactive) 
  (load-file "~/.emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun right-column-asterisk ()
  "Put an asterisk in the column before the current value of fill-column."
  (interactive)
  (move-to-column (- fill-column 1) t)
  (insert "*"))

(defun left-column-percent ()
  "Put a % in the first column."
  (interactive)
  (move-to-column 0 t)
  (insert "%")
  (forward-line 1))

(defun fill-line-with-asterisks ()
  "Put asterisks from the current point to the column before fill-column."
  (interactive)
  (let ((numstars (-
		   (save-excursion
		     (move-to-column fill-column t))
		   (current-column))))
    (insert-char ?* numstars)
    (kill-line)				; can't get overwrite to work
    (newline)))				; use hack to delete extra spaces

(defun fill-line-with-hashes ()
  "Put hashes from the current point to the column before fill-column."
  (interactive)
  (let ((numstars (-
		   (save-excursion
		     (move-to-column fill-column t))
		   (current-column))))
    (insert-char ?# numstars)
    (kill-line)				; can't get overwrite to work
    (newline)))				; use hack to delete extra spaces

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun my-kill-emacs ()
  "Confirm before save-buffers-kill-emacs"
  (interactive)
  (if (y-or-n-p "Really kill emacs? ")
      (save-buffers-kill-emacs)
    (message "Aborted")))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun wc ()
  "Counts the number of words in the region"
  (interactive)
  (shell-command-on-region (point) (mark) "wc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun dos2unix () "Turn DOS file to Unix"
  (interactive)
  (save-excursion 
    (beginning-of-buffer)
    (replace-string "\015" "" )))

(defun unix2dos () "Turn Unix file to DOS"
  (interactive)
  (save-excursion 
    (beginning-of-buffer)
    (replace-string "\012" "\015\012" )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun mime2unix ()
  "Get rid of MIME =RET junk"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "=\n" "" )
    (replace-string "=20" "" )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun find-function (function)
  (interactive "Function: ")
  (find-file (locate-library 
	      (concat (describe-function-find-file function) ".el")
	      'nosuffix))
  (imenu (symbol-name function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun quote-region-text ()
  "Quote the text in the region with '> '"
  (interactive)
  (save-excursion
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (re-search-forward "^" nil t)
      (replace-match "> " nil nil))
    (widen)
))
;(global-set-key "\M-\>" 'quote-region-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(load-library "man")
(defun read-man ()
  "Run the current buffer through nroff -man, and clean up output"
  (interactive)
  (let ((beg (progn (beginning-of-buffer) (point)))
	(end (progn (end-of-buffer) (point))))
    (message "Running nroff...")
    (shell-command-on-region beg end "nroff -man" nil)
    (pop-to-buffer "*Shell Command Output*")
    (message "Cleaning...")
    (nuke-nroff-bs)
    (message "Done")))

;; Code Cleanup for Emacs V1.0
;; http://blog.modp.com/2008/11/handy-emacs-functions-for-code-cleanup.html
;; PUBLIC DOMAIN

;; zap tabs
;;
(defun buffer-untabify ()
  "Untabify an entire buffer"
  (interactive)
  (untabify (point-min) (point-max)))

;;
;; re-indent buffer
;;
(defun buffer-indent()
  "Reindent an entire buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

;;
;; Untabify, re-indent, make EOL be '\n' not '\r\n'
;;   and delete trailing whitespace
;;
(defun buffer-cleanup()
  "Untabify and re-indent an entire buffer"
  (interactive)
  (if (equal buffer-file-coding-system 'undecided-unix )
      nil
    (set-buffer-file-coding-system 'undecided-unix))
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil)
  (buffer-untabify)
  (buffer-indent)
  (delete-trailing-whitespace)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End by setting default font...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(set-default-font "-*-Lucida Console-normal-r-*-*-13-97-96-96-c-*-iso8859-1")
;(set-default-font "-*-Lucida Console-bold-r-*-*-11-90-96-96-c-*-iso8859-1")
;(set-default-font "-*-Lucida Console-normal-r-*-*-11-90-96-96-c-*-iso8859-1")
;(set-default-font "-*-Lucida Console-normal-r-*-*-14-90-96-96-c-*-iso8859-1")
;(set-default-font "-outline-Consolas-bold-r-*-*-11-*-*-*-c-*-iso8859-1")
;(set-default-font "-apple-consolas-bold-r-normal--0-0-0-0-m-0-iso10646-1")
(set-default-font "-apple-Consolas-medium-normal-normal-*-11-*-*-*-m-0-fontset-auto3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From here the file has been modified by emacs itself
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "goldenrod")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "bsd"))))
 '(c-set-style "bsd" t)
 '(c-syntactic-indentation t)
 '(column-number-mode t)
 '(fill-column 78)
 '(filladapt-token-table (quote (("^" beginning-of-line) (">+" citation->) ("\\(\\w\\|[0-9]\\)[^'`\"< 	
]*>[ 	]*" supercite-citation) (";+" lisp-comment) ("#+" sh-comment) ("%+" postscript-comment) ("^[ 	]*\\(//\\|\\*\\)[^ 	]*" c++-comment) ("@c[ \\t]" texinfo-comment) ("@comment[ 	]" texinfo-comment) ("[0-9]+\\.[ 	]" bullet) ("[0-9]+\\(\\.[0-9]+\\)+[ 	]" bullet) ("[A-Za-z]\\.[ 	]" bullet) ("(?[0-9]+)[ 	]" bullet) ("(?[A-Za-z])[ 	]" bullet) ("[0-9]+[A-Za-z]\\.[ 	]" bullet) ("(?[0-9]+[A-Za-z])[ 	]" bullet) ("[-~*+o]+[ 	]" bullet) ("o[ 	]" bullet) ("[\\@]\\(param\\|throw\\|exception\\|addtogroup\\|defgroup\\)[ 	]*[A-Za-z_][A-Za-z_0-9]*[ 	]+" bullet) ("\\\\item[ 	]*" bullet) ("[\\@][A-Za-z_]+[ 	]*" bullet) ("[ 	]+" space) ("$" end-of-line))))
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(mouse-buffer-menu-mode-mult 1)
 '(msb-max-file-menu-items 1)
 '(msb-max-menu-items 35)
 '(msb-mode t)
 '(ns-command-modifier (quote meta))
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-custom-commands (quote (("c" todo #("DONE|CANCELLED" 0 14 (face org-warning)) nil) ("w" todo #("WAITING" 0 7 (face org-warning)) nil) ("W" agenda "" ((org-agenda-ndays 21))) ("A" agenda "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) (org-agenda-ndays 1) (org-agenda-overriding-header "Today's Priority #A tasks: "))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote regexp) "<[^>
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/.todo/todo.org")))
 '(org-agenda-include-diary t)
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy (quote (time-up priority-down)))
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/.todo/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote ((116 "* %?
  %u" "~/.todo/todo.org" "Tasks") (110 "* %u %?" "~/.todo/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(org-tags-match-list-sublevels t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow))))
(european-calendar)

