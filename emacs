;; -*- mode: Emacs-Lisp; fill-column: 78; -*-

;; package management
(add-to-list 'load-path "/Users/mort/.emacs.d")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; evaluate locally if behind nottingham proxy
;(setq url-proxy-services '(("http" . "proxy.nottingham.ac.uk:8080")))

;; non-package-managed libraries
(require 'fill-column-indicator)

;; colours
(set-background-color "black")
(set-foreground-color "gray85")

(set-mouse-color "white")
(set-cursor-color "white")

(set-face-background 'region "grey32")
(set-face-foreground 'region "white")
(set-face-background 'highlight "grey32")
(set-face-foreground 'highlight "white")

;; ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "british")

;; default save encoding- ut8
(set-language-environment "utf-8")
(set-coding-priority (list 'coding-category-utf-8))
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun line-to-top-of-window () "Move line point is on to top of window"
  (interactive)
  (recenter 0)
  )

(defun line-to-bottom-of-window () "Move line point is on to bottom of window"
  (interactive)
  (recenter (frame-height))
  )

(defun warp-to-top-of-window () "Move the point to line 0"
  (interactive)
  (move-to-window-line 0)
  )

(defun warp-to-bottom-of-window () "Move the point to line (frame-height)"
  (interactive)
  (move-to-window-line -1)
  )

(defun reread-dot-emacs () "Re-read ~/.emacs"
  (interactive)
  (load-file "~/.emacs")
  )

(defun dos2unix () "Turn DOS file to Unix"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "\015" "")
    ))

(defun unix2dos () "Turn Unix file to DOS"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "\012" "\015\012")
    ))

(defun mime2unix () "Get rid of MIME =RET junk"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "=\n" "")
    (replace-string "=20" "")
    ))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1))))
  )

(defun my-kill-emacs () "Confirm before save-buffers-kill-emacs"
  (interactive)
  (if (y-or-n-p "Really kill emacs? ")
      (save-buffers-kill-emacs)
    (message "Aborted"))
  )

(defun wc () "Counts the number of words in the region"
  (interactive)
  (shell-command-on-region (point) (mark) "wc")
  )

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;; Takes a multi-line paragraph and makes it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;; Code Cleanup for Emacs V1.0
;;; http://blog.modp.com/2008/11/handy-emacs-functions-for-code-cleanup.html
;;; PUBLIC DOMAIN

;; zap tabs
(defun buffer-untabify ()
  "Untabify an entire buffer"
  (interactive)
  (untabify (point-min) (point-max)))

;; re-indent buffer
(defun buffer-indent()
  "Reindent an entire buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

;; Untabify, re-indent, make EOL be '\n' not '\r\n' and delete trailing
;; whitespace
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

;; prb ispell functions
(defun ispell-check-paragraph () "Spell check each word in a paragraph"
  (interactive "*")
  (let ((ispell-check-only nil)
        (ispell-quietly t)
        )
    (save-excursion
      (forward-paragraph) (setq end (point))
      (forward-paragraph -1) (setq start (point))
      (ispell-region start end))    
    ))

(defun fill-and-check () "Fill a paragraph and spell check"
  (interactive)
  (fill-paragraph nil)
  (ispell-check-paragraph)
  )

;; modified from swiftex.el
(defun tex-enclose-word (before after)
  (interactive "*Mbefore: \nMafter: ")
  (let* ((oldpoint (point))
         (start oldpoint)
         (end oldpoint))

    ;; get the start and end of the current word
    (skip-syntax-backward "w")
    (setq start (point))
    (goto-char oldpoint)
    (skip-syntax-forward "w")
    (setq end (point))
    (if (and (eq start oldpoint) 
             (eq end oldpoint))
        ;; insert the command as nothing to enclose
        (progn (insert before) (insert after) (backward-char))
      
      ;; enclose the word with the command
      (progn 
        (insert after)
        (goto-char start)
        (insert before)
        (goto-char (+ oldpoint (length before)))
        )
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mode hooks

(add-hook 'org-mode-hook 
          '(lambda () 
;             (imenu-add-to-menubar "Imenu")
;             (local-set-key [left] 'org-agenda-later)
             ))

(add-hook 'text-mode-hook
          '(lambda ()
             (fci-mode t)
             (auto-fill-mode 1)
             (flyspell-mode 1)
;             (turn-on-filladapt-mode)
             (local-set-key [M-q] 'fill-and-check)
             ))

(add-hook 'latex-mode-hook
          '(lambda () 
             (local-set-key (kbd "M-q") 'fill-and-check)
;             (local-set-key (kbd "C-c C-b") 'latex-insert-block)
             (local-set-key (kbd "{") 'tex-insert-braces)
             (local-set-key (kbd "M-[") 
                            '(lambda () (interactive) (insert "{")))
             (local-set-key (kbd "M-]") 
                            '(lambda () (interactive) (insert "}")))
             (local-set-key (kbd "C-c m") 
                            '(lambda () (interactive "*") 
                               (tex-enclose-word "\\emph{" "}")))
             (local-set-key (kbd "C-c C-m") 
                            '(lambda () (interactive "*") 
                               (tex-enclose-word "\\emph{" "}")))
             (local-set-key (kbd "C-c b") 
                            '(lambda () (interactive "*") 
                               (tex-enclose-word "{\\bf " "}")))
             ))

(add-hook 'prog-mode-hook
          '(lambda ()
             (local-set-key (kbd "%") 'match-paren)
             ))

(add-hook 'java-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)
             (setq c-set-style "java")
             (fci-mode 1)
             ))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)
             (fci-mode t)
             ))

(add-hook 'lisp-interaction-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)
             ))

(add-hook 'ecmascript-mode-hook
          '(lambda ()
             (fci-mode t)
;             (setq c-basic-offset 4)
;             (c-set-style "java")
             ))

;; xml-mode
(add-hook 'nxml-mode-hook
          '(lambda ()
             (fci-mode t)
             ))
(push '("\\`<\\?xml" . nxml-mode) magic-mode-alist)

;; markdown-mode
(add-hook 'markdown-mode-hook
          '(lambda ()
             (fci-mode t)
             ))
(push '("\\.md$" . markdown-mode) auto-mode-alist)
(push '("\\.markdown$" . markdown-mode) auto-mode-alist)

;; makefile-mode
(add-hook 'makefile-mode-hook
          '(lambda ()
             (fci-mode t)
             ))
(push '("sources$" . makefile-mode) auto-mode-alist)

;; sh-mode
(add-hook 'sh-mode-hook
          '(lambda ()
             (fci-mode t)
             ))

;; python-mode
(add-hook 'python-mode-hook
          '(lambda ()
             (fci-mode t)
             ))

;; tuareg-mode
;; (add-hook 'tuareg-mode-hook
;;           '(lambda ()
;;              (fci-mode 1)
;;              (setq tuareg-lazy-= t) ; indent `=' like a standard keyword
;;              (setq tuareg-lazy-paren t) ; indent [({ like standard keywords
;;              (setq tuareg-in-indent 0) ; no indentation after `in' keywords
;;              (auto-fill-mode 1) ; turn on auto-fill minor mode
;; ;;              (setq tuareg-default-indent 2)
;; ;;              (if (featurep 'sym-lock)   ; Sym-Lock customization only
;; ;;                  (setq sym-lock-mouse-face-enabled nil))
;; ;;                                         ; turn off special face under mouse
;; ;;              (set-face-background caml-types-expr-face "slategray")
;;              ))

;; typerex-mode (ocaml)
(add-hook 'typerex-mode-hook
          (lambda ()
            (fci-mode 1)
            (auto-fill-mode 1)
            ))
(autoload 'typerex-mode "typerex.el" "Major mode for editing Caml code" t)
(push'("\\.ml[iylp]?" . typerex-mode) auto-mode-alist)
(push '("\\.fs[ix]?" . typerex-mode) auto-mode-alist)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make my key-bindings win, except in minibuffer
;; <http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs>

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map (kbd "C-x C-c") 'my-kill-emacs)
(define-key my-keys-minor-mode-map (kbd "C-x p") 
  '(lambda () (interactive) (other-window -1)))
(define-key my-keys-minor-mode-map (kbd "C-c C-g") 'goto-line)
(define-key my-keys-minor-mode-map (kbd "C-c C-c") 'comment-region)
(define-key my-keys-minor-mode-map (kbd "C-c C-q") 'indent-region)
(define-key my-keys-minor-mode-map (kbd "M-C-q") 'unfill-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-%") 'replace-regexp)
(define-key my-keys-minor-mode-map (kbd "C-z") 'nil)
(define-key my-keys-minor-mode-map (kbd "C-<tab>") 'dabbrev-expand)
(define-key my-keys-minor-mode-map (kbd "C-<return>") 'split-line)

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
(define-key my-keys-minor-mode-map (kbd "M-<up>") 'warp-to-top-of-window)
(define-key my-keys-minor-mode-map (kbd "M-<down>") 'warp-to-bottom-of-window)
(define-key my-keys-minor-mode-map (kbd "C-M-<down>") 'line-to-top-of-window)
(define-key my-keys-minor-mode-map (kbd "C-M-<up>") 'line-to-bottom-of-window)

;;
;; for a sensible pc keyboard with pgup|pgdn|home|end
;;
(define-key my-keys-minor-mode-map (kbd "C-<prior>") 'warp-to-top-of-window)
(define-key my-keys-minor-mode-map (kbd "C-<next>") 'warp-to-bottom-of-window)
(define-key my-keys-minor-mode-map (kbd "C-<home>") 'line-to-top-of-window)
(define-key my-keys-minor-mode-map (kbd "C-<end>") 'line-to-bottom-of-window)
(define-key my-keys-minor-mode-map (kbd "<home>") 'beginning-of-buffer) ; also M-<
(define-key my-keys-minor-mode-map (kbd "<end>") 'end-of-buffer) ; also M->

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; org-mode
(define-key mode-specific-map [?a] 'org-agenda)
(defun todo ()
  (interactive)
  (find-file "~/.todo/todo.org")
  )

;; default font

;(set-default-font "-*-Lucida Console-normal-r-*-*-13-97-96-96-c-*-iso8859-1")
;(set-default-font "-*-Lucida Console-bold-r-*-*-11-90-96-96-c-*-iso8859-1")
;(set-default-font "-*-Lucida Console-normal-r-*-*-11-90-96-96-c-*-iso8859-1")
;(set-default-font "-*-Lucida Console-normal-r-*-*-14-90-96-96-c-*-iso8859-1")
;(set-default-font "-outline-Consolas-bold-r-*-*-11-*-*-*-c-*-iso8859-1")
;(set-default-font "-apple-consolas-bold-r-normal--0-0-0-0-m-0-iso10646-1")
(set-default-font "-apple-Consolas-medium-normal-normal-*-11-*-*-*-m-0-fontset-auto3")

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(default-major-mode (quote text-mode) t)
 '(fill-column 78)
 '(frame-title-format "%b  %f" t)
 '(global-visual-line-mode nil)
 '(indent-tabs-mode nil)
 '(interprogram-paste-function (quote x-selection-value) t)
 '(make-backup-files nil)
 '(mouse-buffer-menu-mode-mult 1)
 '(msb-max-file-menu-items 1)
 '(msb-max-menu-items 35)
 '(msb-mode t)
 '(nobreak-char-display t t)
 '(ns-command-modifier (quote meta))
 '(nxml-slash-auto-complete-flag t)
 '(ocp-theme "tuareg_like")
 '(scroll-conservatively 100)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(typerex-font-lock-symbols t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(vc-follow-symlinks t)
 '(visible-bell t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(x-select-enable-clipboard t)
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
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(caml-types-expr-face ((t (:foreground "slategray"))))

 '(font-lock-TRC-face ((t (:foreground "seagreen3"))))
 '(font-lock-alarm-face ((t (:foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "goldenrod"))))
 '(font-lock-define-face ((t (:foreground "aquamarine"))))
 '(font-lock-faded-face ((t (:foreground "slategray"))))
 '(font-lock-function-name-face ((t (:foreground "green"))))
 '(font-lock-globals-face ((t (:foreground "orange"))))
 '(font-lock-keyword-face ((t (:foreground "gold"))))
 '(font-lock-preproc-face ((t (:foreground "aquamarine"))))
 '(font-lock-prototype-face ((t (:foreground "palegreen"))))
 '(font-lock-string-face ((t (:foreground "turquoise"))))
 '(font-lock-type-face ((t (:foreground "orange"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod")))))
