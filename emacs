;; -*- mode: Emacs-Lisp; fill-column: 78; -*-

(add-to-list 'load-path "/Users/mort/.emacs.d")
;(setq-default word-wrap t)
;(global-visual-line-mode t)

;; package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; evaluate locally if behind nottingham proxy
;(setq url-proxy-services '(("http" . "proxy.nottingham.ac.uk:8080")))
;(setq url-proxy-services '(("http" . "wwwcache.cs.nott.ac.uk:3128")))

;; non-package-managed libraries
(require 'fill-column-indicator)
(require 'filladapt)
(require 'scroll-in-place)

;; colours
(require 'color-theme)
(color-theme-sanityinc-solarized-dark)

(defun light () "light colour scheme" 
  (interactive)
  (color-theme-sanityinc-solarized-light)
  )
(defun dark () "dark colour scheme" 
  (interactive)
  (color-theme-sanityinc-solarized-dark)
  )

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

(defun todo ()
  (interactive)
  (find-file "~/.todo/todo.org")
  )
(defun notes ()
  (interactive)
  (find-file "~/.todo/notes.org")
  )

(require 'calendar)
(defun insdate-insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil
                                omit-day-of-week-p))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mode hooks

(add-hook 'text-mode-hook
          '(lambda ()
             (fci-mode t)
             (auto-fill-mode 1)
             (flyspell-mode 1)
;             (longlines-mode 1)
             (turn-on-filladapt-mode)
;             (setq word-wrap t)
;             (turn-on-filladapt-mode)
             (local-set-key (kbd "M-q") 'fill-and-check)
             (local-set-key (kbd "S-<tab>") 'flyspell-auto-correct-previous-word)
             ))

(add-hook 'prog-mode-hook
          '(lambda ()
             (flyspell-prog-mode)
             (local-set-key (kbd "%") 'match-paren)
             ))

(add-hook 'org-mode-hook 
          '(lambda () 
             (local-set-key (kbd "C-c a") 'org-agenda)
             (local-set-key (kbd "S-<up>") 'org-move-line-up)
             (local-set-key (kbd "S-<down>") 'org-move-line-down)
             (local-unset-key (kbd "S-<tab>"))
             (local-set-key (kbd "S-C-<tab>") 'flyspell-auto-correct-previous-word)
             (local-set-key (kbd "C-x C-d") `insdate-insert-current-date)
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
(push '("\\.tex$" . latex-mode) auto-mode-alist)

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
(push '("bash_" . sh-mode) auto-mode-alist)

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

;; org-mode: Holidays -- from <http://www.gnomon.org.uk/diary.html>

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make my key-bindings win, except in minibuffer
;; <http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs>

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map (kbd "C-x C-c") 'my-kill-emacs)
(define-key my-keys-minor-mode-map (kbd "C-x p") 
  '(lambda () (interactive) (other-window -1)))
(define-key my-keys-minor-mode-map (kbd "C-c C-g") 'goto-line)
(define-key my-keys-minor-mode-map (kbd "C-c ;") 'comment-region)
(define-key my-keys-minor-mode-map (kbd "C-c C-q") 'indent-region)
(define-key my-keys-minor-mode-map (kbd "M-C-q") 'unfill-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-%") 'replace-regexp)
(define-key my-keys-minor-mode-map (kbd "C-z") 'nil)
(define-key my-keys-minor-mode-map (kbd "C-<tab>") 'dabbrev-expand)
(define-key my-keys-minor-mode-map (kbd "C-<return>") 'split-line)

;; yes, the keybindings are counter intuitive. but hey, life is a stack.
(define-key my-keys-minor-mode-map (kbd "M-p") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "M-n") 'previous-buffer)

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

;; default font

(set-default-font "-apple-Consolas-medium-normal-normal-*-11-*-*-*-m-0-fontset-auto3")

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-hscroll-mode nil)
 '(calendar-bahai-all-holidays-flag nil)
 '(calendar-christian-all-holidays-flag t)
 '(calendar-date-style (quote iso))
 '(calendar-mark-holidays-flag t)
 '(column-number-mode t)
 '(default-major-mode (quote text-mode) t)
 '(fill-column 78)
 '(frame-title-format "%b  %f" t)
 '(holiday-bahai-holidays nil)
 '(holiday-hebrew-holidays nil)
 '(holiday-islamic-holidays nil)
 '(holiday-oriental-holidays nil)
 '(holiday-other-holidays (quote ((holiday-float 1 1 3 "Martin Luther King Day") (holiday-float 2 1 3 "President's Day") (holiday-float 5 1 -1 "Memorial Day") (holiday-fixed 7 4 "Independence Day") (holiday-float 9 1 1 "Labor Day") (holiday-float 10 1 2 "Columbus Day") (holiday-fixed 11 11 "Veteran's Day") (holiday-float 11 4 4 "Thanksgiving"))))
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
 '(ocp-theme "tuareg_like" t)
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
 '(org-remember-templates 
   (quote ((116 "* %? %u" "~/.todo/todo.org" "Tasks") 
           (110 "* %u %?" "~/.todo/notes.org" "Notes")))
   )
 '(org-reverse-note-order t)
 '(org-tags-match-list-sublevels t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(scroll-conservatively 100)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(vc-follow-symlinks t)
 '(visible-bell t)
 '(x-select-enable-clipboard t)
 '(x-stretch-cursor t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
