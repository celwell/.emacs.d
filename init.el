(add-to-list 'load-path "~/.emacs.d/lisp")

(setq exec-path (add-to-list 'exec-path "/Users/celwell/bin"))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)

(defvar my-packages '(better-defaults
                      clojure-mode
                      cider
                      ;; coffee-mode
                      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'clojure-mode)

(require 'transpose-frame)

(add-to-list 'load-path "~/.emacs.d/expand-region/")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-to-list 'load-path "~/.emacs.d/php-mode/")
(require 'php-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode 1)

;; show's current position within buffer in modeline
(sml-modeline-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(setq js-indent-level 2)
(setq css-indent-offset 2)

;; coffee-mode
(require 'coffee-mode)
(setq coffee-tab-width 2)

;; make this frame maximized in MS Windows
;; (w32-send-sys-command 61488)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "dist" "development")))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (ace-window avy idle-highlight-mode clj-refactor js2-mode slime-js fireplace rjsx-mode solidity-mode zoom-window zone-nyan web-mode syslog-mode stripe-buffer sml-modeline rich-minority rainbow-delimiters paredit multiple-cursors magit key-chord js-comint how-many-lines-in-project exec-path-from-shell company circe cider better-defaults)))
 '(tool-bar-mode nil)
 '(web-mode-markup-indent-offset 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "outline" :slant normal :weight normal :height 125 :width normal))))
 '(fringe ((t (:background "#2E3436"))))
 '(js2-function-param ((t (:foreground "white"))))
 '(mode-line ((t (:background "#212526" :foreground "#eee" :box (:line-width -1 :color "#212526")))))
 '(mode-line-inactive ((t (:background "#212526" :foreground "#777" :box (:line-width -1 :color "#212526")))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#A0FA67"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "dodger blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "hot pink"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "lawn green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "firebrick1"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "SeaGreen1"))))
 '(vertical-border ((t (:foreground "#212526")))))

(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)

(defun frame-bck()
  (interactive)
  (other-frame -1))
(global-set-key (kbd "C-x 8") 'other-frame)
(global-set-key (kbd "C-x *") 'frame-bck)

(defun window-bck()
  (interactive)
  (other-window -1))
(define-key (current-global-map) (kbd "M-o") 'other-window)
(define-key (current-global-map) (kbd "M-O") 'window-bck)
(define-key (current-global-map) (kbd "C-x O") 'window-bck)

(global-set-key (kbd "C-,") 'hippie-expand)

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(define-key (current-global-map) (kbd "C-a") 'smart-line-beginning)

(define-key (current-global-map) (kbd "C-;") 'comment-or-uncomment-region)

(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "C-q") 'kill-ring-save)

(global-set-key (kbd "M-R") 'revert-buffer)

;; whitespace-mode
;; free of trailing whitespace and to use 80-column width, standard indentation
(setq whitespace-style '(;; trailing
                         lines-tail
                         ;; space-before-tab
                         ;; indentation
                         ;; space-after-tab
                         face)
      whitespace-line-column 180 ;; originally 80, but got annoyed
      )
(put 'downcase-region 'disabled nil)
(global-whitespace-mode t)
(setq whitespace-global-modes '(clojure-mode
                                coffee-mode
                                javascript-mode
                                ;; php-mode
                                ))

(global-linum-mode t)
(set-face-foreground 'linum "#555")

;; scroll up and down without moving point
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

;; Company Mode
(global-company-mode)

;; (setq js2-strict-missing-semi-warning nil)
(setq js2-mode-show-strict-warnings nil)

(require 'js-comint)
(setq inferior-js-program-command "node")
(add-hook 'js2-mode-hook '(lambda () 
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)
                            ))

;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))

;; Multiple Cursors Mode
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun indent-buffer-and-indent ()
  "Indent the currently visited buffer"
  (interactive)
  (indent-region (point-min) (point-max))
  (indent-according-to-mode))

(global-set-key (kbd "C-j") 'newline-and-indent)

(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map [(tab)] 'indent-buffer-and-indent))

(eval-after-load 'clojurescript-mode
  '(define-key clojurescript-mode-map [(tab)] 'indent-buffer-and-indent))

;; start paredit with clojure(script) modes
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojurescript-mode-hook 'paredit-mode)

(require 'clj-refactor)

(defun clj-refactor-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding overwrites cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c"))

(add-hook 'clojure-mode-hook #'clj-refactor-clojure-mode-hook)

(setq cider-save-file-on-load t) ; don't ask to save file (just save it!) when loading via C-c C-k
(setq cider-default-cljs-repl 'figwheel)
;; (setq cider-cljs-lein-repl
;; 	"(do (require 'figwheel-sidecar.repl-api)
;;        (figwheel-sidecar.repl-api/start-figwheel!)
;;        (figwheel-sidecar.repl-api/cljs-repl))")

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-map "4" 'toggle-window-split)

;; Show file path in frame title
(setq-default frame-title-format "%b (%f)")


;; Journaling tools
(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))


(setq avy-all-windows 'all-frames)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(require 'key-chord)
(key-chord-mode 1)
;; (key-chord-define-global "jk" 'backward-char)
;; (key-chord-define-global "kl" 'forward-char)
(key-chord-define-global "jk" 'avy-goto-word-or-subword-1)
(key-chord-define-global "kl" 'ace-window)

(key-chord-define-global "sd" 'delete-backward-char)
(key-chord-define-global "df" 'delete-char)

(key-chord-define-global "nj" 'next-line)
(key-chord-define-global "ji" 'previous-line)

(key-chord-define-global "hu" 'save-buffer)

(key-chord-define-global "l;" 'newline-and-indent)

(key-chord-define-global "xc" 'cider-connect)
(key-chord-define-global "zc" 'cider-connect-cljs)




;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; (require 'rainbow-delimiters)
;; (global-rainbow-delimiters-mode)


(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; IRC
(setq circe-active-users-timeout 180)
(setq circe-reduce-lurker-spam t)


;; Optional convenience binding. This allows C-y to paste even when in term-char-mode (see below). 
(add-hook 'term-mode-hook
          (lambda()
            (define-key term-raw-map
              (kbd "C-y")
              (lambda ()
                (interactive)
                (term-line-mode)
                (yank)
                (term-char-mode)))))

;; https://www.reddit.com/r/emacs/comments/5lybts/prevent_automatically_save_to_kill_ring_when_mark/dc0ut9e/
(setq x-select-enable-primary nil)

;; no auto backups
(setq create-lockfiles nil)
