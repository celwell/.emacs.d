(add-to-list 'load-path "~/.emacs.d/")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)

(defvar my-packages '(better-defaults
                      clojure-mode
                      clojure-test-mode
                      cider
                      ;; coffee-mode
                      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
(setq-default tab-width 4)

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
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 90 :width normal))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#A0FA67"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "dodger blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "hot pink"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "lawn green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "firebrick1"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "SeaGreen1")))))

(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)

(global-set-key (kbd "C-x 8") 'other-frame)

(defun frame-bck()
  (interactive)
  (other-window -1))
(define-key (current-global-map) (kbd "M-o") 'other-window)
(define-key (current-global-map) (kbd "M-O") 'frame-bck)
(define-key (current-global-map) (kbd "C-x O") 'frame-bck)

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
      whitespace-line-column 80)
(put 'downcase-region 'disabled nil)
(global-whitespace-mode t)
(setq whitespace-global-modes '(clojure-mode
                                coffee-mode
                                javascript-mode
                                ;; php-mode
                                ))

;; scroll up and down without moving point
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")


;; Multiple Cursors Mode
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


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


(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'backward-char)
(key-chord-define-global "kl" 'forward-char)

(key-chord-define-global "sd" 'delete-backward-char)
(key-chord-define-global "df" 'delete-char)

(key-chord-define-global "nj" 'next-line)
(key-chord-define-global "ji" 'previous-line)

(key-chord-define-global "hu" 'save-buffer)

(key-chord-define-global "l;" 'newline-and-indent)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)


;; MS Windows only stuff
(when (string-equal system-type "windows-nt")
  (progn
    (setenv "PATH" (concat "c:\\Program Files\\Git\\bin;" (getenv "PATH")))
    (setq exec-path (add-to-list 'exec-path "c:\\Program Files\\Git\\bin"))
    (setq-default ispell-program-name "C:\\Program Files\\Aspell\\bin\\aspell.exe")
    (setq text-mode-hook '(lambda()
                            (flyspell-mode t)
                            ))))


;; for Cygwin shell
;; (when (string-equal system-type "windows-nt")
;;   (progn
;;     (setenv "PATH" (concat  "c:\\cygwin\\bin;"  (getenv "PATH")))
;;     (setq exec-path (append exec-path '("c:\\cygwin\\bin")))))
