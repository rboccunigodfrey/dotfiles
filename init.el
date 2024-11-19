; --- VARIABLES ---
(setq custom-file "~/.emacs.d/custom.el")

;; --- FUNCTIONS ---
(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
	(line (let ((s (thing-at-point 'line t)))
		(if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))


;; --- MODE SETTINGS ---
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1) 
(fido-mode 1)

;; --- PACKAGE LOADING ---

;; custom package location
(add-to-list 'load-path "~/.emacs.d/emacs.local")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; load custom packages
(require 'simpc-mode)
(require 'odin-mode)

;; existing packages
(use-package magit :ensure t)
(use-package multiple-cursors :ensure t)
(use-package jupyter :ensure t)
(use-package rust-mode :ensure t)

;; --- CONFIG ---

;; simpc-mode filetype config
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; --- KEY SETTINGS ---

;; multiple cursors config
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this)

;; rust keys
(global-set-key (kbd "C-M-=") 'rust-run)

;; dupe line key
(global-set-key (kbd "C-,") 'rc/duplicate-line)

;; load custom file
(load-file custom-file)
