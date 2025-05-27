;--- VARIABLES ---
(setq custom-file "~/.emacs.d/custom.el")
(setq treesit-language-source-alist
      '((c3 "https://github.com/c3lang/tree-sitter-c3")))
(setq treesit-font-lock-level 4)
(setq inhibit-splash-screen t)

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p))

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


;; --- PACKAGE LOADING ---

(require 'package)

;; custom package location
(add-to-list 'load-path "~/.emacs.d/emacs.local")
;(add-to-list 'load-path "/home/gimli/.opam/4.14.0/share/emacs/site-lisp")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; themes
(use-package horizon-theme :ensure t)
(use-package dracula-theme :ensure t)
;; existing packages

(use-package auctex :ensure t)

(with-eval-after-load 'auctex
  (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LaTeXmk")))
  (setq TeX-source-correlate-mode t))

(use-package reftex :ensure t)
(use-package company-auctex :ensure t)
(use-package company-reftex :ensure t)
(use-package company-math :ensure t)

(use-package pdf-tools :ensure t
  :init
  (pdf-tools-install)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-resize-factor 1.05)
  :bind (:map pdf-view-mode-map
              ("<left>" . pdf-view-previous-page-command)
              ("<right>" . pdf-view-next-page-command)))


(use-package emms :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package vertico :ensure t)
(use-package consult :ensure t)
(use-package marginalia :ensure t)
(use-package embark :ensure t)
(use-package embark-consult :ensure t)
(use-package lua-mode :ensure t)
(use-package company :ensure t)

(with-eval-after-load 'company
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'odin-mode-hook 'company-mode)
  (add-hook 'zig-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

(use-package cape :ensure t
  :bind ("C-c p" . cape-prefix-map)

  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package which-key :ensure t)
(use-package avy :ensure t)
(use-package magit :ensure t)
(use-package multiple-cursors :ensure t)
(use-package jupyter :ensure t)
(use-package cc-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package platformio-mode :ensure t)
(use-package lsp-mode :ensure t)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.tex$" . "texlib"))
  (add-to-list 'lsp-language-id-configuration '(".*\\.zig$" . "zls")))

(use-package lsp-ui :ensure t)
(use-package dap-mode :ensure t)

(with-eval-after-load 'dap-mode
  (dap-mode 1)

  ;; The modes below are optional

  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1))

(require 'dap-gdb) 					;
					; or
(use-package ccls :ensure t
  :hook ((c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))
(setq ccls-executable "ccls")


(use-package eglot :ensure t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) .
			       		("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs '((c-mode simpc-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((fennel-mode) "fennel-ls"))
  (add-to-list 'eglot-server-programs '((odin-mode) "ols"))
  (add-to-list 'eglot-server-programs '((c3-ts-mode) "c3lsp"))
  (add-to-list 'eglot-server-programs '((LaTeX-mode) "texlab"))
  (add-to-list 'eglot-server-programs '((zig-mode) "zls"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'simpc-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode 'eglot-ensure)
  (add-hook 'fennel-mode 'eglot-ensure)
  (add-hook 'odin-mode 'eglot-ensure)
  (add-hook 'c3-ts-mode 'eglot-ensure)
  (add-hook 'zig-mode-hook 'eglot-ensure)
  )

;;(use-package org-roam :ensure t)
(use-package eat :ensure t)
(use-package ligature
  :ensure t
  :load-path "https://github.com/mickeynp/ligature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;;Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package svelte-mode :ensure t)

(use-package rust-mode :ensure t)


(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; load custom packages

(require 'simpc-mode)
;(require 'ocp-indent)
;(load "/home/gimli/.opam/4.14.0/share/emacs/site-lisp/tuareg-site-file")
;
;(use-package opam-switch-mode
;  :ensure t
;  :hook
;  ((coq-mode tuareg-mode) . opam-switch-mode));
;
;
;(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
;  (when (and opam-share (file-directory-p opam-share))
;    ;; Register Merlin
;    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;    (autoload 'merlin-mode "merlin" nil t nil)
;    ;; Automatically start it in OCaml buffers
;    (add-hook 'tuareg-mode-hook 'merlin-mode t)
;    (add-hook 'caml-mode-hook 'merlin-mode t)
;    ;; Use opam switch to lookup ocamlmerlin binary
;    (setq merlin-command 'opam)
;    ;; To easily change opam switches within a given Emacs session, you can
;    ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
;    ;; and use one of its "OPSW" menus.
;    ))

(use-package fennel-mode
  :mode ("\\.fnl\\'". fennel-mode))

(use-package c3-ts-mode
  :mode ("\\.c3\\'" . c3-ts-mode))

(defun my-c3-ts-compile ()
  "Execute 'c3c compile-run' on current file in directory."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (compile (concat "c3c compile-run -l lib/raylib/lib/libraylib.a " file " raylib.c3"))
      (message "Buffer is not visiting a file!"))))

(defun my-c3-ts-proj-run ()
  "Execute 'c3c run' in current directory"
  (interactive)
  (compile "c3c run"))

(defun my-c3-ts-mode-setup ()
  "Setup custom keybindings for c3-ts-mode."
  (local-set-key (kbd "C-c c") 'my-c3-ts-compile)
  (local-set-key (kbd "C-c x") 'my-c3-ts-proj-run))
  
(add-hook 'c3-ts-mode-hook 'my-c3-ts-mode-setup)

(defun my-c-ts-compile ()
  "Compile nob in current directory."
  (interactive)
  (compile "./nob"))

(defun my-c-ts-execute ()
  "Execute ./main in current directory."
  (interactive)
  (compile "./main"))

(defun my-c-ts-mode-setup ()
  "Setup custom keybindings for c-ts-mode."
  (local-set-key (kbd "C-c C-c c") 'my-c-ts-compile)
  (local-set-key (kbd "C-c C-c x") 'my-c-ts-execute)
  (local-set-key (kbd "C-c C-c d") 'dap-hydra))


(add-hook 'simpc-mode-hook 'my-c-ts-mode-setup)
(add-hook 'c-mode-hook 'my-c-ts-mode-setup)


(use-package odin-mode :mode ("\\.odin\\'" . odin-mode))

(defun om/odin-run ()
  "Run 'odin run .' in compilation buffer"
  (interactive)
  (compile "odin run . -o:speed"))

(defun my-odin-mode-setup ()
  (local-set-key (kbd "C-c C-c") 'om/odin-run))

(add-hook 'odin-mode-hook 'my-odin-mode-setup)


(use-package zig-mode :ensure t)

;; --- MODE SETTINGS ---

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1) 
(vertico-mode 1)
(marginalia-mode 1)
(delete-selection-mode 1)
(recentf-mode 1)
(which-key-mode 1)

;; --- CONFIG ---

;; simpc-mode filetype config
;(add-to-list 'auto-mode-alist '("\\.[hc]\\(\\)?\\'" . simpc-mode))

;; --- KEY SETTINGS ---

;; multiple cursors config
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-s l") 'consult-line)
(define-key isearch-mode-map (kbd "M-j") 'avy-isearch)

(global-set-key [f7] (lambda () (interactive) (find-file "~/.emacs.d/init.el")))


;; rust keys
(global-set-key (kbd "C-M-=") 'rust-run)

;; dupe line key
(global-set-key (kbd "C-,") 'rc/duplicate-line)

;; load custom file
(load-file custom-file)



(setq ring-bell-function 'ignore)
(put 'list-threads 'disabled nil)
