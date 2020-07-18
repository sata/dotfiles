;; (unless package--initialized (package-initialize)) ;; emacs27 does this before evaluating user config file

(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; don't think I need this for emacs27
;; (unless package-archive-contents
;;   (package-refresh-contents))

(unless (package-installed-p 'package+)
  (package-install 'package+))

(package-manifest 'ag
                  'expand-region
                  'magit
                  'cl-lib
		              'cyberpunk-theme
		              'flycheck
                  'company
                  'nyan-mode
                  'ssh
                  'paredit
                  'highlight-symbol
                  'markdown-mode
                  'projectile
                  'python-mode
                  'ruby-mode
                  'cc-mode

                  ;; go
                  'go-mode
                  'go-projectile
                  'company-go
                  'gotest
                  'go-tag
                  'flycheck-golangci-lint

                  'web-mode
		              'package+
                  ;; lsp
                  'lsp-mode
		              'lsp-ui
                  'company-lsp
                  'lsp-treemacs
                  'helm-lsp
                  'use-package
                  'yasnippet
                  'eglot

                  'yaml-mode

                  ;; elixir
                  'exunit
                  'elixir-mode

                  ;; c
                  'ccls

                  ;; rust
                  'toml-mode
                  'rust-mode
                  'cargo
                  'flycheck-rust
                  )

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".emacs" ".org" ".md"))
(ido-mode 1)
;; remember C-f when it interfers with new files, renaming files etc

(require 'package+)
(require 'nyan-mode)
(require 'highlight-symbol)
(global-company-mode)
(require 'elixir-mode)

(require 'projectile)
(require 'cc-mode)
;; (require 'go-flycheck)
(require 'go-projectile)
(require 'company)
(require 'go-mode)
(require 'company-go)
(require 'uniquify)
(require 'lsp-mode)
(require 'eglot)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-global-mode)
(nyan-mode)
(nyan-start-animation)

(defun better-defaults ()
  (progn
    (tool-bar-mode -1) (scroll-bar-mode -1)
;;    (defalias 'yes-or-no-p 'y-or-n-p)
    (setq x-select-enable-clipboard t)
;;    (setq-default save-place t) (require 'saveplace)
    (global-set-key (kbd "M-/") 'hippie-expand)
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (setq apropos-do-all t)))

(better-defaults)

(setq inhibit-default-init t)
(setq inhibit-startup-message t)
(global-font-lock-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq line-number-mode               t)
(setq column-number-mode             t)
;; (split-window-horizontally)
;; (balance-windows)
(global-hi-lock-mode 1)

(setq-default
 tab-width 2
 standard-indent 2
 indent-tabs-mode nil)

(setq c-basic-offset 2)

;; Python Hook 4 space tab
(add-hook 'python-mode-hook
      (lambda ()
        (setq python-indent 2)))

;;; cperl-mode is preferred to perl-mode
;;; "Brevity is the soul of wit" <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)

(setq cperl-indent-level 2)
(setq cperl-invalid-face (quote off))

(setq default-frame-alist
      '( (font . "DejaVu Sans Mono-10")
         ;; (font . "-B&H-LucidaTypewriter-Medium-R-Normal-Sans-13-*-*-*-*-*-*-*")
        ;; (font . "-DAMA-Ubuntu Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
	))

(load-theme 'cyberpunk t)

;; parenthesis matching
(show-paren-mode 1)
(setq show-paren-delay 0)

;; erlang related
(defun set-erlang-dir (dir)
  (let ((bin-dir (expand-file-name "bin" dir))
        (tools-dirs (file-expand-wildcards
                     (concat dir "/lib/tools-*/emacs"))))

    (when tools-dirs
      (add-to-list 'load-path (car tools-dirs))
      (add-to-list 'exec-path bin-dir)
      (defvar erlang-electric-commands
        '(erlang-electric-comma
          erlang-electric-semicolon
          erlang-electric-gt
          erlang-electric-newline))
      (setq erlang-root-dir dir)
      (require 'erlang-start))))

(defun erl-root ()
   (shell-command-to-string "erl -boot start_clean -noinput -noshell -eval 'io:format(os:getenv(\"ROOTDIR\")),halt().'"))

(set-erlang-dir (erl-root))

(setq erlang-indent-level 2)

(defun my-erlang-mode-hook ()
  ;; when starting an Erlang shell in Emacs, default in the node name
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")
  ;; customize keys
  (local-set-key [return] 'newline-and-indent)
  )
;; Some Erlang customizations
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;; (require 'flymake)
;; (defun flymake-erlang-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-inplace))
;; 	 (local-file (file-relative-name temp-file
;; 		(file-name-directory buffer-file-name))))
;;     (list "~/.emacs.d/check_erlang.erl" (list local-file))))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-default-notes-file "~/org/notes.org")
(setq org-log-done t)

(global-set-key [(f6)] (lambda () (interactive) (erlang-man-function (current-word))))

(setq-default show-trailing-whitespace t)

;; proper copy clipboard
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)


(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(setq reftex-plug-into-AUCTeX t)

;; Add F12 to toggle line wrap
(global-set-key (kbd "<f12>") 'toggle-truncate-lines)

;; tramp
;; (require 'tramp)
(setq tramp-default-method "ssh")

(setq lpr-command "xpp")

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; plantuml
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(setq plantuml-output-type `"ascii")

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))


;; lsp  --------------------------------------------------------------

(setq lsp-file-watch-threshold 10000)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(setq company-global-modes '(not org-mode not shell-mode eshell-mode not debugger-mode))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

(setq lsp-ui-flycheck-enable 1)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

;; elixir related
(add-hook 'elixir-mode-hook 'eglot-ensure)
(add-to-list 'auto-mode-alist '("\\.eex$" . web-mode))
(add-to-list 'elixir-mode-hook 'company)
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t)))
(add-hook 'elixir-mode-hook #'lsp)
(add-to-list 'eglot-server-programs `(elixir-mode "language_server.sh"))


;; ccls
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(require 'ccls)
(setq ccls-executable "ccls")

(setq ring-bell-function 'ignore)

;; rust
(use-package toml-mode)
(use-package rust-mode
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                         (lsp-format-buffer))))

;; nicked from https://stackoverflow.com/questions/915985/
;; Align with spaces only
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

;; ---------------------------------------------------------------------

(projectile-mode 1)
(go-projectile-tools-add-path)

;; (ac-config-default)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ag cargo cc-mode ccls cl-lib company company-go company-lsp cyberpunk-theme eglot elixir-mode expand-region exunit flycheck flycheck-golangci-lint flycheck-rust go-mode go-projectile go-tag gotest helm-lsp highlight-symbol lsp-mode lsp-treemacs lsp-ui magit markdown-mode nyan-mode package+ paredit projectile python-mode ruby-mode rust-mode ssh toml-mode use-package web-mode yaml-mode yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
