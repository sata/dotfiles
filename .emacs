(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(setq comp-deferred-compilation t)

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

                  'go-mode
                  'go-projectile
                  'company-go
                  'gotest
                  'go-tag
                  'flycheck-golangci-lint

                  'web-mode
		              'package+

                  'lsp-mode
		              'lsp-ui
                  'company-lsp
                  'lsp-treemacs
                  'helm-lsp
                  'use-package
                  'yasnippet
                  'eglot

                  'yaml-mode

                  'exunit
                  'elixir-mode

                  'ccls

                  'rust-mode
                  'toml-mode
                  'cargo
                  'flycheck-rust

                  'lua-mode

                  'plantuml-mode
                  'flycheck-plantuml

                  'terraform-mode

                  'nix-mode

                  'vterm
                  'org-roam)


;; general config
(load-theme 'cyberpunk t)
(nyan-mode)
(nyan-start-animation)

(global-set-key (kbd "<f12>") 'toggle-truncate-lines)
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

(tool-bar-mode -1)
(scroll-bar-mode -1)


(setq-default
 x-select-enable-clipboard t
 apropos-do-all t
 inhibit-default-init t
 inhibit-startup-message t
 tab-width 2
 standard-indent 2
 indent-tabs-mode nil
 show-trailing-whitespace t
 show-paren-delay 0
 c-basic-offset 2

 default-frame-alist
      '( (font . "-CTDB-Fira Code-semibold-normal-normal-*-13-*-*-*-d-0-iso10646-1")
       ))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-display-line-numbers-mode)
(global-hi-lock-mode 1)
(global-font-lock-mode t)
(show-paren-mode 1)
(global-company-mode)

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(require 'package+)
(require 'nyan-mode)
(require 'highlight-symbol)
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

(setq-default
 ido-enable-flex-matching t
 ido-everywhere t
 ido-use-filename-at-point 'guess
 ido-create-new-buffer 'always
 ido-file-extensions-order '(".emacs" ".org" ".md"))
(ido-mode 1)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-global-mode)

;; perl
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 2)
(setq cperl-invalid-face (quote off))

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(setq org-agenda-files (list "~/org/"))
(setq org-default-notes-file "~/org/notes.org")
(setq org-log-done t)

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/org/roam")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(setq org-roam-completion-system 'ido)

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(setq reftex-plug-into-AUCTeX t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; plantuml
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

(setq plantuml-jar-path "/home/s/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-output-type '"txt")

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

;; terraform
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
                  :major-modes '(terraform-mode)
                  :server-id 'terraform-ls))

(add-hook 'terraform-mode-hook #'lsp)

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

;; (ac-config-default)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ag cargo cc-mode ccls cl-lib company company-go company-lsp cyberpunk-theme eglot elixir-mode expand-region exunit flycheck flycheck-golangci-lint flycheck-plantuml flycheck-rust go-mode go-projectile go-tag gotest helm-lsp highlight-symbol lsp-mode lsp-treemacs lsp-ui lua-mode magit markdown-mode nix-mode nyan-mode org-roam package+ paredit plantuml-mode projectile python-mode ruby-mode rust-mode ssh terraform-mode toml-mode use-package vterm web-mode yaml-mode yasnippet))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
