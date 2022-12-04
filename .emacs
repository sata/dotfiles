(require 'package)

(add-to-list 'package-archives '("elpa"  . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))

;; only do this for bootstrapping a new environment
(unless (file-exists-p "~/.emacs.d/elpa")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; ;; use for debugging slow startup
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(setq
 comp-deferred-compilation t
 custom-file "~/.emacs.d/.emacs-custom.el")

(load custom-file)

(add-to-list 'auto-mode-alist '("\\.common\\'" . makefile-mode))

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun delete-current-file ()
  "Helper to delete currently opened file and kill buffer."
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
        (if (y-or-n-p (format "delete file: %s?" fname))
            (progn
              (delete-file fname)
              (kill-current-buffer)
              (message "file deleted: %s" fname)))
      (message "not a file buffer"))))

;; disable trailing whitespace where it's annoying
(setq show-trailing-whitespace t)
(dolist (hook '(term-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package consult
  :ensure t
  :bind
  (("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; init so we bind before lsp-mode binds
(use-package consult-lsp
  :ensure t
  :bind
  (("s-o d" . consult-lsp-diagnostics)
   ("s-o s" . consult-lsp-symbols)
   ("s-o f" . consult-lsp-file-symbols))
  :init)

(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult-projectile
  :ensure t)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package vterm
  :ensure t)

(use-package anzu
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t)

(use-package yaml
  :ensure t
  :defer t)

(use-package terraform-mode
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-start-animation)
  (nyan-mode t))

(use-package projectile
  :ensure t
  :defer t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode))

(use-package go-mode
  :defer t
  :ensure t
  :hook (go-mode . (lambda()
                     (flycheck-golangci-lint-setup)
                     (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint)))))))))

(use-package dap-mode
  :ensure t
  :defer t
  :config
  (setq dap-auto-configure-features '()) ;; prefer hydra
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (add-to-list 'display-buffer-alist
               '("^\\*Test function.*server log\\*.*" display-buffer-no-window)))

(use-package dap-dlv-go
  :after dap-mode)

;; required for code completion
(use-package yasnippet
  :defer t
  :ensure t
  :hook ((lsp-mode . yas-minor-mode)))

;; hack because lsp-mode is not handling the lsp-keymap-prefix nicely
;; https://github.com/emacs-lsp/lsp-mode/issues/1672
(setq lsp-keymap-prefix "s-o")
;; (define-key lsp-mode-map (kbd lsp-keymap-prefix) lsp-command-map)

(defun lsp-go-install-save-hooks ()
  "Save hooks for go mode."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp lsp-deferred
  :config
  (setq lsp-keymap-prefix "s-o")
  (setq lsp-go-use-gofumpt t)
  (setq lsp-go-env '((GOFLAGS . "-mod=vendor")))
  (setq lsp-file-watch-threshold 10000)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-go-analyses
        '((nilness        . t)
          (shadow         . t)
          (unusedparams   . t)
          (unusedwrite    . t)
          (useany         . t)
          (unusedvariable . t)))
  (setq debug-on-error nil)
  (setq lsp-terraform-server `(,"terraform-ls" "serve"))
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  :hook (
         (go-mode        . lsp-deferred)
         (go-mode        . lsp-go-install-save-hooks)
         (yaml-mode      . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (lsp-mode       . lsp-enable-which-key-integration)))

;; https://github.com/weijiangan/flycheck-golangci-lint/issues/8
(defvar-local flycheck-local-checkers nil)

(defun +flycheck-checker-get(fn checker property)
  "Get CHECKER from buffer-local var before asking FN.
Provides a way for modes to hook their checkers in."
  (or (alist-get property (alist-get checker flycheck-local-checkers))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

(use-package flycheck-golangci-lint
  :defer t
  :ensure t
  :config
  ;; (setq flycheck-golangci-lint-enable-all t) ;; rely more on available lint file.
  (setq flycheck-golangci-lint-fast t))

;; Optional - provides fancier overlays.
(use-package lsp-treemacs
  :ensure t
  :defer t
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ui
  :ensure t
  :defer t
  :config
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq lsp-completion-provider :capf)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-global-modes '(not org-mode not sh-mode not eshell-mode not debugger-mode not latex-mode))
  :init
  (global-company-mode t))

(use-package k8s-mode
  :ensure t
  :defer t
  :hook (k8s-mode . yas-minor-mode))

(setq org-roam-v2-ack t)
(setq org-roam-directory "~/org/roam")
(use-package org-roam
  :ensure t
  :defer t
  :custom
  (
   (org-roam-dailies-directory "daily/")
   (org-roam-v2-ack t)
   ;; tags display don't work nicely with ido, i'm not using it
   ;; either so lets just display title
   (org-roam-node-display-template "${title}")
   (org-roam-dailies-capture-templates
    '(("d" "default" entry
       "* %?"
       :if-new (file+head "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>\n\n[[id:aa4e2c92-c164-44e8-9491-38e57084b61f][dailies]]\n\n* Tasks:\n  - [ ] "))))
   )

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c d r" . org-roam-dailies-goto-tomorrow)
         ("C-c d t" . org-roam-dailies-goto-today)
         ("C-c d y" . org-roam-dailies-goto-yesterday)
         ("C-C d p" . org-roam-dailies-goto-previous-note)
         ("C-C d n" . org-roam-dailies-goto-next-note)
         )
  :config
  (org-roam-setup))

(use-package tex-mode
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (TeX-fold-mode 1)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'"     . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode)))

(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setq plantuml-jar-path "/home/s/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type '"png")
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.puml\\'"     . plantuml-mode)))

(use-package ccls
  :ensure t
  :defer t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

;; rust
(use-package toml-mode
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :defer t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer)))))

;; nicked from https://stackoverflow.com/questions/915985/
;; Align with spaces only
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

(use-package org-roam-ui
  :ensure t
  :defer t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start t))

;; -------
;; hack
;; stuff I used that are not on ELPA, but only matter on a fully blown
;; workstation, not Vagrant

;; only care about presentations when I've got it cloned
;; helps vagrant setups
;; (setq ox-reveal-path "~/sources/org-reveal/ox-reveal.el")
;; (when (file-exists-p ox-reveal-path)
;;       (load ox-reveal-path)
;;       (use-package ox-reveal
;;         :defer t
;;         :config
;;         (setq org-reveal-root "file:///home/s/sources/reveal.js")))
