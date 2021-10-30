(require 'package)

(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

;; only do this for bootstrapping a new environment
(unless (file-exists-p "~/.emacs.d/elpa")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq
 comp-deferred-compilation t
 custom-file "~/.emacs.d/.emacs-custom.el")

(load custom-file)

(use-package cyberpunk-theme
  :ensure t
  :config
  (load-theme 'cyberpunk t))

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key (kbd "<f12>") #'deadgrep)

(defalias 'yes-or-no-p 'y-or-n-p)

;; disable trailing whitespace where it's annoying
(setq show-trailing-whitespace t)
(dolist (hook '(term-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

(use-package vterm
  :ensure t)

(use-package deadgrep
  :ensure t)

(use-package magit
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

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
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode))

(use-package go-mode
  :ensure t)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-mode
  :ensure t
  :commands lsp lsp-deferred
  :init
  (setq lsp-keymap-prefix "s-o")
  (setq lsp-file-watch-threshold 10000)
  (setq lsp-enable-file-watchers t)
  (setq debug-on-error nil)
  :hook (
         (go-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-flycheck-enable 1)
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(setq company-global-modes '(not org-mode not shell-mode eshell-mode not debugger-mode))

(use-package company
  :ensure t
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

(setq org-roam-v2-ack t)
(setq org-roam-directory "~/org/roam")
(use-package org-roam
      :ensure t
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
                              "#+title: %<%Y-%m-%d>\n\n* Tasks:\n  - [ ] "))))
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
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  )

(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path "/home/s/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type '"png")
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode)))

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

;; rust
(use-package toml-mode
  :ensure t)
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer)))))

(use-package yaml
  :ensure t)

;; nicked from https://stackoverflow.com/questions/915985/
;; Align with spaces only
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

;; -------
;; hack
;; stuff I used that are not on ELPA, but only matter on a fully blown
;; workstation, not Vagrant
(setq org-roam-ui-path  "~/.emacs.d/slask/org-roam-ui")
(when (file-exists-p org-roam-ui-path)
  (add-to-list 'load-path "~/.emacs.d/slask/org-roam-ui")
  (use-package simple-httpd
    :ensure t)
  (use-package websocket
    :ensure t)
  (use-package org-roam-ui
    :after org-roam
    :init
    (setq org-roam-ui-sync-theme t)
    (setq org-roam-ui-follow t)
    (setq org-roam-ui-update-on-save t)
    (setq org-roam-ui-open-on-start t)
    :config
    (load-library "org-roam-ui"))
  )

;; only care about presentations when I've got it cloned
;; helps vagrant setups
(setq ox-reveal-path "~/sources/org-reveal/ox-reveal.el")
(when (file-exists-p ox-reveal-path)
      (load ox-reveal-path)
      (use-package ox-reveal
        :init
        (setq org-reveal-root "file:///home/s/sources/reveal.js")
        :config))
