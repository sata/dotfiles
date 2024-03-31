(straight-use-package 'use-package)

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

(add-to-list 'display-buffer-alist
             '("\\*Shell Command Output\\*.*"
               (cons #'display-buffer-no-window nil)))

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
  ;; (load-theme 'gruvbox-light-medium t))
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
   ("M-s k" . consult-keep-lines)
   ("C-x b" . consult-buffer)
   ("M-g i" . consult-imenu))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Drop embark--confirm from kill-buffeammoc rnd

  :config
  (setq embark-pre-action-hooks
        (assoc-delete-all 'kill-buffer embark-pre-action-hooks))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  :defer t
  :bind
  ("C-x m" . magit))

(use-package forge
  :ensure t
  :after magit)

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
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode))

(use-package go-mode
  :defer t
  :ensure t)

;; required for code completion
(use-package yasnippet
  :ensure t)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(defun eglot-format-buffer-on-save ()
  "Format buffer on save."
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))


(use-package breadcrumb
  :load-path "~/.emacs.d/lisp"

  :config
  (setq which-func-functions #'(breadcrumb-imenu-crumbs))

  :hook
  ((go-mode . breadcrumb-mode)
   (c-mode . breadcrumb-mode)
   (c++-mode . breadcrumb-mode)
   (yaml-mode . breadcrumb-mode)
   (terraform-mode . breadcrumb-mode)
   (rust-mode . breadcrumb-mode)
   (python-mode . breadcrumb-mode)))

(use-package gotest
  :straight (gotest :type git :host github :repo "nlamirault/gotest.el"
                    :fork (:host github :repo "sata-form3/gotest.el"))
  :config
  (setq go-test-verbose t)
  (setq go-test-failfast t)
  :bind
  ("s-o t" . go-test-current-test)
  ("s-o f" . go-test-current-file))

(use-package eglot
  :after yasnippet
  :init
  (add-hook 'project-find-functions #'project-find-go-module)
  (setq eglot-workspace-configuration
        '((:gopls .
                  ((staticcheck . t)
                   (gofumpt . t)
                   (usePlaceholders . t)
                   (analyses .
                              ((nilness . t)
                               (shadow . t)
                               (unusedparams . t)
                               (unusedwrite . t)
                               (useany . t)
                               (unusedvariable . t)))))))
  :bind
  (("s-o r" . eglot-rename)
   ("s-o h" . eldoc)
   ("s-o i" . eglot-code-action-organize-imports)
   ("s-o a" . eglot-code-actions)
   ("s-o ." . eglot-find-implementation)
   ("s-o e" . consult-flymake))

  :config
  (setenv "GOFLAGS" "-mod=vendor")
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-12"))

  :hook
  (
   (go-mode . yas-minor-mode)
   (go-mode . eglot-ensure)
   (go-mode . eglot-format-buffer-on-save)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (yaml-mode . eglot-ensure)
   (terraform-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (python-mode . eglot-ensure)))

(use-package consult-eglot
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-global-modes
        '(not org-mode not sh-mode not eshell-mode not latex-mode not gud-mode))
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
         ("C-C d n" . org-roam-dailies-goto-next-note))

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

;; rust
(use-package toml-mode
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :defer t
  :hook (rust-mode . cargo-minor-mode))

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
