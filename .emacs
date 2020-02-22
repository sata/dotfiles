(package-initialize)

(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'package+)
  (package-install 'package+))

(package-manifest 'ag
                  'expand-region
                  'magit
                  ;; 'melpa
                  ;; 'flycheck-golang
		  'cyberpunk-theme
		  'flycheck
                  'paredit
                  'ruby-mode 
		  'ssh
                  'nyan-mode
                  'python-mode
                  'highlight-symbol
                  'markdown-mode
                  'alchemist
                  'elixir-mode
                  'projectile
                  'cc-mode
                  'go-projectile
                  'company
                  'go-mode
                  'company-go
                  'cl-lib
		  'package+
                  )

(require 'cl-lib)

(setq paths-to-load '("~/go/src/github.com/dougm/goflymake"))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".emacs" ".org" ".md"))
(ido-mode 1)

(cl-map nil #'(lambda (e) (add-to-list 'load-path e)) paths-to-load)

(require 'package+)
(require 'nyan-mode)
(require 'highlight-symbol)
(require 'alchemist)
(global-company-mode)
(require 'elixir-mode)

(require 'projectile)
(require 'cc-mode)
(require 'go-flycheck)
(require 'go-projectile)
(require 'company)
(require 'go-mode)
(require 'company-go)
(require 'uniquify)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-global-mode)
(nyan-mode)
(nyan-start-animation)

(defun better-defaults ()
  (progn
    (tool-bar-mode -1) (scroll-bar-mode -1)
    (setq-default indent-tabs-mode nil)
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

(setq indent-tabs-mode nil)

(setq c-basic-offset 2)

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)


;; Python Hook 4 space tab
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

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

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(company go-projectile company-go go-eldoc flycheck-golangci-lint flymake flymake-google-cpplint flymake-go go-autocomplete auto-complete exec-path-from-shell go-mode fireplace edts dockerfile-mode yaml-mode json-mode projectile alchemist plantuml-mode web-mode elixir-mode floobits ##)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;; (global-set-key (kbd "<f3>") 'erlang-man-function)

;; elixir
(add-to-list 'auto-mode-alist '("\\.eex$" . web-mode))
(add-to-list 'elixir-mode-hook 'alchemist-mode 'company)

;; plantuml
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(setq plantuml-output-type `"ascii")

(add-hook 'after-init-hook #'global-flycheck-mode)

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

(defun my-go-mode-hook ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -race -v && go vet"))

  (local-set-key (kbd "M-.") 'godef-jump)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook (lambda ()
                          (company-mode)
                          (set (make-local-variable 'company-backends) '(company-go))))

(projectile-global-mode 1)
(go-projectile-tools-add-path)
;; (ac-config-default)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ag alchemist cc-mode cl-lib company company-go cyberpunk-theme elixir-mode expand-region flycheck go-mode go-projectile highlight-symbol magit markdown-mode nyan-mode package+ paredit projectile python-mode ruby-mode ssh))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
