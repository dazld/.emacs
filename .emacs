

;;; Code:

;; no thx
(menu-bar-mode -1)

;; nom nom
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(line-number-mode 1)

;; Unicode
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)

(setq history-delete-duplicates t)
(setq history-length            100) ; default is 30.

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq line-move-visual                 nil
      indent-tabs-mode                 nil
      use-package-always-ensure        t
      use-file-dialog                  nil
      require-final-newline            t
      uniquify-buffer-name-style       nil
      register-preview-delay           nil
      inhibit-startup-message          t
      message-log-max                  1000
      kill-ring-max                    80
      mark-ring-max                    60
      global-mark-ring-max             200)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


(global-set-key (kbd "C-M-s") #'helm-projectile-grep)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-d") #'helm-browse-project)
(global-set-key (kbd "M-{") #'paredit-forward-slurp-sexp)
(global-set-key (kbd "M-}") #'paredit-forward-barf-sexp)


(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package cider
  :bind ("C-c C-o" . 'cider-repl-clear-buffer))

(use-package clojure-mode
  :config
  (defun my-clojure-mode-hook ()
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package adjust-parens)

(use-package expand-region
  :bind (("<M-up>" . 'er/expand-region)
	 ("<M-down>" . 'er/contract-region)))
(use-package find-file-in-repository
  :bind ("C-x f" . 'find-file-in-repository))
(use-package helm
  :bind (("M-x" . 'helm-M-x)
	 ("C-M-s" . 'helm-projectile-ag)
	 ("C-x C-d" . 'helm-browse-project))
  :init
  (setq helm-mode-fuzzy-match                 t
        helm-completion-in-region-fuzzy-match t
        helm-grep-ag-command                  "rg --color=always --smart-case --no-heading --line-number %s %s %s"
        helm-M-x-fuzzy-match                  t
        helm-bookmark-show-location           t
        helm-buffers-fuzzy-matching           t
        helm-file-cache-fuzzy-match           t
        helm-imenu-fuzzy-match                t
        helm-locate-fuzzy-match               t
        helm-quick-update                     t
        helm-recentf-fuzzy-match              t
        helm-semantic-fuzzy-match             t)
  (add-hook 'helm-mode-hook
	    (lambda ()
	      (setq completion-styles
		    (cond ((assq 'helm-flex completion-styles-alist)
			   '(helm-flex))
			  ((assq 'flex completion-styles-alist)
			   '(flex))))))
  :config
  (helm-mode 1))
(use-package helm-ag)
(use-package helm-css-scss)
(use-package helm-flx)
(use-package helm-company)
(use-package irony)
(use-package lispy)
(use-package markdown-mode)
(use-package multiple-cursors)
(use-package paredit)
(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))
(use-package scss-mode)
(use-package smartparens)

(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-enable-indentation nil))

'(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))





;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes '(monokai))
 '(custom-safe-themes
   '("8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(package-selected-packages
   '(flycheck monokai-theme smartparens paredit multiple-cursors cider editorconfig use-package adjust-parens helm-ag scss-mode lispy irony helm-flx helm-css-scss parinfer markdown-mode find-file-in-repository expand-region el-get css-eldoc company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
