

;;; Code:

;; no thx
(menu-bar-mode -1)

;; nom nom
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'exec-path "/usr/local/bin")

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(line-number-mode 1)

;; Unicode
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)

(setq-default backup-directory-alist `(("." . "~/.saves"))
              history-length 100
              history-delete-duplicates t
              delete-old-versions t
              backup-by-copying t
              kept-new-versions 6
              kept-old-versions 2
              version-control t)

(setq-default line-move-visual                 nil
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


(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "M-{") #'paredit-forward-slurp-sexp)
(global-set-key (kbd "M-}") #'paredit-forward-barf-sexp)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package cider
  :bind ("C-c C-o" . 'cider-repl-clear-buffer))

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'parinfer-rust-mode)
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package company
  :init
  (setq company-minimum-prefix-length 2
        company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode))

;; (use-package adjust-parens)

(use-package expand-region
  :bind (("<C-up>" . 'er/expand-region)
         ("<C-down>" . 'er/contract-region)))

(use-package rust-mode)

(use-package find-file-in-repository
  :bind ("C-x f" . 'find-file-in-repository))

(use-package helm
  :bind (("M-x" . 'helm-M-x)
         ("C-x b" . 'helm-buffers-list)
         ("C-s" . 'helm-occur)
         ("M-y" . 'helm-show-kill-ring)
         ("C-x C-d" . 'helm-browse-project))
  :init
  (setq helm-mode-fuzzy-match                 t
        helm-grep-ag-command                  "rg --color=always --smart-case --no-heading --line-number %s %s %s"
        helm-move-to-line-cycle-in-source     t
        helm-completion-in-region-fuzzy-match t
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

(use-package helm-css-scss)
(use-package helm-flx)
(use-package helm-company)
(use-package lispy)
(use-package markdown-mode)
(use-package multiple-cursors)

(use-package paredit
  :bind (("ESC <left>" . 'paredit-forward-slurp-sexp)
         ("ESC <right>" . 'paredit-forward-barf-sexp)))

(use-package parinfer-rust-mode
  :hook (clojure-mode emacs-lisp-mode)
  :init
  (setq parinfer-rust-mode-auto-download t))

(use-package smartparens)

(use-package lsp-mode
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

(use-package sass-mode)
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package flycheck
  :init (global-flycheck-mode))
(use-package monokai-theme)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(monokai))
 '(custom-safe-themes
   '("d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" default))
 '(package-selected-packages
   '(use-package smartparens sass-mode rust-mode parinfer-rust-mode paredit monokai-theme modus-vivendi-theme modus-operandi-theme lsp-ui lispy helm-flx helm-css-scss helm-company flycheck find-file-in-repository expand-region el-get editorconfig css-eldoc cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
