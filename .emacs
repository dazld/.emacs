

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
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package adjust-parens)

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
	helm-move-to-line-cycle-in-source     t
        qhelm-completion-in-region-fuzzy-match t
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
(use-package helm-css-scss)
(use-package helm-flx)
(use-package helm-company
  :ensure t)
(use-package lispy
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package multiple-cursors
  :ensure t)
(use-package paredit
  :bind (("ESC <left>" . 'paredit-forward-slurp-sexp)
	 ("ESC <right>" . 'paredit-forward-barf-sexp)))

(use-package parinfer
  :ensure t
  :bind
  (("ESC ." . parinfer-toggle-mode))
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

(use-package smartparens
  :ensure t)

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

(use-package sass-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package monokai-theme
  :ensure t)






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
   '("d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "7ea491e912d419e6d4be9a339876293fff5c8d13f6e84e9f75388063b5f794d6" "96c56bd2aab87fd92f2795df76c3582d762a88da5c0e54d30c71562b7bf9c605" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#70480f")
     ("TODO" . "#721045")
     ("NEXT" . "#5317ac")
     ("THEM" . "#8f0075")
     ("PROG" . "#00538b")
     ("OKAY" . "#30517f")
     ("DONT" . "#315b00")
     ("FAIL" . "#a60000")
     ("BUG" . "#a60000")
     ("DONE" . "#005e00")
     ("NOTE" . "#863927")
     ("KLUDGE" . "#813e00")
     ("HACK" . "#813e00")
     ("TEMP" . "#5f0000")
     ("FIXME" . "#a0132f")
     ("XXX+" . "#972500")
     ("REVIEW" . "#005a5f")
     ("DEPRECATED" . "#201f55")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-header)
 '(package-selected-packages
   '(modus-operandi-theme modus-vivendi-theme rust-mode flycheck monokai-theme smartparens paredit multiple-cursors cider editorconfig use-package adjust-parens helm-ag scss-mode lispy irony helm-flx helm-css-scss parinfer markdown-mode find-file-in-repository expand-region el-get css-eldoc company))
 '(vc-annotate-background-mode nil)
 '(xterm-color-names
   ["#000000" "#a60000" "#005e00" "#813e00" "#0030a6" "#721045" "#00538b" "#f0f0f0"])
 '(xterm-color-names-bright
   ["#505050" "#972500" "#315b00" "#70480f" "#223fbf" "#8f0075" "#30517f" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
