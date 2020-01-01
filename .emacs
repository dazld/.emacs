

;;; Code:
;; (require 'req-package)


(menu-bar-mode -1)
(prefer-coding-system 'utf-8)
(setq require-final-newline t)
(setq use-file-dialog nil)

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(require 'req-package)

;(req-package use-package-el-get ;; prepare el-get support for use-package (optional)
;  :force t ;; load package immediately, no dependency resolution
;  :config)
;  ;(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
;  ;(el-get 'sync)
;  ;(use-package-el-get-setup))

(setq history-delete-duplicates t)
(setq history-length            100) ; default is 30.
(setq indent-tabs-mode nil)

(when (= emacs-major-version 26)
  (setq x-wait-for-event-timeout nil))

(setq line-move-visual                 nil
      report-emacs-bug-no-explanations t
      comint-prompt-read-only          t
      uniquify-buffer-name-style       nil
      register-preview-delay           nil
      inhibit-startup-message          t
      message-log-max                  1000
      kill-ring-max                    80
      mark-ring-max                    60
      global-mark-ring-max             200)


(use-package flycheck
  :config
  (progn
    (global-flycheck-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)


(use-package helm-mode
  :init
  (add-hook 'helm-mode-hook
            (lambda ()
              (setq completion-styles
                    (cond ((assq 'helm-flex completion-styles-alist)
                           '(helm-flex))
                          ((assq 'flex completion-styles-alist)
                           '(flex))))))
  :config
  (helm-mode 1))


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

(global-set-key (kbd "C-x f") #'find-file-in-repository)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-d") #'helm-browse-project)
(global-set-key (kbd "<M-up>") #'er/expand-region)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'clojure-mode-hook #'cider-mode)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("28caf31770f88ffaac6363acfda5627019cac57ea252ceb2d41d98df6d87e240" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "8dc7f4a05c53572d03f161d82158728618fb306636ddeec4cce204578432a06d" "2eb1f5551310e99101f0f9426485ab73aa5386054da877aacd15d438382bb72e" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "947190b4f17f78c39b0ab1ea95b1e6097cc9202d55c73a702395fc817f899393" default)))
 '(package-selected-packages
   (quote
    (flycheck-clojure helm-css-scss scss-mode flycheck-inline js2-mode markdown-mode el-get req-package irony flycheck ample-theme calmer-forest-theme clj-refactor lispy use-package naquadah-theme find-file-in-repository helm-flx expand-region paredit css-eldoc sass-mode helm gruvbox-theme cider parinfer company clojure-mode dracula-theme)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#282828"))))

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


;;; .emacs ends here
