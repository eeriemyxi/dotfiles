;;; post-init.el --- [core init file] -*- no-byte-compile: t; lexical-binding: t; -*-

(setq show-trailing-whitespace t)
(setq display-line-numbers-type 'relative)
(setq default-frame-alist '((font . "JetBrainsMono Nerd Font Mono 13")))
(setq backup-directory-alist
      `(("." . ,(expand-file-name ".backups/" user-emacs-directory))))
(setq auto-save-list-file-prefix (expand-file-name ".auto-saves/" user-emacs-directory))
(setq auto-save-file-name-transforms `(("\\`.*\\'" ,(expand-file-name ".auto-saves/" user-emacs-directory))))
(global-display-line-numbers-mode)
(global-auto-revert-mode t)

;; INFO: theme
(use-package gruvbox-theme :ensure t :config (load-theme 'gruvbox-dark-medium t))

;; INFO: custom files
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; INFO: macros
(require 'my-macros)

;; INFO: settings
(defun apply-sane-indent () 
  (require 'sane-indent)
  ;; indent level 4 is hard-coded
  (define-key global-map (kbd "RET") 'ey/sane-newline-and-indent))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default word-wrap t)
(setq-default truncate-lines nil)
(setq indent-line-function 'insert-tab)
(setq c-default-style "linux") 
(setq c-basic-offset 4) 
(setq recentf-max-saved-items 200)
(c-set-offset 'comment-intro 0)
(add-hook 'python-ts-mode-hook 'apply-sane-indent)
(add-hook 'lua-mode-hook 'apply-sane-indent)
(add-hook 'fish-mode-hook 'apply-sane-indent)

(add-hook 'prog-mode-hook
          (lambda () 
            (whitespace-newline-mode t)))

;; INFO: packages
(require 'my-meow)

(use-package meow-tree-sitter
  :ensure t
  :config
  (unless (featurep 'meow-tree-sitter-configured)
    (meow-tree-sitter-register-defaults)
    (provide 'meow-tree-sitter-configured)))

(require 'emacs-surround)
(add-to-list 'emacs-surround-alist '("*" . ("**" . "**")))
(add-to-list 'emacs-surround-alist '("8" . ("*" . "**")))
(global-set-key (kbd "C-c C-SPC C-s") 'emacs-surround)

(use-package goto-chg :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-;" . 'er/expand-region))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name ".undo-tree/" user-emacs-directory))))
  (undo-tree-visualizer-timestamps t))

;; INFO: language-specific
(require 'languages)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package magit :ensure t)

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode t))

(use-package titlecase
  :bind ("C-c C-SPC T" . titlecase-region)
  :ensure t)

(use-package company-prescient
  :ensure t
  :config (company-prescient-mode))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook
           'prog-mode-hook #'rainbow-delimiters-mode))

(use-package dirvish
  :ensure t
  :config
  (define-key dired-mode-map (kbd "-") 'dired-up-directory)
  (dirvish-override-dired-mode))

(use-package eglot
  :ensure t
  :bind (("C-c C-SPC C-l l" . eglot)
         ("C-c C-SPC C-l s" . eglot-shutdown)
         ("C-c C-SPC C-l d" . eldoc)
         ("C-c C-SPC C-l r" . eglot-rename)
         ("C-c C-SPC C-l R" . eglot-reconnect)
         ("C-c C-SPC C-l f" . eglot-format-buffer)
         ("C-c C-SPC C-l i" . eglot-code-action-organize-imports))
  :commands (eglot
             eglot-rename
             eglot-format-buffer
             eglot-shutdown
             eglot-reconnect
             eglot-code-action-organize-imports)
  :custom
  (eglot-report-progress nil)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  (add-to-list 'eglot-server-programs
               ;; pip install python-lsp-server[all]
               `(python-ts-mode . ,(eglot-alternatives
                                    '(("pylsp")))))
  (setq-default eglot-workspace-configuration
                `(:pylsp (:plugins
                          (
                           ;; pip install python-lsp-{isort,black}
                           :isort (:enabled t)
                           :black (:enabled t))))))
(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

(use-package centaur-tabs
  :ensure t
  :bind ("C-c C-SPC C-k t" . centaur-tabs-ace-jump)
  :demand
  :custom
  (setq-default centaur-tabs-ace-jump-keys
                '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?c ?k ?m ?w ?y))
  (setq-default centaur-tabs-set-bar 'right)
  (setq-default centaur-tabs-style "slant")
  :config
  (centaur-tabs-mode t)
  (custom-set-faces
   '(centaur-tabs-default
     ((t (:background "#282828" :foreground "#928374"))))
   '(centaur-tabs-unselected
     ((t (:background "#282828" :foreground "#a89984"))))
   '(centaur-tabs-selected
     ((t (:background "#504945" :foreground "#ebdbb2"))))
   '(centaur-tabs-unselected-modified
     ((t (:background "#3c3836" :foreground "#fabd2f"))))
   '(centaur-tabs-selected-modified
     ((t (:background "#504945" :foreground "#fabd2f")))))
  (centaur-tabs-headline-match))

(use-package hydra :ensure t)

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#fb4934")
          ("FIXME"  . "#fb4934")
          ("DEBUG"  . "#d3869b")
          ("GOTCHA" . "#fe8019")
          ("INFO"   . "#83a598")
          ("STUB"   . "#83a598")))
  (global-hl-todo-mode 1))

(use-package snap-indent
  :ensure t
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save t)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-dabbrev-minimum-length 1)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(delete-consecutive-dups
                               company-sort-by-occurrence))
  (setq company-backends '((company-capf
                            company-dabbrev-code
                            company-files
                            company-tempo
                            company-keywords)))
  (global-company-mode))
;; (company-tng-configure-default))

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package avy
  :ensure t
  :bind (("C-c C-SPC C-n t" . avy-goto-char-2)
         ("C-c C-SPC C-n T" . avy-goto-char-timer))
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  (avy-style 'de-bruijn)
  (avy-keys (string-to-list "arstneiofu")))

(use-package conf-mode
  :bind (:map conf-space-mode-map ("C-c SPC" . nil)))

(use-package shift-number
  :ensure t)

(use-package flx
  :ensure t
  :config)

(use-package swiper
  :bind (("C-c C-SPC C-n I" . swiper-isearch)))

(use-package counsel
  :ensure t
  :after flx
  :bind (("C-c C-SPC C-n a" . counsel-grep-or-swiper)
         ("C-c C-SPC C-n i" . counsel-file-jump)
         ("C-c C-SPC C-n n" . counsel-bookmark)
         ("C-c C-SPC C-n f" . counsel-buffer-or-recentf))
  :bind (("C-x C-f" . counsel-find-file)
         ("M-y"     . counsel-yank-pop)
         ("M-x"     . counsel-M-x)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("<f2> j"  . counsel-set-variable))
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode 1))

;; INFO: startup stuff
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

(add-hook 'focus-out-hook (lambda ()
                            (save-some-buffers t)))
(add-hook 'meow-insert-exit-hook (lambda ()
                                   (save-some-buffers t)))

;; INFO: additional code
(require 'funcs)
(require 'hydras)

(add-to-list 'pulsar-pulse-functions #'hydra-paging/scroll-up-command)
(add-to-list 'pulsar-pulse-functions #'hydra-paging/scroll-down-command)
(add-to-list 'pulsar-pulse-functions #'avy-goto-char-timer)
(add-to-list 'pulsar-pulse-functions #'avy-goto-char-2)

;; INFO: keymaps
(define-key global-map (kbd "C-,") 'my-duplicate-line)

(meow-normal-define-key
 '("C-<left>"  . centaur-tabs-backward)
 '("C-<right>" . centaur-tabs-forward)
 '("C-i"       . goto-last-change)
 '("C-S-i"     . goto-last-change-reverse)
 '("V"         . call-last-kbd-macro))

(meow-leader-define-key
 '("C-SPC y"     . (lambda () (interactive)
                     (find-file (expand-file-name "post-init.el" user-emacs-directory))))
 '("C-SPC Y"     . (lambda () (interactive) (load-file user-init-file)))
 '("SPC"         . "C-c C-SPC")
 '("C-SPC SPC"   . "H-SPC")
 '("C-SPC f"     . dired-jump)
 '("C-SPC q"     . quit-window)
 '("C-SPC Q"     . kill-emacs)
 '("C-SPC t"     . hydra-paging/body)
 '("C-SPC TAB"   . mode-line-other-buffer)
 '("C-SPC C-w n" . windmove-down)
 '("C-SPC C-w e" . windmove-up)
 '("C-SPC C-w h" . windmove-right)
 '("C-SPC C-w i" . windmove-left)
 '("C-SPC C-w y" . delete-window)
 '("C-SPC C-c y" . spacemacs/show-hide-compilation-window)
 '("C-SPC C-c s" . spacemacs/switch-to-compilation-buffer)
 '("C-SPC C-c c" . compile)
 '("C-SPC C-c r" . recompile)
 '("C-SPC C-c n" . next-error)
 '("C-SPC C-c e" . previous-error)
 '("C-SPC C-c k" . kill-compilation)
 '("C-SPC C-u u" . delete-trailing-whitespace)
 '("C-SPC C-u a" . align-regexp)
 '("C-SPC C-u n" . hydra-numbers/body)
 '("C-SPC C-u n" . hydra-numbers/body)
 '("C-SPC C-u t" . hydra-todo/body)
 '("C-SPC C-u h" . evil-ex-nohighlight)
 '("C-SPC C-u s" . query-replace)
 '("C-SPC C-u S" . query-replace-regexp)
 '("C-SPC C-u z" . hydra-zoom/body)
 '("C-SPC C-k p" . kill-this-buffer)
 '("C-SPC C-k P" . kill-buffer)
 '("C-SPC C-k T" . bookmark-delete)
 '("C-SPC C-k m" . spacemacs/switch-to-messages-buffer)
 '("C-SPC C-k s" . spacemacs/switch-to-scratch-buffer)
 )

