;;; post-init.el --- [core init file] -*- no-byte-compile: t; lexical-binding: t; -*-

(setq show-trailing-whitespace t)
(setq tramp-default-method "ssh")
(setq display-line-numbers-type 'relative)
;; (setq default-frame-alist '((font . "FiraCode Nerd Font Mono 13")))
;; (setq default-frame-alist '((font . "JetBrainsMono Nerd Font Mono 13")))
(setq default-frame-alist '((font . "Maple Mono NF 13")))
(setq backup-directory-alist
      `(("." . ,(expand-file-name ".backups/" user-emacs-directory))))
(setq auto-save-list-file-prefix (expand-file-name ".auto-saves/" user-emacs-directory))
(setq auto-save-file-name-transforms `(("\\`.*\\'" ,(expand-file-name ".auto-saves/" user-emacs-directory))))
(global-display-line-numbers-mode)
(global-auto-revert-mode t)
(auto-save-visited-mode)

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

(global-unset-key (kbd "M-'"))

;; INFO: packages
(use-package multiple-cursors
  :after boon
  :ensure t
  :bind (:map boon-command-map
              (("C->" . 'mc/mark-next-like-this)
               ("C-<" . 'mc/mark-previous-like-this)
               ("C-S-c C-S-c" . 'mc/edit-lines)
               ("C-c C-<" . 'mc/mark-all-like-this))))

(use-package which-key :ensure t :config (which-key-mode))

(use-package simpleclip
  :after boon
  :ensure t
  :commands (simpleclip-paste simpleclip-cut simpleclip-copy)
  :bind ("C-v" . simpleclip-paste)
  :config (simpleclip-mode 1))

(use-package boon
  :ensure t
  :config
  (require 'boon-colemak-hnei)
  (boon-mode)

  ;; https://github.com/meow-edit/meow/discussions/168#discussioncomment-2974816
  (setq boon-two-char-escape-sequence "kt")
  (setq boon-two-char-escape-delay 0.2)

  (defun boon--two-char-exit-insert-state (s)
    (when boon-insert-state
      (let ((modified (buffer-modified-p)))
        (insert (elt s 0))
        (let* ((second-char (elt s 1))
               (event
                (if defining-kbd-macro
                    (read-event nil nil)
                  (read-event nil nil boon-two-char-escape-delay))))
          (when event
            (if (and (characterp event) (= event second-char))
                (progn
                  (backward-delete-char 1)
                  (set-buffer-modified-p modified)
                  (boon-set-command-state))
              (push event unread-command-events)))))))

  (defun boon-two-char-exit-insert-state ()
    (interactive)
    (boon--two-char-exit-insert-state boon-two-char-escape-sequence))

  (define-key boon-insert-map (substring boon-two-char-escape-sequence 0 1)
              #'boon-two-char-exit-insert-state))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package ag :ensure t)

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("{{" "}}" "{{--" "--}}" "{|" "|}" "[|" "|]" "//" "///" "/*" "/**"
                                       "++" "+++" ".?" ".." "..." "..<" "<!--" "<<-" "<-" "<#--" "<>" "<:"
                                       "<:<" ">:>" "<=>" "<->" "<|||" "<||" "<|" "<|>" "||>" "|>" "-|" "->>"
                                       "-->" "->" ">=" "<=" "<<=" "<==" "!=" "!!" "!==" "=!=" "=>" "==" "=:="
                                       ":=:" ":=" ":>" ":<" "::" ";;" ";;;" ":?" ":?>" "::=" "||-" "||=" "|-"
                                       "|=" "||" "--" "---" "<--" "??" "???" "?:" "?." "&&" "__" "=/=" "<-<"
                                       "<=<" "<==>" "==>" "=>>" ">=>" ">>=" ">>-" "-<" "-<<" "<-|" "<=|" "|=>"
                                       ">-" "<~" "~~" "<~>" "<~~" "-~" "~~>" "~>" "~-" "~@" "<+>" "<+" "+>"
                                       "<*>" "<*" "*>" "</>" "</" "/>" "<<" "<<<" ">>" ">>>" "#{" "#[" "#("
                                       "#?" "#_" "#__" "#:" "#=" "#_(" "]#" "0x12" "[TRACE]" "[DEBUG]" "[INFO]"
                                       "[WARN]" "[ERROR]" "[FATAL]" "[TODO]" "todo))" "[FIXME]" "fixme))"
                                       "########" "<!---->" "\\\\"))
  (global-ligature-mode t))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name ".undo-tree/" user-emacs-directory))))
  (undo-tree-visualizer-timestamps t))

;; INFO: language-specific
(load-file (expand-file-name "lisp/languages.el" user-emacs-directory))

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
  :bind ("M-' T" . titlecase-region)
  :ensure t)

(use-package prescient
  :custom
  (setq prescient-history-length 1000)
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :ensure t
  :after ivy
  :config (ivy-prescient-mode))

(use-package company-prescient
  :ensure t
  :after company
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
  :bind (("M-' l l" . eglot)
         ("M-' l s" . eglot-shutdown)
         ("M-' l d" . eldoc)
         ("M-' l r" . eglot-rename)
         ("M-' l R" . eglot-reconnect)
         ("M-' l f" . eglot-format-buffer)
         ("M-' l i" . eglot-code-action-organize-imports))
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

(use-package hydra :ensure t)

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-require-punctuation t)
  (setq hl-todo-highlight-punctuation ":")
  (setq hl-todo-keyword-faces
        '(("TODO"  . "#fb4934")
          ("FIXME" . "#fb4934")
          ("DEBUG" . "#d3869b")
          ("WARN"  . "#d3869b")
          ("INFO"  . "#83a598")))
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

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package avy
  :ensure t
  :bind (:map boon-command-map
              ("o" . my/avy-goto-char-2-end)
              ("O" . avy-goto-char))
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  (avy-style 'de-bruijn)
  (avy-keys (string-to-list "arstneiofu"))
  :config
  (defun my/avy-goto-char-2-end ()
    "Move to the end of the match for avy-goto-char-2."
    (interactive)
    (let ((char1 (read-char "First char: "))
          (char2 (read-char "Second char: ")))
      (avy-goto-char-2 char1 char2)
      (forward-char 2))))

(use-package shift-number
  :ensure t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :demand t
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("M-' p" . projectile-command-map)))

;; (use-package zoom
;;   :ensure t
;;   :config (zoom-mode))

(use-package flx
  :ensure t
  :config)

(use-package swiper
  :bind (:map boon-command-map ("p" . swiper)))

(use-package counsel
  :ensure t
  :after flx
  :bind (:map ivy-mode-map ("<escape>" . 'minibuffer-keyboard-quit))
  :bind (("M-' n a" . counsel-grep-or-swiper)
         ("M-' n i" . counsel-file-jump)
         ("M-' n n" . counsel-bookmark)
         ("M-' n f" . counsel-buffer-or-recentf))
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

(defun my-on-boon-command-state-change (symbol new-value operation where)
  "Run something whenever `boon-command-state` is set to t."
  (when (and (eq symbol 'boon-command-state)
             (eq new-value t))
    (save-some-buffers t)))
(add-variable-watcher 'boon-command-state #'my-on-boon-command-state-change)

(add-hook 'find-file-hook (lambda ()
                            (undo-tree-load-history nil t)))
;; save history on exit
(add-hook 'kill-emacs-query-functions (lambda ()
                                        (undo-tree-save-history nil 'overwrite)))

(add-hook 'server-kill-emacs-query-function (lambda ()
                                              (undo-tree-save-history nil 'overwrite)))

;; save history when focus out, save the buffer as well
(add-hook 'focus-out-hook
          (lambda ()
            (save-some-buffers t)
            (when (and (buffer-file-name) undo-tree-mode)
              (undo-tree-save-history nil 'overwrite))))


;; INFO: additional code
(require 'funcs)
(require 'hydras)

(add-to-list 'pulsar-pulse-functions #'hydra-paging/scroll-up-command)
(add-to-list 'pulsar-pulse-functions #'hydra-paging/scroll-down-command)
(add-to-list 'pulsar-pulse-functions #'avy-goto-char-timer)
(add-to-list 'pulsar-pulse-functions #'avy-goto-char-2)

(defun my/backward-delete-word ()
  "Delete the previous word without saving to the kill ring."
  (interactive)
  (let ((start (point))
        (end (progn (backward-word) (point))))
    (delete-region start end)))

;; INFO: keymaps
(define-key boon-x-map (kbd "x") 'counsel-M-x)

(define-key global-map (kbd "C-<backspace>") 'my/backward-delete-word)
(define-key global-map (kbd "C-,") 'my-duplicate-line)

(define-key global-map (kbd "M-' q") #'quit-window)
(define-key global-map (kbd "M-' M-f") #'delete-indentation)
(define-key global-map (kbd "M-' s") #'boon-enclose)
(define-key global-map (kbd "M-' TAB") #'mode-line-other-buffer)
(define-key global-map (kbd "M-' f") #'dired-jump)
(define-key global-map (kbd "M-' Q") #'kill-emacs)
(define-key global-map (kbd "M-' y") (lambda () (interactive)
                                       (find-file (expand-file-name "post-init.el" user-emacs-directory))))
(define-key global-map (kbd "M-' Y") (lambda () (interactive) (load-file user-init-file)))
(define-key global-map (kbd "M-' u s") #'query-replace)
(define-key global-map (kbd "M-' u S") #'query-replace-regexp)

(define-key global-map (kbd "M-' w n") #'windmove-down)
(define-key global-map (kbd "M-' w e") #'windmove-up)
(define-key global-map (kbd "M-' w h") #'windmove-right)
(define-key global-map (kbd "M-' w i") #'windmove-left)
(define-key global-map (kbd "M-' w y") #'delete-window)

(define-key global-map (kbd "M-' c y") #'spacemacs/show-hide-compilation-window)
(define-key global-map (kbd "M-' c s") #'spacemacs/switch-to-compilation-buffer)
(define-key global-map (kbd "M-' c c") #'compile)
(define-key global-map (kbd "M-' c r") #'recompile)
(define-key global-map (kbd "M-' c n") #'next-error)
(define-key global-map (kbd "M-' c e") #'previous-error)
(define-key global-map (kbd "M-' c k") #'kill-compilation)

(define-key global-map (kbd "M-' u u") #'delete-trailing-whitespace)
(define-key global-map (kbd "M-' u a") #'align-regexp)
(define-key global-map (kbd "M-' u h") 'boon-unhighlight)

(define-key global-map (kbd "M-' k p") #'kill-this-buffer)
(define-key global-map (kbd "M-' k P") #'kill-buffer)
(define-key global-map (kbd "M-' k T") #'bookmark-delete)
(define-key global-map (kbd "M-' k m") #'spacemacs/switch-to-messages-buffer)
(define-key global-map (kbd "M-' k s") #'spacemacs/switch-to-scratch-buffer)

(define-key global-map (kbd "M-' h t") #'hydra-paging/body)
(define-key global-map (kbd "M-' h n") #'hydra-numbers/body)
(define-key global-map (kbd "M-' h T") #'hydra-todo/body)
(define-key global-map (kbd "M-' h z") #'hydra-zoom/body)
