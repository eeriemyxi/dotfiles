;;; post-init.el --- [core init file] -*- no-byte-compile: t; lexical-binding: t; -*-

(setq show-trailing-whitespace t)
(setq confirm-kill-processes nil)
(setq tramp-default-method "ssh")
(setq default-frame-alist '((font . "Maple Mono NF 9")))
(setq backup-directory-alist
      `(("." . ,(expand-file-name ".backups/" user-emacs-directory))))
(setq auto-save-list-file-prefix (expand-file-name ".auto-saves/" user-emacs-directory))
(setq auto-save-file-name-transforms `(("\\`.*\\'" ,(expand-file-name ".auto-saves/" user-emacs-directory))))
(global-auto-revert-mode t)
(auto-save-visited-mode)
(column-number-mode 1)
(menu-bar-mode -1)
(electric-pair-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-width 1)
(setq ispell-program-name "aspell")

(setq select-enable-clipboard t)

;; INFO: theme
(use-package gruvbox-theme :ensure t :config (load-theme 'gruvbox-dark-medium t))
(set-face-attribute 'font-lock-keyword-face nil
                    :weight 'bold)

;; INFO: custom files
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; INFO: macros
(require 'my-macros)

;; INFO: settings
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(defun apply-sane-indent ()
  (require 'sane-indent)
  ;; indent level 4 is hard-coded
  (define-key global-map (kbd "RET") 'ey/sane-newline-and-indent))

;; (setq-default word-wrap t)
;; (setq-default truncate-lines nil)
(setq recentf-max-saved-items 5000)
(setq recentf-auto-cleanup 'never)

(add-hook 'python-ts-mode-hook 'apply-sane-indent)
(add-hook 'python-mode-hook 'apply-sane-indent)
(add-hook 'lua-mode-hook 'apply-sane-indent)
(add-hook 'fish-mode-hook 'apply-sane-indent)
(add-hook 'oil-mode-hook #'turn-on-boon-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'compilation-mode-hook
          (lambda ()
            (local-set-key (kbd "q") 'quit-window)))

(defun my/disable-electric-pair-in-replace ()
  "Disable `electric-pair-mode' in query-replace by checking the prompt text."
  (when (string-match-p "^Query replace" (minibuffer-prompt))
    (electric-pair-local-mode -1)))

(add-hook 'minibuffer-setup-hook #'my/disable-electric-pair-in-replace)

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

(use-package boon
  :ensure t
  :config
  ;; (require 'boon-colemak-hnei)
  (require 'boon-qwerty-hjkl)
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

(use-package xterm-color
  :ensure t
  :config
  (setq compilation-environment '("TERM=xterm-256color"))

  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package ag :ensure t)

(use-package consult
  :ensure t
  :bind (("M-' n n" . counsel-switch-buffer))
  :after ivy)

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures '(prog-mode markdown-mode) '("{{" "}}" "{{--" "--}}" "{|" "|}" "[|" "|]" "//" "///" "/*" "/**"
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

(use-package ultra-scroll
  :init
  (setq pixel-scroll-precision-interpolate-page t
        pixel-scroll-precision-interpolate-mice t
        scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

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

(setq treesit-language-source-alist
      '(
        ;; Pin TypeScript/TSX to stable 0.20.3
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
        
        ;; Pin Rust to stable 0.20.4
        (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.20.4" "src")
        
        ;; Pin Python to the Emacs 30 sweet-spot
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.2" "src")
        ))

(use-package treesit-auto
  :custom
  :disabled
  (treesit-auto-install 'prompt)
  :config
  ;; INFO: https://www.reddit.com/r/emacs/comments/17gtxmr/indentation_in_yamltsmode/
  ;; (delete 'yaml treesit-auto-langs)
  ;; INFO: Doesn't highlight syntax for whatever reason (Wednesday 02 April 2025 06:18:55 PM IST)
  ;; (delete 'lua treesit-auto-langs)
  ;; (delete 'rust treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package magit
  :ensure t
  :bind (:map magit-mode-map
              ;; Rebind movement keys to hnei-friendly alternatives
              ("n" . magit-section-forward)       ; Move to next section (was n)
              ("e" . magit-section-backward)      ; Move to previous section (was p)
              ("i" . magit-section-up)            ; Move up a section (was u)
              ("h" . magit-section-toggle)        ; Toggle section visibility (was TAB)
              ;; Retain important commands with intuitive bindings
              ("j" . magit-status)                ; Open magit status
              ("k" . magit-dispatch)              ; Open magit dispatch menu
              ("l" . magit-log-all)               ; Open log view
              ("s" . magit-stage)                ; Stage changes
              ("u" . magit-unstage)))            ; Unstage changes


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
  (define-key dired-mode-map (kbd "n") 'dired-next-line)
  (define-key dired-mode-map (kbd "e") 'dired-previous-line)
  (define-key dired-mode-map (kbd "i") 'dired-find-file)
  (define-key dired-mode-map (kbd "h") 'dired-up-directory)
  (dirvish-override-dired-mode))

(use-package eglot
  :after exec-path-from-shell
  :disabled
  :ensure t
  :bind (("M-' l l" . eglot)
         ("M-' l s" . eglot-shutdown)
         ("M-' l d" . eldoc)
         ("M-' l r" . eglot-rename)
         ("M-' l R" . eglot-reconnect)
         ("M-' l f" . eglot-format-buffer)
         ("M-' l p" . eglot-code-action-quickfix)
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
  (defun my/project-try-npm (dir)
    (when-let ((root (locate-dominating-file dir "package.json")))
      (cons 'transient root)))

  ;; Add to the START of the list so it takes precedence over the default Git detection
  (add-hook 'project-find-functions #'my/project-try-npm nil nil)
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode)
                 . ("ty" "server"))))

(use-package lsp-haskell
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package lsp-mode
  :ensure t
  :bind (("M-' l l" . lsp)
         ("M-' l s" . lsp-disconnect)
         ("M-' l d" . lsp-describe-thing-at-point)
         ("M-' l r" . lsp-rename)
         ("M-' l R" . lsp-workspace-restart)
         ("M-' l f" . lsp-format-buffer)
         ("M-' l p" . lsp-execute-code-action)
         ("M-' l i" . lsp-organize-imports))
  :after exec-path-from-shell lsp-haskell yasnippet
  :commands (lsp lsp-deferred)
  :hook ((typescript-ts-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         (lsp-mode . lsp-ui-mode))

  :custom
  (lsp-disabled-clients '(pyright pylsp ruff ty-server))
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints "never")

  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 0.8)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  (defun my/project-try-npm (dir)
    (when-let ((root (locate-dominating-file dir "package.json")))
      (cons 'transient root)))
  (add-hook 'project-find-functions #'my/project-try-npm nil nil)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("ty" "server"))
                    :major-modes '(python-ts-mode python-mode)
                    :server-id 'ty-server)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-enhanced-markdown t)
  (lsp-ui-sideline-show-hover t)
  ;; Helps with rendering
  (lsp-ui-peek-always-show t))

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
  :hook ((prog-mode . snap-indent-mode)
         (sql-mode . (lambda () (snap-indent-mode -1))))
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save nil)))

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
  (projectile-enable-caching nil)
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("M-' p" . projectile-command-map)))

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
         ;; ("M-' n n" . counsel-bookmark)
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

(use-package puni
  :defer t
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

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
            (recentf-save-list)
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
(add-to-list 'pulsar-pulse-functions #'my/avy-goto-char-2-end)

(defun switch-to-last-file-buffer ()
  (interactive)
  (let ((buf (current-buffer))
        (bufs (seq-filter #'buffer-file-name (buffer-list))))
    (if (not (buffer-file-name (or (buffer-base-buffer buf) buf)))
        (switch-to-buffer (car bufs))
      (switch-to-buffer (cadr bufs)))))

;; INFO: keymaps
(define-key boon-command-map (kbd "g,") 'insert-line-above)
(define-key boon-x-map (kbd "x") 'counsel-M-x)

(define-key global-map (kbd "C-<backspace>") 'my/backward-delete-word)
(define-key global-map (kbd "C-,") 'my-duplicate-line)

(define-key global-map (kbd "M-' q") #'quit-window)
(define-key global-map (kbd "M-' M-f") #'delete-indentation)
(define-key global-map (kbd "M-' TAB") 'switch-to-last-file-buffer)
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
(define-key global-map (kbd "M-' u e") 'hs-toggle-hiding)

(define-key global-map (kbd "M-' k p") #'kill-this-buffer)
(define-key global-map (kbd "M-' k P") #'kill-buffer)
(define-key global-map (kbd "M-' k T") #'bookmark-delete)
(define-key global-map (kbd "M-' k m") #'spacemacs/switch-to-messages-buffer)
(define-key global-map (kbd "M-' k s") #'spacemacs/switch-to-scratch-buffer)

(define-key global-map (kbd "M-' h t") #'hydra-paging/body)
(define-key global-map (kbd "M-' h n") #'hydra-numbers/body)
(define-key global-map (kbd "M-' h T") #'hydra-todo/body)
(define-key global-map (kbd "M-' h z") #'hydra-zoom/body)
(define-key global-map (kbd "M-' h h") #'hydra-multiple-cursors/body)

(define-key boon-command-map (kbd "C-S-u") 'scroll-down-command)
(define-key boon-command-map (kbd "C-S-y") 'scroll-up-command)

(defun my/tscout--jump (cand)
  (when (string-match
         "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):"
         cand)
    (let ((file (match-string 1 cand))
          (line (string-to-number (match-string 2 cand)))
          (col  (string-to-number (match-string 3 cand))))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column (- col 1)))))

(defun my/tscout (&optional dir)
  (interactive)
  (let* ((default-directory
          (or dir
              ;; Projectile (preferred)
              (when (fboundp 'projectile-project-root)
                (ignore-errors (projectile-project-root)))
              ;; project.el fallback
              (and (fboundp 'project-current)
                   (when-let ((proj (project-current)))
                     (project-root proj)))
              default-directory))
         (candidates
          (process-lines "tscout" "." "-d:5")))
    (unless candidates
      (user-error "No tscout results found"))
    (my/tscout--jump
     (if (fboundp 'consult--read)
         (consult--read
          candidates
          :prompt "tscout: ")
       (completing-read
        "tscout: "
        candidates
        nil
        t))))
  )
(define-key boon-command-map (kbd "C-s") #'my/tscout)
(define-key global-map (kbd "M-' s") #'my/tscout)
(define-key global-map (kbd "M-' n s") #'boon-enclose)
