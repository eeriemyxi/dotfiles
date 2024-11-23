;;; post-init.el --- [core init file] -*- no-byte-compile: t; lexical-binding: t; -*-

(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq show-trailing-whitespace t)
(setq display-line-numbers-type 'relative)
(setq indent-line-function 'insert-tab)
(setq default-frame-alist '((font . "JetBrainsMono Nerd Font Mono 13")))
(global-display-line-numbers-mode)
(global-auto-revert-mode t)

;; INFO: theme
(use-package gruvbox-theme :ensure t :config (load-theme 'gruvbox-dark-medium t))

;; INFO: custom files
(add-to-list 'load-path (concat user-emacs-directory "/lisps"))

;; INFO: macros
(require 'macros)

;; INFO: packages
(use-package compile-angel
  :disabled
  :ensure t
  :demand t
  :custom
  (compile-angel-verbose nil)
  :config
  (setq compile-angel-excluded-files-regexps '("lisps/hydras\.el$"))
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; INFO: tree-sitter
(require 'treesit)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

(setq major-mode-remap-alist
      '((bash-mode       . bash-ts-mode)
        (c-or-c++-mode   . c-or-c++-ts-mode)
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (csharp-mode     . csharp-ts-mode)
        (css-mode        . css-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (yaml-mode       . yaml-ts-mode)
        (js2-mode        . js-ts-mode)
        (tsx-mode        . tsx-ts-mode)
        (json-mode       . json-ts-mode)
        (python-mode     . python-ts-mode)
        (ruby-mode       . ruby-ts-mode)
        (rust-mode       . rust-ts-mode)
        (toml-mode       . toml-ts-mode)
        (go-mode         . go-ts-mode)))

(use-package magit :ensure t)

(use-package dtrt-indent
  :ensure t
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight)
  :config
  (dtrt-indent-global-mode))

(use-package outline-indent
  :ensure t
  :custom
  (outline-indent-ellipsis "🢓")
  :config
  (dolist (hook '(python-mode python-ts-mode-hook))
    (add-hook hook #'(lambda()
                       (setq-local outline-indent-default-offset 4)
                       (setq-local outline-indent-shift-width 4))))

  (dolist (hook '(yaml-mode yaml-ts-mode-hook))
    (add-hook hook #'(lambda()
                       (setq-local outline-indent-default-offset 2)
                       (setq-local outline-indent-shift-width 2)))
    (outline-indent-minor-mode)))

(use-package titlecase
  :ensure t
  :bind (:map evil-normal-state-map
              ("<leader>T" . titlecase-region))
  :bind (:map evil-visual-state-map
              ("<leader>T" . titlecase-region)))

(use-package evil-mc
  :ensure t
  :after evil evil-collection evil-colemak-basics
  :config
  (global-evil-mc-mode 1)

  ;; INFO: https://github.com/gabesoft/evil-mc/issues/41#issuecomment-890887060
  (setq evil-mc-custom-known-commands
        '((custom/evil-mc-evil-escape-move-back-fake-cursors
           (:default . evil-mc-execute-default-call))))
  (defun custom/evil-mc-evil-escape-move-back-fake-cursors ()
    "Move the fake cursors to the left once,
       unless they already are at the beginning of the line."
    (unless (bolp) (backward-char)))
  (defun custom/evil-mc-evil-escape-fix ()
    "Prevent the first evil-escape-key-sequence key (default: f),
       from being typed at all of the fake cursors.
       And move back the fake cursors when the real insert state cursor is at the end
       of a line."
    (when (evil-mc-has-cursors-p)
      (evil-mc-pause-cursors)
      (run-with-idle-timer
       0 nil '(lambda ()
                (evil-mc-resume-cursors)
                (let ((evil-mc-command '((:name . custom/evil-mc-evil-escape-move-back-fake-cursors))))
                  (evil-mc-execute-for-all))))))
  (advice-add 'evil-escape-func :before 'custom/evil-mc-evil-escape-fix))

(use-package evil-snipe
  :ensure t
  :config
  ;; (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  :custom
  (evil-snipe-override-evil-repeat-keys nil)
  (evil-snipe-scope 'line)
  (evil-snipe-repeat-scope 'line)
  (evil-snipe-smart-case t)
  (evil-snipe-tab-increment t)
  :custom-face
  (evil-snipe-matches-face ((t (:inherit lazy-highlight)))))

(use-package shell-pop
  :ensure t
  :bind (:map evil-normal-state-map
              ("<leader>'" . shell-pop)))

(use-package rainbow-delimiters :ensure t :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package dirvish
  :after evil-colemak-basics
  :ensure t
  :bind (:map evil-normal-state-map
              ("<leader>f" . dired-jump))
  :config
  (dirvish-override-dired-mode))

(use-package eglot
  :ensure t
  :bind (:map evil-normal-state-map
              ("<leader>ll" . eglot)
              ("<leader>L"  . eglot)
              ("<leader>s"  . eglot-shutdown)
              ("<leader>ld" . eldoc)
              ("<leader>lr" . eglot-rename)
              ("<leader>lR" . eglot-reconnect)
              ("<leader>lf" . eglot-format-buffer)
              ("<leader>li" . eglot-code-action-organize-imports))
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
               `(python-ts-mode . ,(eglot-alternatives
                                    '(;("ruff-lsp")
                                      ("pylsp"))))))
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "kt")
  (setq-default evil-escape-delay 0.2)
  (evil-escape-mode))

(use-package centaur-tabs
  :ensure t
  :demand
  :bind (:map evil-normal-state-map
              ("<leader>t" . centaur-tabs-ace-jump)
              ("C-<left>" . centaur-tabs-backward)
              ("C-<right>" . centaur-tabs-forward))
  :config
  (centaur-tabs-mode t)
  (with-eval-after-load 'centaur-tabs
    (custom-set-faces
     '(centaur-tabs-default
       ((t (:background "#282828" :foreground "#928374")))) ; Gruvbox dark background and neutral gray
     '(centaur-tabs-unselected
       ((t (:background "#282828" :foreground "#a89984")))) ; Medium gray and light neutral
     '(centaur-tabs-selected
       ((t (:background "#504945" :foreground "#ebdbb2")))) ; Dark brown and light tan
     '(centaur-tabs-unselected-modified
       ((t (:background "#3c3836" :foreground "#fabd2f")))) ; Same as unselected, golden foreground
     '(centaur-tabs-selected-modified
       ((t (:background "#504945" :foreground "#fabd2f")))))) ; Same as selected, golden foreground
  (setq-default centaur-tabs-ace-jump-keys
                '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?c ?k ?m ?w ?y))
  (setq-default centaur-tabs-set-bar 'right)
  (setq-default centaur-tabs-style "slant")
  (centaur-tabs-headline-match))

(use-package hydra :ensure t)

(use-package simpleclip
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-v" . simpleclip-paste))
  :bind (:map evil-visual-state-map
              ("C-v" . simpleclip-paste))
  :bind (:map evil-insert-state-map
              ("C-v" . simpleclip-paste))
  :config (simpleclip-mode 1))

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

(use-package yasnippet
  :ensure t
  :init
  (use-package yasnippet-snippets
    :ensure t)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

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
                            company-keywords
                            company-yasnippet)))
  (global-company-mode)
  (company-tng-configure-default))

(use-package exec-path-from-shell :ensure t)

(use-package avy
  :ensure t
  :bind (:map evil-normal-state-map
              ("<leader>N" . avy-goto-char-timer)
              ("<leader>n" . avy-goto-char-2))
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  (avy-style 'de-bruijn)
  (avy-keys (string-to-list "arstneiofu")))

(use-package evil-numbers
  :ensure t)

(use-package flx
  :ensure t
  :config)

(use-package counsel
  :ensure t
  :after flx
  :bind (:map evil-normal-state-map
              ("<leader>a" . counsel-grep-or-swiper)
              ("<leader>p" . counsel-M-x)
              ("<leader>i" . counsel-file-jump)
              ("<leader>I" . swiper-isearch)
              ("<leader>ht" . counsel-bookmark)
              ("<leader>hf" . counsel-buffer-or-recentf))
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package evil-surround
  :after evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("<leader>ds" . evil-surround-region)
              ("<leader>dc" . evil-surround-change)
              ("<leader>dd" . evil-surround-delete))
  :bind (:map evil-visual-state-map
              ("<leader>ds" . evil-surround-region)
              ("<leader>dc" . evil-surround-change)
              ("<leader>dd" . evil-surround-delete))

  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))
     (?\8 . ("*" . "*"))
     (?\* . ("**" . "**"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))

(use-package evil-colemak-basics
  :after evil evil-snipe
  :demand t
  :ensure t
  :delight
  :init
  (setq evil-colemak-basics-char-jump-commands 'evil-snipe)
  :config
  (global-evil-colemak-basics-mode))

(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode 1))

;; INFO: startup stuff
(add-hook 'after-init-hook #'dired-jump)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

(when (daemonp)
  (exec-path-from-shell-initialize))

(add-hook 'focus-out-hook (lambda ()
                            (save-some-buffers t)))

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory ".saves"))))

;; INFO: emacs keybinds in insert mode
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-o") 'evil-execute-in-normal-state)

;; INFO: additional code
(require 'funcs)
(require 'hydras)

(add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-scroll-page-up)
(add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-scroll-page-down)
(add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-scroll-up)
(add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-avy-goto-char-timer)
(add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-scroll-down)

;; INFO: keymaps
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

;; INFO: OCCUPIED: [I, L, N, Q, SPC, T, TAB, Y, a, c, d, f, h, i, l, n, p, q, ', s, t, u, w, y, z]
(define-key evil-normal-state-map (kbd "<leader>TAB") #'mode-line-other-buffer)
(define-key evil-insert-state-map (kbd "C-,")         #'my-duplicate-line)
(define-key evil-normal-state-map (kbd "C-,")         #'my-duplicate-line)
(define-key evil-normal-state-map (kbd "<leader>y") (lambda () (interactive)
                                                      (find-file (concat user-emacs-directory "/post-init.el"))))
(define-key evil-normal-state-map (kbd "<leader>Y") (lambda () (interactive) (load-file user-init-file)))
(define-key evil-normal-state-map (kbd "<leader>Q")   #'kill-emacs)
(define-key evil-normal-state-map (kbd "<leader>q")   #'evil-quit-all)
(define-key evil-normal-state-map (kbd "<leader>z")   #'hydra-zoom/body)
(define-key evil-normal-state-map (kbd "<leader>s")   #'hydra-cursors/body)
(define-key evil-normal-state-map (kbd "<leader>SPC") #'hydra-paging/body)

(define-key evil-normal-state-map (kbd "<leader>wn") #'windmove-down)
(define-key evil-visual-state-map (kbd "<leader>wn") #'windmove-down)
(define-key evil-normal-state-map (kbd "<leader>we") #'windmove-up)
(define-key evil-visual-state-map (kbd "<leader>we") #'windmove-up)
(define-key evil-normal-state-map (kbd "<leader>wh") #'windmove-right)
(define-key evil-visual-state-map (kbd "<leader>wh") #'windmove-right)
(define-key evil-normal-state-map (kbd "<leader>wi") #'windmove-left)
(define-key evil-visual-state-map (kbd "<leader>wi") #'windmove-left)
(define-key evil-normal-state-map (kbd "<leader>wy") #'delete-window)

(define-key evil-normal-state-map (kbd "<leader>cn") #'next-error)
(define-key evil-normal-state-map (kbd "<leader>ce") #'previous-error)
(define-key evil-normal-state-map (kbd "<leader>cc") #'compile)
(define-key evil-normal-state-map (kbd "<leader>cr") #'recompile)
(define-key evil-normal-state-map (kbd "<leader>ck") #'kill-compilation)
(define-key evil-normal-state-map (kbd "<leader>cy") #'spacemacs/show-hide-compilation-window)
(define-key evil-normal-state-map (kbd "<leader>cu") #'spacemacs/switch-to-compilation-buffer)

(define-key evil-normal-state-map (kbd "<leader>hp") #'kill-this-buffer)
(define-key evil-normal-state-map (kbd "<leader>hP") #'kill-buffer)
(define-key evil-normal-state-map (kbd "<leader>hT") #'bookmark-delete)
(define-key evil-normal-state-map (kbd "<leader>hm") #'spacemacs/switch-to-messages-buffer)
(define-key evil-normal-state-map (kbd "<leader>hs") #'spacemacs/switch-to-scratch-buffer)

(define-key evil-normal-state-map (kbd "<leader>uu") #'delete-trailing-whitespace)
(define-key evil-visual-state-map (kbd "<leader>ua") #'align-regexp)
(define-key evil-normal-state-map (kbd "<leader>un") #'hydra-numbers/body)
(define-key evil-visual-state-map (kbd "<leader>un") #'hydra-numbers/body)
(define-key evil-normal-state-map (kbd "<leader>ut") #'hydra-todo/body)
(define-key evil-normal-state-map (kbd "<leader>uh") #'evil-ex-nohighlight)
(define-key evil-normal-state-map (kbd "<leader>uc") #'comment-or-uncomment-region)
(define-key evil-visual-state-map (kbd "<leader>us") (lambda () (interactive) (evil-ex "'<,'>s/")))
(define-key evil-visual-state-map (kbd "<leader>uS") #'query-replace)

;; INFO: Emacs stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-redo)
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(aggressive-indent-mode elisp-format company markdown-mode
                            dirvish hl-todo fic-mode pulsar shell-pop simpleclip hydra
                            exec-path-from-shell evil-escape centaur-tabs evil-commentary
                            titlecase evil-colemak-basics evil-mode gruvbox-theme))
 '(shell-pop-shell-type
   '("terminal" "*terminal*"
     (lambda nil
       (term shell-pop-term-shell)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-tabs-default ((t (:background "#282828" :foreground "#928374"))))
 '(centaur-tabs-selected ((t (:background "#504945" :foreground "#ebdbb2"))))
 '(centaur-tabs-selected-modified ((t (:background "#504945" :foreground "#fabd2f"))))
 '(centaur-tabs-unselected ((t (:background "#282828" :foreground "#a89984"))))
 '(centaur-tabs-unselected-modified ((t (:background "#3c3836" :foreground "#fabd2f")))))
