;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default show-trailing-whitespace t)
(setq display-line-numbers-type 'relative)
(setq indent-line-function 'insert-tab)
(setq evil-want-keybinding nil)
(setq default-frame-alist '((font . "JetBrainsMono Nerd Font Mono 13")))
(global-display-line-numbers-mode)
(global-auto-revert-mode t)
(add-hook 'after-init-hook #'dired-jump)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

;; INFO: theme
(use-package gruvbox-theme :ensure t :config (load-theme 'gruvbox-dark-medium t))

;; INFO: packages
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
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map
    "n" 'dired-next-line
    "e" 'dired-previous-line
    "h" 'dired-up-directory
    "y" 'dired-find-file
    (kbd "M-C-e") 'dired-tree-up
    (kbd "M-C-n") 'dired-tree-down
    (kbd "M-C-h") 'dired-prev-subdir
    (kbd "M-C-i") 'dired-next-subdir
    (kbd "* C-e") 'dired-prev-marked-file
    (kbd "* C-n") 'dired-next-marked-file))

;; INFO: tree-sitter
(require 'treesit)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
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
(use-package titlecase :ensure t)

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

(use-package shell-pop :ensure t)

(use-package rainbow-delimiters :ensure t :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package dirvish
  :after evil-colemak-basics
  :ensure t
  :config
  (dirvish-override-dired-mode))

(use-package eglot
  :ensure t
  :defer t
  :commands (eglot
             eglot-rename
             eglot-format-buffer
             eglot-shutdown
             eglot-reconnect
             eglot-code-action-organize-import)
  :custom
  (eglot-report-progress nil)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ,(eglot-alternatives
                                    '(;("ruff-lsp")
                                      ("pylsp"))))))

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files))

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

(use-package simpleclip :ensure t :config (simpleclip-mode 1))

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

;; INFO: alternative to company
;; (use-package auto-complete
;;   :ensure t
;;   :config (ac-config-default) (setq ac-use-fuzzy t) (setq ac-auto-show-menu 0.4) (global-auto-complete-mode))

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
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  (avy-style 'de-bruijn)
  (avy-keys (string-to-list "arstneiofu")))

(use-package evil-numbers
  :ensure t)

(use-package evil-colemak-basics
  :after evil evil-snipe
  :demand t
  :ensure t
  :delight
  :init
  (setq evil-colemak-basics-char-jump-commands 'evil-snipe)
  :config
  (global-evil-colemak-basics-mode))

;; INFO: startup stuff
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

(defun my-save-if-bufferfilename ()
  (when (buffer-file-name)
    (save-some-buffers t)))
(add-hook 'evil-insert-state-exit-hook 'my-save-if-bufferfilename)

(defun spacemacs//get-scratch-buffer-create ()
  (or (get-buffer "*scratch*")
      (let ((scratch (get-buffer-create "*scratch*")))
        (with-current-buffer scratch
          (add-hook 'kill-buffer-hook
                    #'spacemacs//confirm-kill-buffer
                    nil t)
          (when (and (not (eq major-mode dotspacemacs-scratch-mode))
                     (fboundp dotspacemacs-scratch-mode))
            (funcall dotspacemacs-scratch-mode)
            (run-hooks 'spacemacs-scratch-mode-hook)))
        scratch)))

(defun spacemacs/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
  if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((scratch (spacemacs//get-scratch-buffer-create)))
    (if arg
        (switch-to-buffer-other-window scratch)
      (switch-to-buffer scratch))))

(defun spacemacs/switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer.
  if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))
    (when (evil-evilified-state-p)
      (evil-normal-state))))

(defun spacemacs/show-hide-compilation-window ()
  "Show/Hide the window containing the compilation buffer."
  (interactive)
  (when-let* ((buffer next-error-last-buffer))
    (if (get-buffer-window buffer 'visible)
        (delete-windows-on buffer)
      (spacemacs/switch-to-compilation-buffer))))

(defun spacemacs/switch-to-compilation-buffer ()
  "Go to last compilation buffer."
  (interactive)
  (if (buffer-live-p next-error-last-buffer)
      (pop-to-buffer next-error-last-buffer)
    (user-error "There is no compilation buffer")))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
  Prefixed with negative \\[universal-argument], sorts in reverse.

  The variable `sort-fold-case' determines whether alphabetic case
  affects the sort order.

  See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
  See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun my-duplicate-line ()
  "Duplicate current line"
  (interactive)
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(defhydra hydra-zoom nil
  "zoom"
  ("n" text-scale-increase "in")
  ("e" text-scale-decrease "out")
  ("SPC" nil "quit"))

(defhydra hydra-todo nil
  "todo"
  ("n" hl-todo-next "next")
  ("e" hl-todo-previous "previous")
  ("o" hl-todo-occur "occur")
  ("i" hl-todo-insert "insert")
  ("SPC" nil "quit"))

(defhydra hydra-numbers nil
  "numbers"
  ("n" evil-numbers/inc-at-pt "increase")
  ("e" evil-numbers/dec-at-pt "decrease")
  ("SPC" nil "quit"))

(defhydra hydra-paging nil
  "paging"
  ("N" #'evil-scroll-page-down "scroll down")
  ("E" #'evil-scroll-page-up "scroll down")
  ("n" #'evil-scroll-down "scroll half down")
  ("e" #'evil-scroll-up "scroll half down")
  ("SPC" nil "quit"))

(defhydra hydra-cursors nil
  "cursors"
  ("h" evil-mc-make-cursor-here "make here")
  ("n" evil-mc-make-and-goto-next-match "make and next")
  ("e" evil-mc-make-and-goto-prev-match "make and prev")
  ("N" evil-mc-skip-and-goto-next-match "skip and next")
  ("E" evil-mc-skip-and-goto-prev-match "skip and prev")
  ("a" evil-mc-make-all-cursors "make all")
  ("u" evil-mc-undo-all-cursors "undo all")
  ("U" evil-mc-undo-last-added-cursor "undo last added")
  ("SPC" nil "quit"))

(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode 1)
  (add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-scroll-page-up)
  (add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-scroll-page-down)
  (add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-scroll-up)
  (add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-avy-goto-char-timer)
  (add-to-list 'pulsar-pulse-functions #'hydra-paging/evil-scroll-down))

;; INFO: keymaps
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

;; INFO: OCCUPIED: Q, SPC, T, Y, a, c, f, h, l, n, N, p, q, s, ', t, u, w, y, z
(define-key evil-normal-state-map (kbd "<leader>N")   #'avy-goto-char-timer)
(define-key evil-normal-state-map (kbd "<leader>n")   #'avy-goto-char-2)
(define-key evil-normal-state-map (kbd "<leader>f")   #'dired-jump)
(define-key evil-normal-state-map (kbd "<leader>a")   #'helm-do-grep-ag)
(define-key evil-normal-state-map (kbd "<leader>'")   #'shell-pop)
(define-key evil-normal-state-map (kbd "<leader>TAB") #'mode-line-other-buffer)
(define-key evil-normal-state-map (kbd "C-<left>")    #'centaur-tabs-backward)
(define-key evil-normal-state-map (kbd "C-<right>")   #'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "C-v")         #'simpleclip-paste)
(define-key evil-insert-state-map (kbd "C-v")         #'simpleclip-paste)
(define-key evil-visual-state-map (kbd "C-v")         #'simpleclip-paste)
(define-key evil-insert-state-map (kbd "C-,")         #'my-duplicate-line)
(define-key evil-normal-state-map (kbd "C-,")         #'my-duplicate-line)
(define-key evil-normal-state-map (kbd "<leader>p")   #'helm-M-x)
(define-key evil-normal-state-map (kbd "<leader>y") (lambda () (interactive)
                                                      (find-file (concat user-emacs-directory "/post-init.el"))))
(define-key evil-normal-state-map (kbd "<leader>Y") (lambda () (interactive) (load-file user-init-file)))
(define-key evil-normal-state-map (kbd "<leader>Q")   #'kill-emacs)
(define-key evil-normal-state-map (kbd "<leader>q")   #'evil-quit-all)
(define-key evil-normal-state-map (kbd "<leader>t")   #'centaur-tabs-ace-jump)
(define-key evil-normal-state-map (kbd "<leader>z")   #'hydra-zoom/body)
(define-key evil-normal-state-map (kbd "<leader>s")   #'hydra-cursors/body)
(define-key evil-normal-state-map (kbd "<leader>SPC") #'hydra-paging/body)
(define-key evil-normal-state-map (kbd "<leader>T")   #'titlecase-region)
(define-key evil-visual-state-map (kbd "<leader>T")   #'titlecase-region)

(define-key evil-normal-state-map (kbd "<leader>ll") #'eglot)
(define-key evil-normal-state-map (kbd "<leader>L")  #'eglot)
(define-key evil-normal-state-map (kbd "<leader>ls") #'eglot-shutdown)
(define-key evil-normal-state-map (kbd "<leader>ld") #'eldoc)
(define-key evil-normal-state-map (kbd "<leader>lr") #'eglot-rename)
(define-key evil-normal-state-map (kbd "<leader>lR") #'eglot-reconnect)
(define-key evil-normal-state-map (kbd "<leader>lf") #'eglot-format-buffer)
(define-key evil-normal-state-map (kbd "<leader>li") #'eglot-code-action-organize-import)

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

(define-key evil-normal-state-map (kbd "<leader>hp") #'kill-buffer-and-window)
(define-key evil-normal-state-map (kbd "<leader>ht") #'helm-filtered-bookmarks)
(define-key evil-normal-state-map (kbd "<leader>hf") #'helm-buffers-list)
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
   '(aggressive-indent-mode elisp-format company markdown-mode dirvish hl-todo fic-mode pulsar shell-pop simpleclip hydra exec-path-from-shell evil-escape centaur-tabs evil-commentary helm titlecase evil-colemak-basics evil-mode gruvbox-theme))
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
