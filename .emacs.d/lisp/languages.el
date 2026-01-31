(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package xonsh-mode
  :ensure t)

(use-package kotlin-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package fish-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package nim-mode
  :ensure t)

(use-package zig-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("C-c C-c C-b" . rustic-cargo-build)
              ("C-c C-c C-r" . rustic-cargo-run)
              ("C-c C-c C-t" . rustic-cargo-test)
              ("C-c C-c C-c" . rustic-compile))   ; or whatever keys you like
  :config
  (setq rustic-compile-command "cargo build --message-format=json-diagnostic-short"))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'odin-mode)
(require 'cmake-mode)
(require 'kdl-ts-mode)

(provide 'languages)
