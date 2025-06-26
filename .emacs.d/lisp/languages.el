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

(use-package android-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'odin-mode)
(require 'cmake-mode)
(require 'kdl-ts-mode)

(provide 'languages)
