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

(use-package caddyfile-mode
  :ensure t
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("C-c C-c C-b" . rustic-cargo-build)
              ("C-c C-c C-r" . rustic-cargo-run)
              ("C-c C-c C-t" . rustic-cargo-test)
              ("C-c C-c C-c" . rustic-compile))   ; or whatever keys you like
  :config
  (setq rustic-compile-command "cargo build --message-format=json-diagnostic-short")
  :custom
  ;; Automatically format with rustfmt on save (optional, but highly recommended)
  (rust-format-on-save t))

(use-package web-mode
  :ensure t
  :mode ("\\.tsx\\'" . web-mode)
  :custom
  ;; I noticed your indentation was set to 4 in your logs
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  :config
  ;; Tell web-mode to treat .tsx files as JSX/TSX content
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.tsx\\'")))

;; 2. Pure TypeScript (for standard .ts files)
(use-package php-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 4))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'odin-mode)
(require 'cmake-mode)
(require 'kdl-ts-mode)
(require 'oil)
;; (require 'term-cursor)
;; (global-term-cursor-mode)

(provide 'languages)
