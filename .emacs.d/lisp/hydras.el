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
  ("n" shift-number-up "increase")
  ("e" shift-number-down "decrease")
  ("SPC" nil "quit"))

(defhydra hydra-paging nil
  "paging"
  ("N" #'scroll-up-command "scroll down")
  ("E" #'scroll-down-command "scroll up")
  ("n" (lambda () (interactive) (scroll-up (/ (window-body-height) 2)) (pulsar-pulse-line)) "scroll half down")
  ("e" (lambda () (interactive) (scroll-down (/ (window-body-height) 2)) (pulsar-pulse-line)) "scroll half up")
  ("SPC" nil "quit"))

(defhydra hydra-multiple-cursors nil
  ("n" mc/mark-next-like-this "mark next like this")
  ("N" mc/skip-to-next-like-this "skip to next like this")
  ("M-n" mc/unmark-next-like-this "unmark next like this")
  ("e" mc/mark-previous-like-this "mark previous like this")
  ("E" mc/skip-to-previous-like-this "skip to previous like this")
  ("M-e" mc/unmark-previous-like-this "unmark previous like this")
  ("l" mc/edit-lines "edit lines")
  ("a" mc/mark-all-like-this "mark all like this")
  ("|" mc/vertical-align "vertical align")
  ("s" mc/mark-all-in-region-regexp "mark all in region (regexp)")
  ("0" mc/insert-numbers "insert numbers")
  ("A" mc/insert-letters "insert letters")
  ("<mouse-1>" mc/add-cursor-on-click "add cursor on click")
  ("<down-mouse-1>" ignore "ignore down mouse")
  ("<drag-mouse-1>" ignore "ignore drag mouse")
  ("q" nil "quit"))

(provide 'hydras)
