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

(provide 'hydras)
