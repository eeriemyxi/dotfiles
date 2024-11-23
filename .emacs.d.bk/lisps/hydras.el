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
  ("l" evil-mc-make-cursor-move-next-line "make next line")
  ("y" evil-mc-make-cursor-move-prev-line "make prev line")
  ("n" evil-mc-make-and-goto-next-match "make and next")
  ("e" evil-mc-make-and-goto-prev-match "make and prev")
  ("N" evil-mc-skip-and-goto-next-match "skip and next")
  ("E" evil-mc-skip-and-goto-prev-match "skip and prev")
  ("a" evil-mc-make-all-cursors "make all")
  ("u" evil-mc-undo-all-cursors "undo all")
  ("U" evil-mc-undo-last-added-cursor "undo last added")
  ("SPC" nil "quit"))

(provide 'hydras)
