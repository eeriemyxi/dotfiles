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
  (save-excursion
    (let ((sort-fold-case t))
      (sort-regexp-fields reverse "\\w+" "\\&" beg end))))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
  See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun my-duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(provide 'funcs)
