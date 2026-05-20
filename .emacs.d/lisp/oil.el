;;; oil.el --- Minimal Oil.nvim-like editable directory buffer -*- lexical-binding: t; -*-

(require 'dired)
(require 'cl-lib)

(defvar oil-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'oil-save)
    (define-key map (kbd "C-c C-k") #'oil-cancel)
    map)
  "Keymap for `oil-mode'.")

(defvar-local oil--dir nil
  "The directory this Oil buffer represents.")
(defvar-local oil--orig-buf nil
  "The Dired buffer that opened this Oil buffer.")

(defun oil--insert-dir (dir)
  (erase-buffer)
  (insert (format "# Oil: %s\n" dir))
  (dolist (f (directory-files dir nil directory-files-no-dot-files-regexp))
    (insert f "\n"))
  (goto-char (point-max))
  (insert "\n# Type new filenames(each line a new file), then C-c C-c to create.\n")
  (goto-char (point-max)))

;;;###autoload
(defun oil-open (dir)
  "Open DIR in editable Oil buffer."
  (interactive "DDirectory: ")
  (let ((buf (get-buffer-create (format "*oil: %s*" (abbreviate-file-name dir)))))
    (with-current-buffer buf
      (oil-mode)
      (setq oil--dir (expand-file-name dir)
            oil--orig-buf (current-buffer))
      (oil--insert-dir dir))
    (switch-to-buffer buf)))

(defun oil--lines ()
  "Return all non-empty, non-comment lines."
  (split-string
   (replace-regexp-in-string "^#.*" "" (buffer-string))
   "\n" t "[ \t]+"))

(defun oil-save ()
  "Create new files from current buffer lines and refresh buffer."
  (interactive)
  (let* ((dir oil--dir)
         (current (directory-files dir nil directory-files-no-dot-files-regexp))
         (wanted (oil--lines))
         (added (cl-remove-if (lambda (x) (member x current)) wanted)))
    ;; Create new file
    (dolist (f added)
      (let ((path (expand-file-name f dir)))
        (unless (file-exists-p path)
          (make-empty-file path)
          (message "Created: %s" path))))
    ;; Refresh buffer content
    (let ((inhibit-read-only t))
      (oil--insert-dir dir))
    (message "Oil: created %d new files." (length added))))

(defun oil-cancel ()
  "Abort and kill Oil buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (when (buffer-live-p oil--orig-buf)
    (switch-to-buffer oil--orig-buf)))

(define-minor-mode oil-mode
  "A minimal editable directory buffer, inspired by oil.nvim."
  :lighter " Oil"
  :keymap oil-mode-map
  (setq buffer-read-only nil)
  (setq-local header-line-format "Edit filenames directly, . C-c C-c = Save, C-c C-k = Cancel."))

(provide 'oil)
;;; oil.el ends here
