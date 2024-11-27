(defun ey/sane-newline-and-indent ()
  "Insert a newline and indent based on the current context.

This function provides context-aware indentation for structured code, such as
Python. Its behavior is as follows:

1. Immediate newline condition:
   - If the current line is not entirely whitespace AND the cursor is not at
     the end of the line, insert a newline at the cursor position. Then align
     the cursor vertically with the first non-whitespace character of the
     previous line. Stop the process here.

2. Context-aware indentation:
   - If the current line ends with a colon (`:`), it adds an additional level
     of indentation (equal to `tab-width') after the newline.
   - If there is no colon at the end of the line:
     - If the previous line contains non-blank characters, match its
       indentation level.
     - If the previous line is blank, the new line matches its
       whitespace level.

I would say this function is ideal for languages with block-based syntax
(e.g., Python) or other structured indentation rules.

Caveats of this function:
    - Does not clear trailing whitespaces
    - Does not continue comments, hyphens or other symbols in newlines
    - Does not play well with a whitespace-filled line that ends with a colon"
  (interactive)
  (or
   ;; Immediate newline condition
   (let ((current-line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
     (if (and (not (string-match-p "^[ \t]*$" current-line))
              (< (current-column) (length current-line)))
         (progn
           (newline)
           (indent-to (save-excursion
                        (forward-line -1)
                        (back-to-indentation)
                        ;; (beginning-of-line) ; this iinstead of
                                        ; `back-to-indentation' if we want
                                        ; staight vertical \n's for some reason
                        (current-column)))
           t)
       nil))
   ;; Context aware indentation
   (let* ((col-pos (current-column))
          (indent-by-whitespace (save-excursion
                                  (beginning-of-line)
                                  (if (looking-at-p "^[ \t]*$")
                                      (move-to-column col-pos)
                                    (back-to-indentation)
                                    (current-column))))
          ;; extra indent if the current line ends with `:'
          (extra-indent (if (save-excursion
                              (end-of-line)
                              (looking-back ":\\s-*"))
                            4
                          0)))
     (end-of-line)
     (newline)
     (indent-to (+ indent-by-whitespace extra-indent)))))

(provide 'sane-indent)
