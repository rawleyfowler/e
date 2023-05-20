;;; package --- Rawley Fowler's custom functions
;;; Commentary:
;;; My custom functions
;;; Code:

(defun rf/indent-buffer ()
  "Apply indenting to whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun rf/goto-dashboard ()
  "Go back to dashboard buffer."
  (interactive)
  (switch-to-buffer "*dashboard*")
  (dashboard-mode)
  (dashboard-refresh-buffer))

(defun rf/kill-inner-word ()
  "It's ciw from Vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(defun rf/open-in-firefox ()
  "Attempts to open the current buffers file in Firefox."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))

(provide 'funcs)
;;; funcs.el ends here
