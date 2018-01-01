;;;
;;; Code:

(define-derived-mode ede-viewer-mode special-mode
  "view ede projects")

(defcustom ede-viewer-find "find %s -name 'main.c' -o -name 'main.cpp' | head -n 1"
  "Bash command to find main.c."
  :group 'ede-viewer
  :type 'string)


(defun ede-project-list ()
  "Show projects listed in ede global list."
  (interactive)
  (switch-to-buffer "*projects*")
  (read-only-mode 0)
  (ede-viewer-mode)
  (ede-project-list-print)
  )

(defun ede-project-list-item-pressed (button)
  "Print BUTTON."
  (let ((ma (substring (shell-command-to-string
			(format ede-viewer-find (button-label button))) 0 -1)))
    (find-file ma)))


(defun ede-project-list-print ()
  "Print list."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Projects:\n")
    (dolist (val ede-project-directories)
      (insert "- ")
      (insert-button val
		     :type (define-button-type 'custom-button
			     'action 'ede-project-list-item-pressed
			     'follow-link t
			     'help-echo "Open project"
			     'help-args val))
      (insert "\n")
      )
    )
  )

