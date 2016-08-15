(defun get-project-root-dir ()
  "Return path of current project"
  (interactive)
  (let* ((fname (or (buffer-file-name (current-buffer)) default-directory))
	 (current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (message "lol")
    (if (null prj)
	(progn (message "buffer has no project")
	       nil)
      (progn (message (concat "Project dir: " (ede-project-root-directory prj)))
	     (ede-project-root-directory prj)))
    ))


(defun stm32-generate-makefile ()
  "Generate or regenerate Makefile from CubeMX generated code"
  (interactive)
  (let ((dir (get-project-root-dir)))
    (when dir
      (let ((pth (concat dir "CubeMX2Makefile.py")))
	(when (file-exists-p pth)
	  (progn	   
	    (message (shell-command-to-string (concat "/usr/bin/python " pth " " dir)))
	    (message "ok"))
	))
      )
    )
  )

(defun my-load-project ()
   "Load project.el of current project"
   (interactive)
   (let ((dir (get-project-root-dir)))
    (when dir
      (let ((pth (concat dir "project.el"))
	(when (file-exists-p pth)
	  (progn
	    (load-file pth)
	    (message (concat pth " loaded"))
	))
      )
    )
   )
