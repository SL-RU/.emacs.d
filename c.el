;; c.el -- initialize packages for c & c++

;;; Commentary:
;;; c/c++ packages
;;; Code:

(load (concat user-emacs-directory "gendoxy/gendoxy.el"))

;(load-file (concat user-emacs-directory "rtags/src/rtags.el"))
;(load-file (concat user-emacs-directory "rtags/src/helm-rtags.el"))
;(load-file (concat user-emacs-directory "rtags/src/flycheck-rtags.el"))

(add-hook 'c-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

; style I want to use in c++ mode
(c-add-style "my-style"
	     '("stroustrup"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)            ; indent by four spaces
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (innamespace . [0])
				   (brace-list-open . 0)
                                   (brace-list-intro . ++)
                                   (member-init-intro . ++)
				   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  "C++ hook."
;  (semantic-mode -1)
  (c-set-style "my-style") ; use my-style defined above)
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)


(use-package friendly-shell-command
  :ensure t
  )
(use-package s
  :ensure t
  )

(require 'helm)
(require 's)
(defun projectile-run-gdb-elf ()
  "Run arm-none-eabi-gdb and select elf"
  (interactive)
  (setq root (projectile-project-root))
  (when 'root
    (setq file (helm :sources (helm-build-sync-source "test"
                                :candidates (s-split
                                             "\n"
                                             (friendly-shell-command-to-string
                                              "find . -type f -name '*.elf'"
                                              :path root))
                                :fuzzy-match t)
                     :buffer "*helm find*"
                     :case-fold-search helm-file-name-case-fold-search))
    (when file
      (gud-gdb (s-concat "arm-none-eabi-gdb -ex \"tar ext:3333\" " (concat root file))))))

;;; c.el ends here
