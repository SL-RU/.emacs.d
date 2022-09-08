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
  (c-set-style "my-style") ; use my-style defined above
  (auto-fill-mode))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)


;;; c.el ends here
