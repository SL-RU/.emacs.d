
;; Enable EDE only in C/C++
(require 'ede)
;(require 'cedet)
;(require 'eieio)
;(require 'eieio-speedbar)
;(require 'eieio-opt)
;(require 'eieio-base)
(require 'ede/source)
(require 'ede/base)
(require 'ede/auto)
(require 'ede/proj)
(require 'ede/proj-archive)
(require 'ede/proj-aux)
(require 'ede/proj-comp)
(require 'ede/proj-elisp)
(require 'ede/proj-info)
(require 'ede/proj-misc)
(require 'ede/proj-obj)
;(require 'ede/proj-prog)
;(require 'ede/proj-scheme)
(require 'ede/proj-shared)
(require 'ede/cpp-root)


(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-decoration-mode 1)
(ede-enable-generic-projects)
(global-semantic-idle-scheduler-mode)
;(global-semantic-idle-completions-mode)
(global-semantic-highlight-func-mode)
(global-semantic-show-unmatched-syntax-mode)
(global-semantic-tag-folding-mode 1)

(semantic-mode 1)
(global-ede-mode t)

(ede-enable-generic-projects)

(defun my:add-semantic-to-autocomplete() 
  (setq-default ac-sources '(ac-source-semantic-raw))
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
(add-hook 'c-mode-hook 'my:add-semantic-to-autocomplete)
(add-hook 'c++-mode-hook 'my:add-semantic-to-autocomplete)


;;; MAKEFILE

(defclass ede-generic-makefile-project (ede-generic-project)
  ((buildfile :initform "Makefile")
   )
  "Generic Project for makefiles.")

(defmethod ede-generic-setup-configuration ((proj ede-generic-makefile-project) config)
  "Setup a configuration for Make."
  (oset config build-command "make")
  (oset config debug-command "gdb")
  )


(defun dennis-flycheck-get-ede-includes ()
  "Check if the current file is part of an EDE project.
If yes, set up `flycheck-clang-include-path'"
  (interactive)
  (make-variable-buffer-local 'flycheck-clang-include-path)
  (let* ((rel-includes
          (flycheck-cedet-get-cpp-includes "" (buffer-file-name)))
         (dirname (when rel-includes
                    (ede-cpp-root-project-root default-directory))))
    (when rel-includes
      (when (string-match "\\(.*\\)/$" dirname)
        (setq dirname (substring dirname (match-beginning 1) (match-end 1))))
      (setq incl-paths
            (mapcar '(lambda (arg) (concat dirname arg))
                    rel-includes))
      (setq flycheck-clang-include-path
            (append flycheck-clang-include-path incl-paths)))))

(add-hook 'c-mode-common-hook 'dennis-flycheck-get-ede-includes)
