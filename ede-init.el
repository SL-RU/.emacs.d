
;; Enable EDE only in C/C++
(require 'ede)
(require 'cedet)
(require 'eieio)
(require 'eieio-speedbar)
(require 'eieio-opt)
(require 'eieio-base)
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
(require 'ede/proj-prog)
(require 'ede/proj-scheme)
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

(ede-generic-new-autoloader "generic-makefile" "Make"
			    "Makefile" 'ede-generic-makefile-project)  


(ede-cpp-root-project "stm test"
                      :name "stm test"
                      :file "/home/lyra/b/stmtest/stmtest/Readme"
                      :include-path '("/Inc"
                                      "/Drivers/STM32F1xx_HAL_Driver/Inc"
				      "/Drivers/STM32F1xx_HAL_Driver/Inc/Legacy"
				      "/Drivers/CMSIS/Include")
                      )










