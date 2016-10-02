;;; flycheck-cedet.el --- Flycheck CEDET integration  -*- lexical-binding: t -*-

;; Copyright (c) 2013 David Holm <david.holm@gmail.com>
;;
;; Author: David Holm <david.holm@gmail.com>
;; Keywords: convenience language tools
;; Version: 0.1
;; Package-Requires: ((flycheck "0.13") (dash "1.2") (emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'flycheck)
(require 'ede)
;;(require 'semantic/deps)

;;;; ede project support
(defun flycheck-cedet-get-ede-cpp-project (file)
  "Get EDE C/C++ project for FILE.
If FILE does not belong to an EDE C/C++ project, or if EDE is not present, this
function returns nil."
  (when (featurep 'ede)
    (let ((project (ede-current-project (expand-file-name file))))
      (when (ede-cpp-root-project-p project)
        project))))

(defun flycheck-cedet-get-ede-cpp-project-includes (project)
  "Get include paths from EDE C/C++ PROJECT."
  (when (ede-cpp-root-project-p project)
    (let* ((root-path (directory-file-name (ede-project-root-directory project)))
           (include-paths  (oref project include-path)))
      (--map (concat root-path it) include-paths))))


;;;; Project helpers
(defun flycheck-cedet-get-cpp-includes (option-name file)
  "Construct a list of includes using OPTION-NAME for the specified FILE.
If FILE is associated with a supported project type and if it has a list of
include paths a list consisting of OPTION-NAME concatenated with each path
will be constructed."
  (--map (concat option-name it)
         (let ((ede-proj (flycheck-cedet-get-ede-cpp-project file)))
           (cond
            (ede-proj (flycheck-cedet-get-ede-cpp-project-includes ede-proj))))))

(defun flycheck-cedet-get-ede-cpp-project-definitions (project)
  "Get definitions from EDE C/C++ PROJECT."
  ;;(message "lol")
  ;(message (ede-cpp-root-project-p project))
  (when (ede-cpp-root-project-p project)
    ;;(message "kek")
    (--map (let ((macro (car it))
                 (value (cdr it)))
             (concat macro (unless (string-blank-p value)
			     ;;(message value)
                             (concat "=" value))))
           (ede-preprocessor-map project))))


(defun flycheck-cedet-get-cpp-definitions (option-name file)
  "Construct a list of definitions using OPTION-NAME for the specified FILE.
If FILE is associated with a supported project type and if it has a list of
definitions a list consisting of OPTION-NAME concatenated with the name of the
definition followed by an optional =<value> will be constructed."
  ;;(interactive)
  (message file)
  (--map (let ((macro (car it))
               (value (cdr it)))
           (concat option-name macro
                   (unless (string= "" value)
		     ;;(message value)
                     (concat "=" value))))
         (let ((ede-proj (flycheck-cedet-get-ede-cpp-project file)))
           (cond
            (ede-proj (ede-preprocessor-map ede-proj))))))

(defun flycheck-setup-from-cedet ()
  "Setup flycheck include path and definitions using cedet."
  (interactive)
;  (let ((project (ede-current-project)))
  (let* ((fname (or (buffer-file-name (current-buffer)) default-directory))
	 (current-dir (file-name-directory fname))
	 (projectp (ede-current-project current-dir))
	 (l (flycheck-cedet-get-ede-cpp-project-definitions projectp))
         (project (ede-current-project current-dir)))
    (when project
      (flycheck-select-checker 'c/c++-gcc)
      ;(message (flycheck-cedet-get-ede-cpp-project-includes project))
      (setq flycheck-gcc-include-path (flycheck-cedet-get-ede-cpp-project-includes project))
      ;;(message "g")
      ;(message l)
      (setq flycheck-gcc-definitions l))))
      ;;(message "ok"))))

(provide 'flycheck-cedet)
;;; flycheck-cedet.el ends here
