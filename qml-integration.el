;;; qml-integration.el --- This is the short description  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darlan@darlan-notebook>
;; Keywords: QML

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide functions to easily run qmlscene and qmltestrunner with any
;; of the QML files in the project.

;;; Code:
(require 'project)
(require 's)
(require 'qml-mode)


(defgroup qml-integration nil "Easily run qmlscene and qmltestrunner with QML files in your project." :group 'tools :prefix "qml-integration-")


(defcustom qml-integration-qml-root-folder nil "Root folder from where to run qmlscene." :type '(directory) :safe #'stringp)


(defcustom qml-integration-import-directories '(".")
  "Directories to add to include when running qmlscene or qmltestrunner.

Add here any folders with custom components in your project."
  :type '(repeat string) :safe #'listp)

(defcustom qml-integration-qt-quick-controls-style nil "Style to use with qmlscene." :type '(string) :safe #'stringp)


(defcustom qml-integration-qmlscene-extra-args ""
  "Extra arguments passed to qmlscene.
Do not include import folders here. For that use the
`qml-integration-import-directories' variable instead." :type
  '(string))


(defcustom qml-integration-qmltestrunner-extra-args "-silent"
  "Extra arguments passed to qmltestrunner.
Do not include import folders here. For that use the
`qml-integration-import-directories' variable instead." :type
  '(string))


(defconst qml-integration-system-styles '("Fusion" "material" "Universal" "Plasma") "Qt Quick system styles to choose from.")


(defcustom qml-integration-user-styles nil "A list with user user styles." :type '(repeat string) :safe #'listp)


(defcustom qml-integration-ignored-paths '("*/.*" "./build*")
  "List of ignored paths when using 'find' program to find QML files.

Note: This is only used when the 'fd' program is not available.

For each 'directory' in `qml-integration-ignored-paths', the string
'-not -path <directory>' will be added to the find command." :type '(repeat string) :safe #'listp)


(defun qml-integration--get-qmlscene-import-directories-string ()
  "Get a string with import statements to pass to qmlscene."
  (s-join " " (mapcar #'(lambda (arg) (concat "-I " arg)) qml-integration-import-directories)))


(defun qml-integration--get-qmltestrunner-import-directories-string ()
  "Get a string with import statements to pass to qmltestrunner."
  (s-join " " (mapcar #'(lambda (arg) (concat "-import " arg)) qml-integration-import-directories)))


(defun qml-integration--get-fd-command-string (qmlquery)
  "Get the command to find the QML files matching `QMLQUERY'.

This uses the 'fd' program."
  (format "fd -t f %s %s" qmlquery (project-root (project-current)))
  )


(defun qml-integration--get-find-command-string (qmlquery)
  "Get the command to find the QML files matching `QMLQUERY'.

This uses the 'find'  program."
  (cond
   ((eq (length qml-integration-ignored-paths) 0)
    (format "find %s -type f -iname \"%s\""
          (project-root (project-current))
          qmlquery)
    )

   (t
    (format "find %s %s -type f -iname \"%s\""
          (project-root (project-current))
          (s-join " " (mapcar #'(lambda (elem) (format "-not -path \"%s\"" elem) ) qml-integration-ignored-paths))
          qmlquery))
   ))


(defun qml-integration--get-qml-files-using-fd ()
  "Get a list with all QML files in the project using the 'fd' program."
  (split-string (shell-command-to-string (qml-integration--get-fd-command-string ".qml$")))
  )

(defun qml-integration--get-qml-files-using-find ()
  "Get a list with all QML files in the project using the 'find' program."
  (split-string (shell-command-to-string (qml-integration--get-find-command-string "*qml"))))



(defun qml-integration-get-qml-files ()
  "Get a list with all QML files in the project."
  (if (executable-find "fd")
      (qml-integration--get-qml-files-using-fd)
    (qml-integration--get-qml-files-using-find)))



(defun qml-integration--get-qml-test-files-using-fd ()
  "Get a list with all QML files in the project using the 'fd' program."
  (split-string (shell-command-to-string (qml-integration--get-fd-command-string "\"tst_.*.qml$\""))))


(defun qml-integration--get-qml-test-files-using-find ()
  "Get a list with all QML files in the project using the 'find' program."
  (split-string (shell-command-to-string (qml-integration--get-find-command-string "tst_*.qml"))))



(defun qml-integration-get-qml-test-files ()
  "Get a list with all QML files in the project."
  (if (executable-find "fd")
      (qml-integration--get-qml-test-files-using-fd)
    (qml-integration--get-qml-test-files-using-find)
    ))


(defun qml-integration--get-styles ()
  "Get the available styles."
  (append qml-integration-user-styles qml-integration-system-styles)
  )


(defun qml-integration-choose-qml-style ()
  "Choose a style for QML controls."
  (interactive)
  (setq qml-integration-qt-quick-controls-style
        (completing-read "Choose style: "
                         (qml-integration--get-styles))))


(defun qml-integration--get-style-string ()
  "Get the string that must come before qmlscene and qmltestrunner commands."
  (if qml-integration-qt-quick-controls-style
      (format "QT_QUICK_CONTROLS_STYLE=%s" qml-integration-qt-quick-controls-style)
    ""))


(defun qml-integration-run-qmlscene ()
  "Ask the user to choose a QML file in the project and run it with qmlscene."
  (interactive)
  (if qml-integration-qml-root-folder
      (compile (format
                "cd %s && %s qmlscene %s %s %s"
                qml-integration-qml-root-folder
                (qml-integration--get-style-string)
                (qml-integration--get-qmlscene-import-directories-string)
                qml-integration-qmlscene-extra-args
                (completing-read "Choose file: " (qml-integration-get-qml-files) nil t)))
    (compile (format
              "%s qmlscene %s %s %s"
              (qml-integration--get-style-string)
              (qml-integration--get-qmlscene-import-directories-string)
              qml-integration-qmlscene-extra-args
              (completing-read "Choose file: " (qml-integration-get-qml-files) nil t))))
  
  )


(defun qml-integration-run-qmltestrunner ()
  "Ask the user to choose a QML file in the project and run it with qmlscene."
  (interactive)
  (if current-prefix-arg
      (compile (format
                "cd %s && %s qmltestrunner %s %s"
                qml-integration-qml-root-folder
                (qml-integration--get-style-string)
                (qml-integration--get-qmltestrunner-import-directories-string)
                qml-integration-qmltestrunner-extra-args))
    (compile (format
              "cd %s && %s qmltestrunner %s -input %s %s"
              qml-integration-qml-root-folder
              (qml-integration--get-style-string)
              (qml-integration--get-qmltestrunner-import-directories-string)
              (completing-read "Choose test file: " (qml-integration-get-qml-test-files) nil t)
              qml-integration-qmltestrunner-extra-args))))


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(provide 'qml-integration)

;;; qml-integration.el ends here
