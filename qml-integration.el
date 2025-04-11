;;; qml-integration.el --- This is the short description  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2025  Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darcamo@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") s)
;; Homepage: https://github.com/darcamo/qml-integration
;; Keywords: QML language tools
;; URL: https://github.com/darcamo/qml-integration

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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


(defgroup qml-integration nil "Easily run qmlscene and qmltestrunner with QML files in your project." :group 'tools :prefix "qml-integration-")

(defcustom qml-integration-qt-install-bin-folder nil "The bin folder inside Qt installation directory." :type '(string))

(defcustom qml-integration-qmlscene-program "qmlscene" "The qmlscene path.
It can be simply \"qmlscene\" if it is in the system path." :type '(string))

(defcustom qml-integration-qmltestrunner-program "qmltestrunner" "The qmltestrunner path.
It can be simply \"qmltestrunner\" if it is in the system path." :type '(string))

(defcustom qml-integration-qmllint-program "qmllint" "The qmllint path.
It can be simply \"qmllint\" if it is in the system path." :type '(string))

(defcustom qml-integration-qml-root-folder nil "Root folder from where to run qmlscene." :type '(directory) :safe #'stringp)

(defcustom qml-integration-import-directories '(".")
  "Directories to add to include when running qmlscene or qmltestrunner.

Add here any folders with custom components in your project."
  :type '(repeat string) :safe #'listp)

(defcustom qml-integration-qt-quick-controls-style nil "Style to use with qmlscene." :type '(string) :safe #'stringp)

(defcustom qml-integration-qmlscene-extra-args ""
  "Extra arguments passed to qmlscene.
Do not include import folders here. For that use the
`qml-integration-import-directories' variable instead."
  :type '(string) :safe #'stringp)

(defcustom qml-integration-qmllint-extra-args ""
  "Extra arguments passed to qmllint.
Do not include import folders here. For that use the
`qml-integration-import-directories' variable instead."
  :type '(string) :safe #'stringp)

(defcustom qml-integration-qmltestrunner-extra-args "-silent"
  "Extra arguments passed to qmltestrunner.
Do not include import folders here. For that use the
`qml-integration-import-directories' variable instead."
  :type '(string) :safe #'stringp)

(defconst qml-integration-system-styles '("Fusion" "material" "Universal" "Plasma") "Qt Quick system styles to choose from.")

(defcustom qml-integration-user-styles nil "A list with user user styles." :type '(repeat string) :safe #'listp)

(defcustom qml-integration-ignored-paths '("./build*")
  "List of ignored paths when using `find' program to find QML files.

Note: This is only used when the `fd' program is not available.

For each `directory' in `qml-integration-ignored-paths', the string
'-not -path <directory>' will be added to the find command." :type '(repeat string) :safe #'listp)

(defun qml-integration--get-qt-tool (tool)
  "Get the path of the Qt TOOL.

TOOL must be a symbol and one of `(qmlscene qmltestrunner qmllint)'. This
will just return the value in the corresponding
qml-integration-<tool>-program variable."
  (pcase tool
    ('qmlscene qml-integration-qmlscene-program)
    ('qmltestrunner qml-integration-qmltestrunner-program)
    ('qmllint qml-integration-qmllint-program)
    (_ (error "Unknown tool: %s" tool))))


(defun qml-integration--get-qt-tool-fullpath (tool)
  "Get the full path of the Qt TOOL."
  (let ((tool-path (qml-integration--get-qt-tool tool))
        (bin qml-integration-qt-install-bin-folder))

    ;; If the path in tool-path is absolute, or bin is nil, just
    ;; return the value in tool-path. Otherwise, return the path
    ;; concatenating the path in bin with the one in tool-path.
    (if (or (not bin) (file-name-absolute-p tool-path))
        tool-path
      (expand-file-name tool-path bin))))


(defun qml-integration--get-import-directories-string (tool)
  "Get a string with import statements to pass to the TOOL."
  ;; qmltestrunner uses "-import", while qmlscene and qmllint use "-I"
  (let ((flag (if (eq tool 'qmltestrunner) "-import" "-I")))
    (s-join " " (mapcar (lambda (arg) (concat flag " " arg)) qml-integration-import-directories))))


(defun qml-integration--get-extra-args (tool)
  "Get a string with the extra args to pass to the TOOL."
  (pcase tool
    ('qmlscene qml-integration-qmlscene-extra-args)
    ('qmllint qml-integration-qmllint-extra-args)
    ('qmltestrunner qml-integration-qmltestrunner-extra-args)
    (_ (error "Unknown tool: %s" tool))))


(defun qml-integration--get-fd-command-string (pattern)
  "Get the command to find the QML files matching `PATTERN'.

This uses the `fd' program."
  (format "fd -t f %s %s" pattern (project-root (project-current))))


(defun qml-integration--get-find-command-string (qmlquery)
  "Get the command to find the QML files matching `QMLQUERY'.

This uses the `find'  program."
  (cond
   ((eq (length qml-integration-ignored-paths) 0)
    (format "find %s -type f -iname \"%s\""
            (project-root (project-current))
            qmlquery))

   (t
    (format "find %s %s -type f -iname \"%s\""
            (project-root (project-current))
            (s-join " " (mapcar #'(lambda (elem) (format "-not -path \"%s\"" elem) ) qml-integration-ignored-paths))
            qmlquery))))


(defun qml-integration--get-files-using-fd (pattern)
  "Get a list with all files in the project matching PATTERN.

The `fd' program is used to find the files."
  (split-string (shell-command-to-string (qml-integration--get-fd-command-string pattern))))


(defun qml-integration--get-files-using-find (pattern)
  "Get a list with all files in the project matching PATTERN.

The `find' program is used to find the files."
  (split-string (shell-command-to-string (qml-integration--get-find-command-string pattern))))


(defun qml-integration-get-qml-files ()
  "Get a list with all QML files in the project."
  (if (executable-find "fd")
      (qml-integration--get-files-using-fd ".qml$")
    (qml-integration--get-files-using-find "*qml")))


(defun qml-integration-get-qml-test-files ()
  "Get a list with all QML files in the project."
  (if (executable-find "fd")
      (qml-integration--get-files-using-fd "\"tst_.*.qml$\"")
    (qml-integration--get-files-using-find "tst_*.qml")))


(defun qml-integration--get-styles ()
  "Get the available styles."
  (append qml-integration-user-styles qml-integration-system-styles))


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


(defun qml-integration--ask-for-a-qml-file ()
  "Ask the user to choose a QML file in the project."
  (completing-read "Choose file: " (qml-integration-get-qml-files) nil t))


(defun qml-integration--get-qmlscene-run-command (&optional qml-file)
  "Get the command to run QML-FILE with qmlscene.

If QML-FILE is not provided, ask the user to choose one in the project."
  (let* ((root-folder qml-integration-qml-root-folder)
         (program (qml-integration--get-qt-tool-fullpath 'qmlscene))
         (style (qml-integration--get-style-string))
         (import-dir-args (qml-integration--get-import-directories-string 'qmlscene))
         (extra-args (qml-integration--get-extra-args 'qmlscene))
         (qml-file (or qml-file (qml-integration--ask-for-a-qml-file)))
         (run-command (string-join (list style program import-dir-args extra-args qml-file) " ")))
    (if root-folder
        (format "cd %s && %s" root-folder run-command)
      run-command)))


(defun qml-integration-run-qmlscene ()
  "Ask the user to choose a QML file in the project and run it with qmlscene."
  (interactive)
  (compile (qml-integration--get-qmlscene-run-command)))


(defun qml-integration--get-qmllint-run-command (&optional qml-file)
  "Get the command to run qmllint on the QML-FILE.

If QML-FILE is not provided, ask the user to choose one in the project."
  (let*((root-folder qml-integration-qml-root-folder)
        (program (qml-integration--get-qt-tool-fullpath 'qmllint))
        (style (qml-integration--get-style-string))
        (import-dir-args (qml-integration--get-import-directories-string 'qmllint))
        (extra-args (qml-integration--get-extra-args 'qmllint))
        (qml-file (or qml-file (qml-integration--ask-for-a-qml-file)))
        (run-command (string-join (list style program import-dir-args extra-args qml-file) " ")))
    (if root-folder
        (format "cd %s && %s" root-folder run-command)
      run-command)))


(defun qml-integration-run-qmllint ()
  "Ask the user to choose a QML file in the project and run it with qmllint."
  (interactive)
  (compile (qml-integration--get-qmllint-run-command)))


(defun qml-integration--get-qmltestrunner-run-command (&optional qml-file)
  "Get the command to run qmltestrunner on the QML-FILE.

If QML-FILE is not provided, then return the command to run
qmltestrunner without specifying an input file, which will then run on
all files in the project."
  (let* ((root-folder qml-integration-qml-root-folder)
         (program (qml-integration--get-qt-tool-fullpath 'qmltestrunner))
         (style (qml-integration--get-style-string))
         (import-dir-args (qml-integration--get-import-directories-string 'qmltestrunner))
         (extra-args (qml-integration--get-extra-args 'qmltestrunner))
         (run-command (string-join (list style program import-dir-args extra-args) " "))
         (extended-run-command (if qml-file (concat run-command " -input " qml-file) run-command)))
    (if root-folder
        (format "cd %s && %s" root-folder extended-run-command)
      extended-run-command)))


(defun qml-integration-run-qmltestrunner ()
  "Ask the user to choose a QML file in the project and run it with qmlscene."
  (interactive)
  (if current-prefix-arg
      (compile (qml-integration--get-qmltestrunner-run-command))

    (let ((chosen-file (completing-read "Choose file: " (qml-integration-get-qml-test-files) nil t)))
      (compile (qml-integration--get-qmltestrunner-run-command chosen-file)))))


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(provide 'qml-integration)

;;; qml-integration.el ends here
