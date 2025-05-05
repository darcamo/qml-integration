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

(defcustom qi-qt-install-bin-folder nil "The bin folder inside Qt installation directory." :type '(string))

(defcustom qi-qmlscene-program "qmlscene" "The qmlscene path.
It can be simply \"qmlscene\" if it is in the system path." :type '(string))

(defcustom qi-qmltestrunner-program "qmltestrunner" "The qmltestrunner path.
It can be simply \"qmltestrunner\" if it is in the system path." :type '(string))

(defcustom qi-qmllint-program "qmllint" "The qmllint path.
It can be simply \"qmllint\" if it is in the system path." :type '(string))

(defcustom qi-qml-root-folder nil "Root folder from where to run qmlscene." :type '(directory) :safe #'stringp)

(defcustom qi-import-directories '(".")
  "Directories to add to include when running qmlscene or qmltestrunner.

Add here any folders with custom components in your project."
  :type '(repeat string) :safe #'listp)

(defcustom qi-qt-quick-controls-style nil "Style to use with qmlscene." :type '(string) :safe #'stringp)

(defcustom qi-qmlscene-extra-args ""
  "Extra arguments passed to qmlscene.
Do not include import folders here. For that use the
`qml-integration-import-directories' variable instead."
  :type '(string) :safe #'stringp)

(defcustom qi-qmllint-extra-args ""
  "Extra arguments passed to qmllint.
Do not include import folders here. For that use the
`qml-integration-import-directories' variable instead."
  :type '(string) :safe #'stringp)

(defcustom qi-qmltestrunner-extra-args "-silent"
  "Extra arguments passed to qmltestrunner.
Do not include import folders here. For that use the
`qml-integration-import-directories' variable instead."
  :type '(string) :safe #'stringp)

(defconst qi-system-styles '("Fusion" "material" "Universal" "Plasma") "Qt Quick system styles to choose from.")

(defcustom qi-user-styles nil "A list with user user styles." :type '(repeat string) :safe #'listp)

(defcustom qi-ignored-paths '("./build*")
  "List of ignored paths when using `find' program to find QML files.

Note: This is only used when the `fd' program is not available.

For each `directory' in `qml-integration-ignored-paths', the string
'-not -path <directory>' will be added to the find command." :type '(repeat string) :safe #'listp)

(defun qi--get-qt-tool (tool)
  "Get the path of the Qt TOOL.

TOOL must be a symbol and one of `(qmlscene qmltestrunner qmllint)'. This
will just return the value in the corresponding
qml-integration-<tool>-program variable."
  (pcase tool
    ('qmlscene qi-qmlscene-program)
    ('qmltestrunner qi-qmltestrunner-program)
    ('qmllint qi-qmllint-program)
    (_ (error "Unknown tool: %s" tool))))


(defun qi--get-qt-tool-fullpath (tool)
  "Get the full path of the Qt TOOL."
  (let ((tool-path (qi--get-qt-tool tool))
        (bin qi-qt-install-bin-folder))

    ;; If the path in tool-path is absolute, or bin is nil, just
    ;; return the value in tool-path. Otherwise, return the path
    ;; concatenating the path in bin with the one in tool-path.
    (if (or (not bin) (file-name-absolute-p tool-path))
        tool-path
      (expand-file-name tool-path bin))))


(defun qi--get-import-directories-string (tool)
  "Get a string with import statements to pass to the TOOL."
  ;; qmltestrunner uses "-import", while qmlscene and qmllint use "-I"
  (let ((flag (if (eq tool 'qmltestrunner) "-import" "-I")))
    (s-join " " (mapcar (lambda (arg) (concat flag " " arg)) qi-import-directories))))


(defun qi--get-extra-args (tool)
  "Get a string with the extra args to pass to the TOOL."
  (pcase tool
    ('qmlscene qi-qmlscene-extra-args)
    ('qmllint qi-qmllint-extra-args)
    ('qmltestrunner qi-qmltestrunner-extra-args)
    (_ (error "Unknown tool: %s" tool))))


(defun qi--get-fd-command-string (pattern)
  "Get the command to find the QML files matching `PATTERN'.

This uses the `fd' program."
  (format "fd -t f %s %s" pattern (project-root (project-current))))


(defun qi--get-find-command-string (qmlquery)
  "Get the command to find the QML files matching `QMLQUERY'.

This uses the `find'  program."
  (cond
   ((eq (length qi-ignored-paths) 0)
    (format "find %s -type f -iname \"%s\""
            (project-root (project-current))
            qmlquery))

   (t
    (format "find %s %s -type f -iname \"%s\""
            (project-root (project-current))
            (s-join " " (mapcar #'(lambda (elem) (format "-not -path \"%s\"" elem) ) qi-ignored-paths))
            qmlquery))))


(defun qi--get-files-using-fd (pattern)
  "Get a list with all files in the project matching PATTERN.

The `fd' program is used to find the files."
  (split-string (shell-command-to-string (qi--get-fd-command-string pattern))))


(defun qi--get-files-using-find (pattern)
  "Get a list with all files in the project matching PATTERN.

The `find' program is used to find the files."
  (split-string (shell-command-to-string (qi--get-find-command-string pattern))))


(defun qi-get-qml-files ()
  "Get a list with all QML files in the project."
  (if (executable-find "fd")
      (qi--get-files-using-fd ".qml$")
    (qi--get-files-using-find "*qml")))


(defun qi-get-qml-test-files ()
  "Get a list with all QML files in the project."
  (if (executable-find "fd")
      (qi--get-files-using-fd "\"tst_.*.qml$\"")
    (qi--get-files-using-find "tst_*.qml")))


(defun qi--get-styles ()
  "Get the available styles."
  (append qi-user-styles qi-system-styles))


(defun qi-choose-qml-style ()
  "Choose a style for QML controls."
  (interactive)
  (setq qi-qt-quick-controls-style
        (completing-read "Choose style: "
                         (qi--get-styles))))


(defun qi--get-style-string ()
  "Get the string that must come before qmlscene and qmltestrunner commands."
  (if qi-qt-quick-controls-style
      (format "QT_QUICK_CONTROLS_STYLE=%s" qi-qt-quick-controls-style)
    ""))


(defun qi--ask-for-a-qml-file ()
  "Ask the user to choose a QML file in the project."
  (completing-read "Choose file: " (qi-get-qml-files) nil t))


(defun qi--get-qmlscene-run-command (&optional qml-file)
  "Get the command to run QML-FILE with qmlscene.

If QML-FILE is not provided, ask the user to choose one in the project."
  (let* ((root-folder qi-qml-root-folder)
         (program (qi--get-qt-tool-fullpath 'qmlscene))
         (style (qi--get-style-string))
         (import-dir-args (qi--get-import-directories-string 'qmlscene))
         (extra-args (qi--get-extra-args 'qmlscene))
         (qml-file (or qml-file (qi--ask-for-a-qml-file)))
         (run-command (string-join (list style program import-dir-args extra-args qml-file) " ")))
    (if root-folder
        (format "cd %s && %s" root-folder run-command)
      run-command)))


(defun qi-run-qmlscene ()
  "Ask the user to choose a QML file in the project and run it with qmlscene."
  (interactive)
  (compile (qi--get-qmlscene-run-command)))


(defun qi--get-qmllint-run-command (&optional qml-file)
  "Get the command to run qmllint on the QML-FILE.

If QML-FILE is not provided, ask the user to choose one in the project."
  (let*((root-folder qi-qml-root-folder)
        (program (qi--get-qt-tool-fullpath 'qmllint))
        (style (qi--get-style-string))
        (import-dir-args (qi--get-import-directories-string 'qmllint))
        (extra-args (qi--get-extra-args 'qmllint))
        (qml-file (or qml-file (qi--ask-for-a-qml-file)))
        (run-command (string-join (list style program import-dir-args extra-args qml-file) " ")))
    (if root-folder
        (format "cd %s && %s" root-folder run-command)
      run-command)))


(defun qi-run-qmllint ()
  "Ask the user to choose a QML file in the project and run it with qmllint."
  (interactive)
  (compile (qi--get-qmllint-run-command)))


(defun qi--get-qmltestrunner-run-command (&optional qml-file)
  "Get the command to run qmltestrunner on the QML-FILE.

If QML-FILE is not provided, then return the command to run
qmltestrunner without specifying an input file, which will then run on
all files in the project."
  (let* ((root-folder qi-qml-root-folder)
         (program (qi--get-qt-tool-fullpath 'qmltestrunner))
         (style (qi--get-style-string))
         (import-dir-args (qi--get-import-directories-string 'qmltestrunner))
         (extra-args (qi--get-extra-args 'qmltestrunner))
         (run-command (string-join (list style program import-dir-args extra-args) " "))
         (extended-run-command (if qml-file (concat run-command " -input " qml-file) run-command)))
    (if root-folder
        (format "cd %s && %s" root-folder extended-run-command)
      extended-run-command)))


(defun qi-run-qmltestrunner ()
  "Ask the user to choose a QML file in the project and run it with qmlscene."
  (interactive)
  (if current-prefix-arg
      (compile (qi--get-qmltestrunner-run-command))

    (let ((chosen-file (completing-read "Choose file: " (qi-get-qml-test-files) nil t)))
      (compile (qi--get-qmltestrunner-run-command chosen-file)))))


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(provide 'qml-integration)

;;; qml-integration.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("qi-" . "qml-integration-"))
;; End:
