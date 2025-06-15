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


(defgroup qml-integration nil
  "Easily run qmlscene and qmltestrunner with QML files in your project."
  :group 'tools
  :prefix "qml-integration-")

(defcustom qi-qt-install-bin-folder nil
  "The bin folder inside Qt installation directory."
  :type '(string))

(defcustom qi-qmlscene-program "qmlscene"
  "The qmlscene path.
It can be simply \"qmlscene\" if it is in the system path."
  :type '(string))

(defcustom qi-qmltestrunner-program "qmltestrunner"
  "The qmltestrunner path.
It can be simply \"qmltestrunner\" if it is in the system path."
  :type '(string))

(defcustom qi-qmllint-program "qmllint"
  "The qmllint path.
It can be simply \"qmllint\" if it is in the system path."
  :type '(string))

(defcustom qi-qml-root-folder nil
  "Root folder from where to run qmlscene."
  :type '(directory)
  :safe #'stringp)

(defcustom qi-import-directories '(".")
  "Directories to add to include when running qmlscene or qmltestrunner.

Add here any folders with custom components in your project."
  :type '(repeat string)
  :safe #'listp)

(defcustom qi-qt-quick-controls-style nil
  "Style to use with qmlscene."
  :type '(string)
  :safe #'stringp)

(defcustom qi-qmlscene-extra-args nil
  "Extra arguments passed to qmlscene as a list of strings.
Each string should represent one argument. Do not include import folders
here. For that use the `qml-integration-import-directories' variable
instead."
  :type '(repeat string)
  :safe (lambda (x) (and (listp x) (seq-every-p #'stringp x))))

(defcustom qi-qmllint-extra-args nil
  "Extra arguments passed to qmllint as a list of strings.
Each string should represent one argument. Do not include import folders
here. For that use the `qml-integration-import-directories' variable
instead."
  :type '(repeat string)
  :safe (lambda (x) (and (listp x) (seq-every-p #'stringp x))))

(defcustom qi-qmltestrunner-extra-args '("-silent")
  "Extra arguments passed to qmltestrunner as a list of strings.
Each string should represent one argument. Do not include import
folders here. For that use the `qml-integration-import-directories'
variable instead."
  :type '(repeat string)
  :safe (lambda (x) (and (listp x) (seq-every-p #'stringp x))))

(defconst qi-system-styles '("Fusion" "material" "Universal" "Plasma")
  "Qt Quick system styles to choose from.")

(defcustom qi-user-styles nil
  "A list with user user styles."
  :type '(repeat string)
  :safe #'listp)

(defcustom qi-ignored-paths '("./build*")
  "List of ignored paths when using `find' program to find QML files.

Note: This is only used when the `fd' program is not available.

For each `directory' in `qml-integration-ignored-paths', the string
'-not -path <directory>' will be added to the find command."
  :type '(repeat string)
  :safe #'listp)

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


(defun qi--get-import-directories-as-args (tool)
  "Get the arguments specifying import directories to pass to the TOOL.

The list is either in the form `'(\"-I\" \"dir1\" \"-I\" \"dir2\")' or
`'(\"-import\" \"dir1\" \"-import\" \"dir2\")', depending on the tool.

The import directories are taken from the
`qml-integration-import-directories'."
  (let ((flag
         (if (eq tool 'qmltestrunner)
             "-import"
           "-I"))
        (import-dir-args qi-import-directories))
    (mapcan (lambda (dir) (list flag dir)) import-dir-args)))


(defun qi--get-import-directories-string (tool)
  "Get a string with import statements to pass to the TOOL."
  (s-join " " (qml-integration--get-import-directories-as-args tool)))


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
            (s-join
             " "
             (mapcar
              #'(lambda (elem) (format "-not -path \"%s\"" elem))
              qi-ignored-paths))
            qmlquery))))


(defun qi--get-files-using-fd (pattern)
  "Get a list with all files in the project matching PATTERN.

The `fd' program is used to find the files."
  (split-string (shell-command-to-string (qi--get-fd-command-string pattern))))


(defun qi--get-files-using-find (pattern)
  "Get a list with all files in the project matching PATTERN.

The `find' program is used to find the files."
  (split-string
   (shell-command-to-string (qi--get-find-command-string pattern))))


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
        (completing-read "Choose style: " (qi--get-styles))))


(defun qi--get-style-string ()
  "Get the string that must come before qmlscene and qmltestrunner commands."
  (if qi-qt-quick-controls-style
      (format "QT_QUICK_CONTROLS_STYLE=%s" qi-qt-quick-controls-style)
    ""))


(defun qi--ask-for-a-qml-file ()
  "Ask the user to choose a QML file in the project."
  (completing-read "Choose file: " (qi-get-qml-files) nil t))


(defun qi--get-qml-file-as-args (tool &optional qml-file)
  "Get a list with the args to pass TOOL for running with QML-FILE."
  (if (eq tool 'qmltestrunner)
      (when qml-file
        (list "-input" qml-file))
    (unless qml-file
      (error "QML file must be provided"))
    (list qml-file)))


(defun qi--get-tool-program-args (tool &optional qml-file)
  "Get the arguments to run TOOL with the QML-FILE.

QML-FILE is only optional for the `'qmltestrunner' tool. For the other
tools it must be provided."
  (let*
      ((import-dir-args (qi--get-import-directories-as-args tool))
       (extra-args (qi--get-extra-args tool))
       ;; (qml-file (or qml-file (qi--ask-for-a-qml-file)))
       (qml-file-args (qi--get-qml-file-as-args tool qml-file))
       ;; (program-args-list (append import-dir-args extra-args (list qml-file)))
       )
    ;; program-args-list
    (append import-dir-args extra-args qml-file-args)))


(defun qi--get-process-name (tool qml-file)
  "Get the name to use for the process for running TOOL on QML-FILE.

If QML-FILE is not provided, then ask the user to choose one in the
project."
  (if qml-file
      (let* ((name
              (file-name-nondirectory (file-name-sans-extension qml-file))))
        (pcase tool
          ('qmlscene (concat "View-" name))
          ('qmllint (concat "Lint-" name))
          ('qmltestrunner (concat "Test-" name))
          (_ (error "Unknown tool: %s" tool))))

    ;; We allow qml-file not to be provided only for the `qmltestrunner' tool.
    ;; In that case all tests are run (and not just one file).
    (if (equal tool 'qmltestrunner)
        "Test-all"
      (error "QML file must be provided for tool '%s'" tool))))


(defun qi--get-buffer-name (tool qml-file)
  "Get the buffer name to use for running TOOL on QML-FILE.

If QML-FILE is not provided, then ask the user to choose one in the
project."
  (if qml-file
      (let* ((name
              (file-name-nondirectory (file-name-sans-extension qml-file))))
        (pcase tool
          ('qmlscene (format "Viewing qml file '%s'" name))
          ('qmllint (format "Linting qml file '%s'" name))
          ('qmltestrunner (format "Running test file '%s'" name))
          (_ (error "Unknown tool: %s" tool))))

    ;; We allow qml-file not to be provided only for the `qmltestrunner' tool.
    (if (equal tool 'qmltestrunner)
        "Running all tests"
      (error "QML file must be provided for tool '%s'" tool))))


(defun qi--run-tool (tool &optional qml-file)
  "Run TOOL with the QML-FILE.

If QML-FILE is not provided, then ask the user to choose one in the
project, unless `current-prefix-arg' was passed."
  (let* ((qml-file
          (if current-prefix-arg
              qml-file
            (or qml-file (qi--ask-for-a-qml-file))))
         (process-name (qi--get-process-name tool qml-file))
         (buffer-name (qi--get-buffer-name tool qml-file))
         (root-folder qi-qml-root-folder)
         (default-directory (or root-folder default-directory))
         (process-environment
          (if qi-qt-quick-controls-style
              (cons
               (format "QT_QUICK_CONTROLS_STYLE=%s"
                       qi-qt-quick-controls-style)
               process-environment)
            process-environment))
         (program (qi--get-qt-tool-fullpath tool))
         (process-args (qi--get-tool-program-args tool qml-file)))

    ;; Create buffer with name in buffer-name, if it does not exist.
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (setq-local default-directory default-directory)))

    (with-current-buffer buffer-name
      (goto-char (point-max))
      (insert
       (format "\nRunning '%s' on file '%s'\n"
               (qi--get-qt-tool-fullpath tool)
               qml-file))
      (insert (format "Arguments: \"%s\"\n" (s-join " " process-args)))

      (apply 'start-process process-name buffer-name program process-args)
      ;; (compilation-mode)

      (pop-to-buffer (buffer-name))
      ;; (display-buffer (buffer-name))
      )))


(defun qi-run-qmlscene (&optional qml-file)
  "View QML-FILE using the qmlscene program.

If QML-FILE is not provided, then ask the user to choose one in the project."
  (interactive)
  (qi--run-tool 'qmlscene qml-file))


(defun qi-run-qmllint (&optional qml-file)
  "Run linting on QML-FILE.

If QML-FILE is not provided, then ask the user to choose one in the project."
  (interactive)
  (qi--run-tool 'qmllint qml-file))


(defun qi-run-qmltestrunner ()
  "Ask the user to choose a QML file in the project and run it with qmlscene."
  (interactive)
  (if current-prefix-arg
      (qi--run-tool 'qmltestrunner)

    (let ((chosen-file
           (completing-read "Choose file: " (qi-get-qml-test-files) nil t)))
      (qi--run-tool 'qmltestrunner chosen-file))))


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(provide 'qml-integration)

;;; qml-integration.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("qi-" . "qml-integration-"))
;; End:
