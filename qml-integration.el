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

;; Provide functions to easily run the different QML tools with any of the QML
;; files in a project. The supported tools are: qmlscene (to view QML files),
;; qmllint and qmltestrunner.

;;; Code:
(require 'project)
(require 's)


(defgroup qml-integration nil
  "Easily run qmlscene and qmltestrunner with QML files in your project."
  :group 'tools
  :prefix "qml-integration-")

(defcustom qi-qt-install-bin-folder nil
  "The bin folder inside Qt installation directory.

All Qt tools (qmlscene, qmltestrunner, qmllint) are expected to be in
this folder."
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

(defcustom qi-working-directory nil
  "Folder from where to run the tools."
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


(defvar qi--last-tool-output-buffer-name nil
  "Name of the buffer with output from the last tool run.")


(defun qi--get-working-directory (&optional file)
  "Get the working directory to run the tools to process FILE."
  (if qi-working-directory
      qi-working-directory
    (if-let* ((proj (project-current nil file)))
      (project-root proj)
      default-directory)))


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

  (let ((default-directory (qi--get-working-directory))
        (flag
         (if (eq tool 'qmltestrunner)
             "-import"
           "-I"))
        (import-dir-args qi-import-directories))
    (mapcan
     (lambda (dir)
       (list
        flag
        (concat
         "./"
         (file-relative-name (expand-file-name dir)
                             (qi--get-working-directory)))))
     import-dir-args)))


(defun qi--get-import-directories-string (tool)
  "Get a string with import statements to pass to the TOOL."
  (s-join " " (qi--get-import-directories-as-args tool)))


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
         (cwd (qi--get-working-directory))
         (style-string
          (if qi-qt-quick-controls-style
              (format "QT_QUICK_CONTROLS_STYLE=%s" qi-qt-quick-controls-style)
            ""))
         (process-environment
          (if qi-qt-quick-controls-style
              (cons style-string process-environment)
            process-environment))
         (program (qi--get-qt-tool-fullpath tool))
         (process-args (qi--get-tool-program-args tool qml-file))
         (process-args-string (s-join " " process-args)))

    ;; Create buffer with name in buffer-name, if it does not exist.
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)))

    (with-current-buffer buffer-name
      (setq qi--last-tool-output-buffer-name buffer-name)
      (read-only-mode 0)
      (delete-region (point-min) (point-max))

      ;; We need to set default-directory again because we are now in a
      ;; different buffer.
      (let ((default-directory cwd))
        (insert
         (format "\n%s %s\n%s %s\n%s %s\n"
                 (propertize "Working Directory:" 'face 'font-lock-keyword-face)
                 (propertize default-directory 'face 'font-lock-string-face)
                 (propertize "Tool:" 'face 'font-lock-keyword-face)
                 (propertize program 'face 'font-lock-constant-face)
                 (propertize "File:" 'face 'font-lock-keyword-face)
                 (propertize qml-file 'face 'font-lock-string-face)))
        (insert
         (format "%s \"%s\"\n\n"
                 (propertize "Arguments:" 'face 'font-lock-keyword-face)
                 (propertize process-args-string 'face 'font-lock-string-face)))
        (insert
         (format "%s\n    %s\n\n"
                 (propertize "Equivalent shell command:"
                             'face
                             'font-lock-keyword-face)
                 (propertize (format "cd %s && %s %s %s"
                                     default-directory style-string
                                     program ;;
                                     process-args-string)
                             'face 'font-lock-constant-face)))

        (insert
         (propertize
          "----------------------------------------------------------------------\n"
          'face 'font-lock-comment-face))
        (apply 'start-process process-name buffer-name program process-args))

      (unless (eq tool 'qmlscene)
        (read-only-mode 1)
        (display-buffer (buffer-name))))))


(defun qi-view-last-tool-output ()
  "Display the buffer with output from last tool run."
  (interactive)
  (if qi--last-tool-output-buffer-name
      (display-buffer qi--last-tool-output-buffer-name)
    (error "No tool has run previously")))


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


(defconst qi-qmllint-warnings
  '(""
    "missing-property"
    "unqualified"
    "import"
    "deprecated"
    "readonly"
    "unresolved-alias")
  "List of qmllint warnings that we can disable with a comment.")


(defvar qi--last-qmllint-disabled-warning nil
  "Warning ID chosen last time.")


(defun qi--add-qmllint-comment-line (&optional enable warning-id)
  "Add a qmllint comment to enable/disable warnings on the current line.

If ENABLE is non-nil, then enable warnings. Otherwise disable them.

If WARNING-ID is provided, then use it as the warning id to
enable/disable. Otherwise, prompt the user to choose a warning id from a
known list."
  (let* ((initial-input
          (if enable
              qi--last-qmllint-disabled-warning
            nil))
         (warning-id
          (or warning-id
              (completing-read "Disable warning (leave empty to disable all): "
                               qi-qmllint-warnings
                               nil
                               t
                               initial-input
                               nil
                               (car qi-qmllint-warnings))))
         (type-string
          (if enable
              "enable"
            "disable"))
         (comment
          (if (string= warning-id "")
              (format "// qmllint %s" type-string)
            (format "// qmllint %s %s" type-string warning-id))))
    (save-excursion
      (let ((line-end (line-end-position)))
        (beginning-of-line)
        (if (re-search-forward "//\\s-*qmllint disable.*" line-end t)
            (replace-match comment t t)
          (goto-char line-end)
          (unless (or (= (point) (line-beginning-position))
                      (memq (char-before) '(?\s ?\t)))
            (insert " "))
          (insert comment))))

    (unless enable
      (setq qi--last-qmllint-disabled-warning warning-id))

    (message "Added comment: %s" comment)))


(defun qi-add-qmllint-disable-comment ()
  "Add a comment to disable qmllint warning on the current line or region.

When a region is active, wrap it by inserting a disabling comment before
the first line and an enabling comment after the last line."
  (interactive)
  (if (use-region-p)
      (let ((start (copy-marker (region-beginning)))
            (end (copy-marker (region-end)))
            (warning-id
             (completing-read "Disable warning (leave empty to disable all): "
                              qi-qmllint-warnings
                              nil
                              t)))
        (progn
          (save-excursion
            (goto-char start)
            (beginning-of-line)
            (newline)
            (forward-line -1)
            (qi--add-qmllint-comment-line nil warning-id)
            (comment-indent))
          (save-excursion
            (goto-char end)
            (when (and (> (marker-position end) (marker-position start))
                       (= (point) (line-beginning-position)))
              (forward-line -1))
            (end-of-line)
            (newline)
            (qi--add-qmllint-comment-line t warning-id)
            (comment-indent)))
        (set-marker start nil)
        (set-marker end nil))
    (qi--add-qmllint-comment-line)))


(defun qi-add-qmllint-enable-comment ()
  "Add a comment to enable qmllint warning on the current line.

The comment is \"qmllint enable <warning-id>\" where <warning-id> is
one of the possible checks. The user is prompted to choose the warning
id from a know list. An empty warning id enables all checks for the
line."
  (interactive)
  (qi--add-qmllint-comment-line t))

;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(provide 'qml-integration)

;;; qml-integration.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("qi-" . "qml-integration-"))
;; End:
