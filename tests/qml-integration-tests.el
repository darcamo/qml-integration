;;; tests.el --- Tests for the qml-integration library  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darlan@darlan-notebook>
;; Keywords: keywords

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
;;
;; Tests for the qml-integration package.

;;; Code:
(require 'ert)
(require 'qml-integration)


(defun test-fixture-setup (subfolder body)
  "Run BODY with 'default-directory' set to SUBFOLDER.

This is used in the tests definitions to make sure we run the
test code from inside a 'test project'."
  (unwind-protect
      (let ((default-directory (expand-file-name subfolder)))
        (funcall body))))


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxx Define the tests xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(ert-deftest test-qml-integration--get-qt-tool-fullpath ()
  (let* ((qml-integration-qt-install-bin-folder "/some/path/Qt/bin")
         (qml-integration-qmlscene-program "qmlscene6")
         (qml-integration-qmllint-program "qmllint6")
         (qml-integration-qmltestrunner-program "qmltestrunner6")
         (expected-qmlscene-path
          (file-name-concat qml-integration-qt-install-bin-folder
                            qml-integration-qmlscene-program))
         (expected-qmllint-path
          (file-name-concat qml-integration-qt-install-bin-folder
                            qml-integration-qmllint-program))
         (expected-qmltestrunner-path
          (file-name-concat qml-integration-qt-install-bin-folder
                            qml-integration-qmltestrunner-program)))

    (should
     (equal
      (qml-integration--get-qt-tool-fullpath 'qmlscene) expected-qmlscene-path))
    (should
     (equal
      (qml-integration--get-qt-tool-fullpath 'qmllint) expected-qmllint-path))
    (should
     (equal
      (qml-integration--get-qt-tool-fullpath 'qmltestrunner)
      expected-qmltestrunner-path))))


(ert-deftest test-qml-integration--get-import-directories-string ()
  ;; This is a parametric test. The `all-tests-data` variable is a
  ;; list, where each element is a list of parameters corresponding to
  ;; a single test. The first parameter is the tool, followed by the
  ;; list of import directories and then the expected string from
  ;; applying `qml-integration--get-import-directories-string`.
  (let ((all-tests-data
         '((qmlscene () "")
           (qmlscene (".") "-I .")
           (qmlscene ("." "imports") "-I . -I imports")
           (qmltestrunner () "")
           (qmltestrunner (".") "-import .")
           (qmltestrunner ("." "imports") "-import . -import imports")
           (qmllint () "")
           (qmllint (".") "-I .")
           (qmllint ("." "imports") "-I . -I imports"))))

    ;; Decompose test-data for a test into the `tool`,
    ;; `qml-integration-import-directories` and `expected-string`
    ;; variables
    (dolist (test-data all-tests-data)
      (pcase-let* ((`(,tool
                      ,qml-integration-import-directories ,expected-string)
                    test-data))
        (should
         (equal
          (qml-integration--get-import-directories-string tool)
          expected-string))))))


(ert-deftest qml-integration--get-fd-command-string ()
  (should
   (equal
    (qml-integration--get-fd-command-string "query")
    (format "fd -t f %s %s" "query" (project-root (project-current))))))


(ert-deftest test-qml-integration--get-find-command-string ()
  (let ((qml-integration-ignored-paths nil))
    (should
     (equal
      (qml-integration--get-find-command-string "query")
      (format "find %s -type f -iname \"query\""
              (project-root (project-current))))))

  (let ((qml-integration-ignored-paths '("path1")))
    (should
     (equal
      (qml-integration--get-find-command-string "query")
      (format "find %s -not -path \"path1\" -type f -iname \"query\""
              (project-root (project-current))))))

  (let ((qml-integration-ignored-paths '("path1" "path2" "path3")))
    (should
     (equal
      (qml-integration--get-find-command-string "query")
      (format
       "find %s -not -path \"path1\" -not -path \"path2\" -not -path \"path3\" -type f -iname \"query\""
       (project-root (project-current)))))))


(ert-deftest test-qml-integration--get-files-using-find ()
  ;; Test getting regular qml files
  (test-fixture-setup
   "test-empty-project"
   (lambda ()
     (let ((expected-files nil))
       (should
        (seq-set-equal-p
         (qml-integration--get-files-using-find "*qml") expected-files)))))

  (let ((qml-integration-ignored-paths nil))
    (test-fixture-setup
     "test-project"
     (lambda ()
       (let ((expected-files
              (mapcar
               'expand-file-name
               '("path1/path1-file2.qml"
                 "path1/path1-file1.qml"
                 "path2/path2-file2.qml"
                 "path2/path2-file1.qml"
                 "ignore-path1/ignore-path1-file1.qml"
                 "ignore-path2/ignore-path2-file1.qml"
                 "tests/tst_test1.qml"
                 "tests/tst_test2.qml"
                 "tests/tst_test3.qml"))))
         (should
          (seq-set-equal-p
           (qml-integration--get-files-using-find "*qml") expected-files))))))

  (let ((qml-integration-ignored-paths
         '("*/ignore-path1/*" "*/ignore-path2/*")))
    (test-fixture-setup
     "test-project"
     (lambda ()
       (let ((expected-files
              (mapcar
               'expand-file-name
               '("path1/path1-file2.qml"
                 "path1/path1-file1.qml"
                 "path2/path2-file2.qml"
                 "path2/path2-file1.qml"
                 "tests/tst_test1.qml"
                 "tests/tst_test2.qml"
                 "tests/tst_test3.qml"))))
         (should
          (seq-set-equal-p
           (qml-integration--get-files-using-find "*qml") expected-files))))))

  ;; Test getting test files
  (test-fixture-setup
   "test-project"
   (lambda ()
     (let ((qml-integration-ignored-paths
            '("*/ignore-path1/*" "*/ignore-path2/*"))
           (expected-files
            (mapcar
             'expand-file-name
             '("tests/tst_test1.qml"
               "tests/tst_test2.qml"
               "tests/tst_test3.qml"))))
       (should
        (seq-set-equal-p
         (qml-integration--get-files-using-find
          "tst_*.qml")
         expected-files))))))


(ert-deftest test-qml-integration--get-files-using-fd ()
  ;; Test getting regular qml files
  (test-fixture-setup
   "test-empty-project"
   (lambda ()
     (let ((expected-files nil))
       (should
        (seq-set-equal-p
         (qml-integration--get-files-using-fd ".qml$") expected-files)))))

  (test-fixture-setup
   "test-project"
   (lambda ()
     (let ((expected-files
            (mapcar
             'expand-file-name
             '("path1/path1-file2.qml"
               "path1/path1-file1.qml"
               "path2/path2-file2.qml"
               "path2/path2-file1.qml"
               "ignore-path1/ignore-path1-file1.qml"
               "ignore-path2/ignore-path2-file1.qml"
               "tests/tst_test1.qml"
               "tests/tst_test2.qml"
               "tests/tst_test3.qml"))))
       (should
        (seq-set-equal-p
         (qml-integration--get-files-using-fd ".qml$") expected-files)))))

  ;; Test getting test files
  (test-fixture-setup
   "test-project"
   (lambda ()
     (let ((expected-files
            (mapcar
             'expand-file-name
             '("tests/tst_test1.qml"
               "tests/tst_test2.qml"
               "tests/tst_test3.qml"))))
       (should
        (seq-set-equal-p
         (qml-integration--get-files-using-fd
          "\"tst_.*.qml$\"")
         expected-files))))))


(ert-deftest test-qml-integration--get-styles ()
  (let ((qml-integration-user-styles nil))
    (should
     (equal
      (qml-integration--get-styles)
      '("Fusion" "material" "Universal" "Plasma"))))

  (let ((qml-integration-user-styles '("mystyle1" "mystyle2")))
    (should
     (equal
      (qml-integration--get-styles)
      '("mystyle1" "mystyle2" "Fusion" "material" "Universal" "Plasma")))))


(ert-deftest test-qml-integration--get-style-string ()
  (let ((qml-integration-qt-quick-controls-style nil))
    (should (equal (qml-integration--get-style-string) "")))

  (let ((qml-integration-qt-quick-controls-style "some-style"))
    (should
     (equal
      (qml-integration--get-style-string)
      "QT_QUICK_CONTROLS_STYLE=some-style"))))


(ert-deftest test-qml-integration--get-process-name ()
  (dolist (file-name '("MyQmlFile.qml" "SomeOtherQmlFile.qml"))
    ;; qmlscene process name
    (let ((tool 'qmlscene)
          (expected-process-name (format "View-%s" (file-name-base file-name))))
      (should
       (equal
        (qml-integration--get-process-name tool file-name)
        expected-process-name)))

    ;; qmllint process name
    (let ((tool 'qmllint)
          (expected-process-name (format "Lint-%s" (file-name-base file-name))))
      (should
       (equal
        (qml-integration--get-process-name tool file-name)
        expected-process-name)))

    ;; qmltest process name
    (let ((tool 'qmltestrunner)
          (expected-process-name (format "Test-%s" (file-name-base file-name))))
      (should
       (equal
        (qml-integration--get-process-name tool file-name)
        expected-process-name)))))


(ert-deftest test-qml-integration--get-buffer-name ()
  (dolist (file-name '("MyQmlFile.qml" "SomeOtherQmlFile.qml"))

    ;; qmlscene buffer name
    (let ((tool 'qmlscene)
          (expected-buffer-name
           (format "Viewing qml file '%s'" (file-name-base file-name))))
      (should
       (equal
        (qml-integration--get-buffer-name tool file-name)
        expected-buffer-name)))

    ;; qmllint buffer name
    (let ((tool 'qmllint)
          (expected-buffer-name
           (format "Linting qml file '%s'" (file-name-base file-name))))
      (should
       (equal
        (qml-integration--get-buffer-name tool file-name)
        expected-buffer-name)))

    ;; qmltest buffer name
    (let ((tool 'qmltestrunner)
          (expected-buffer-name
           (format "Running test file '%s'" (file-name-base file-name))))
      (should
       (equal
        (qml-integration--get-buffer-name tool file-name)
        expected-buffer-name)))))


(ert-deftest test-qml-integration--get-extra-args ()
  (let ((qml-integration-qmlscene-extra-args
         '("--fullscreen" "--some-other-arg"))
        (qml-integration-qmllint-extra-args '("--strict"))
        (qml-integration-qmltestrunner-extra-args '("-silent")))
    ;; qmlscene extra args
    (should
     (equal
      (qml-integration--get-extra-args 'qmlscene)
      '("--fullscreen" "--some-other-arg")))

    ;; qmllint extra args
    (should (equal (qml-integration--get-extra-args 'qmllint) '("--strict")))

    ;; qmltestrunner extra args
    (should
     (equal (qml-integration--get-extra-args 'qmltestrunner) '("-silent")))))


(ert-deftest test-qml-integration--get-import-directories-as-args ()
  (let ((qml-integration-import-directories '("." "imports")))
    ;; qmlscene import directories
    (should
     (equal
      (qml-integration--get-import-directories-as-args 'qmlscene)
      '("-I" "." "-I" "imports")))

    ;; qmllint import directories
    (should
     (equal
      (qml-integration--get-import-directories-as-args 'qmllint)
      '("-I" "." "-I" "imports")))

    ;; qmltestrunner import directories
    (should
     (equal
      (qml-integration--get-import-directories-as-args
       'qmltestrunner)
      '("-import" "." "-import" "imports")))))


(ert-deftest test-qml-integration--get-qml-file-as-args ()
  (let ((qml-file "some-file.qml"))
    ;; qmlscene qml file args
    (should
     (equal
      (qml-integration--get-qml-file-as-args 'qmlscene qml-file)
      (list qml-file)))

    ;; qmllint qml file args
    (should
     (equal
      (qml-integration--get-qml-file-as-args 'qmllint qml-file)
      (list qml-file)))

    ;; qmltestrunner qml file args
    (should
     (equal
      (qml-integration--get-qml-file-as-args
       'qmltestrunner qml-file)
      (list "-input" qml-file)))

    ;; qmltestrunner qml file args when no QML file is passed
    (should
     (equal (qml-integration--get-qml-file-as-args 'qmltestrunner) nil))))


(ert-deftest test-qml-integration--get-tool-program-args ()
  (let ((qml-integration-import-directories '("." "imports"))
        (qml-integration-qmlscene-extra-args
         '("--fullscreen" "--some-other-arg"))
        (qml-integration-qmllint-extra-args '("--strict"))
        (qml-integration-qmltestrunner-extra-args '("-silent"))
        (qml-file "some-file.qml"))

    (should
     (equal
      (qml-integration--get-tool-program-args 'qmlscene qml-file)
      (append
       (qml-integration--get-import-directories-as-args 'qmlscene)
       qml-integration-qmlscene-extra-args
       (qml-integration--get-qml-file-as-args 'qmllint qml-file))))

    (should
     (equal
      (qml-integration--get-tool-program-args 'qmllint qml-file)
      (append
       (qml-integration--get-import-directories-as-args 'qmllint)
       qml-integration-qmllint-extra-args
       (qml-integration--get-qml-file-as-args 'qmllint qml-file))))

    ;; qmltestrunner with a QML file
    (should
     (equal
      (qml-integration--get-tool-program-args 'qmltestrunner qml-file)
      (append
       (qml-integration--get-import-directories-as-args 'qmltestrunner)
       qml-integration-qmltestrunner-extra-args
       (qml-integration--get-qml-file-as-args 'qmltestrunner qml-file))))

    ;; qmltestrunner without a QML file
    (should
     (equal
      (qml-integration--get-tool-program-args 'qmltestrunner)
      (append
       (qml-integration--get-import-directories-as-args
        'qmltestrunner)
       qml-integration-qmltestrunner-extra-args)))))


(provide 'qml-integration-tests)
;;; qml-integration-tests.el ends here
