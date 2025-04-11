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


(ert-deftest test-qml-integration--get-import-directories-string ()
  ;; This is a parametric test. The `all-tests-data` variable is a
  ;; list, where each element is a list of parameters corresponding to
  ;; a single test. The first parameter is the tool, followed by the
  ;; list of import directories and then the expected string from
  ;; applying `qml-integration--get-import-directories-string`.
  (let ((all-tests-data '((qmlscene () "")
                          (qmlscene (".") "-I .")
                          (qmlscene ("." "imports") "-I . -I imports")
                          (qmltestrunner () "")
                          (qmltestrunner (".") "-import .")
                          (qmltestrunner ("." "imports") "-import . -import imports")
                          (qmllint () "")
                          (qmllint (".") "-I .")
                          (qmllint ("." "imports") "-I . -I imports")
                          )))

    ;; Decompose test-data for a test into the `tool`,
    ;; `qml-integration-import-directories` and `expected-string`
    ;; variables
    (dolist (test-data all-tests-data)
      (pcase-let* ((`(,tool
                      ,qml-integration-import-directories
                      ,expected-string)
                    test-data))
        (should (equal (qml-integration--get-import-directories-string tool) expected-string))))))


(ert-deftest qml-integration--get-fd-command-string()
  (should (equal
           (qml-integration--get-fd-command-string "query")
           (format "fd -t f %s %s" "query" (project-root (project-current))))))


(ert-deftest test-qml-integration--get-find-command-string()
  (let ((qml-integration-ignored-paths nil))
    (should (equal
             (qml-integration--get-find-command-string "query")
             (format "find %s -type f -iname \"query\"" (project-root (project-current))))))

  (let ((qml-integration-ignored-paths '("path1")))
    (should (equal
             (qml-integration--get-find-command-string "query")
             (format "find %s -not -path \"path1\" -type f -iname \"query\"" (project-root (project-current))))))

  (let ((qml-integration-ignored-paths '("path1" "path2" "path3")))
    (should (equal
             (qml-integration--get-find-command-string "query")
             (format "find %s -not -path \"path1\" -not -path \"path2\" -not -path \"path3\" -type f -iname \"query\"" (project-root (project-current))))))
  )


(ert-deftest test-qml-integration--get-files-using-find ()
  ;; Test getting regular qml files
  (test-fixture-setup
   "test-empty-project"
   (lambda ()
     (let ((expected-files nil))
       (should (seq-set-equal-p (qml-integration--get-files-using-find "*qml") expected-files)))))

  (let ((qml-integration-ignored-paths nil))
    (test-fixture-setup
     "test-project"
     (lambda ()
       (let ((expected-files
              (mapcar 'expand-file-name '("path1/path1-file2.qml"
                                          "path1/path1-file1.qml"
                                          "path2/path2-file2.qml"
                                          "path2/path2-file1.qml"
                                          "ignore-path1/ignore-path1-file1.qml"
                                          "ignore-path2/ignore-path2-file1.qml"
                                          "tests/tst_test1.qml"
                                          "tests/tst_test2.qml"
                                          "tests/tst_test3.qml"))))
         (should (seq-set-equal-p (qml-integration--get-files-using-find "*qml") expected-files))))))

  (let ((qml-integration-ignored-paths '("*/ignore-path1/*" "*/ignore-path2/*")))
    (test-fixture-setup
     "test-project"
     (lambda ()
       (let ((expected-files
              (mapcar 'expand-file-name '("path1/path1-file2.qml"
                                          "path1/path1-file1.qml"
                                          "path2/path2-file2.qml"
                                          "path2/path2-file1.qml"
                                          "tests/tst_test1.qml"
                                          "tests/tst_test2.qml"
                                          "tests/tst_test3.qml"
                                          ))))
         (should (seq-set-equal-p (qml-integration--get-files-using-find "*qml") expected-files))))))

  ;; Test getting test files
  (test-fixture-setup
   "test-project"
   (lambda ()
     (let ((qml-integration-ignored-paths '("*/ignore-path1/*" "*/ignore-path2/*"))
           (expected-files
            (mapcar 'expand-file-name '("tests/tst_test1.qml"
                                        "tests/tst_test2.qml"
                                        "tests/tst_test3.qml"))))
       (should (seq-set-equal-p (qml-integration--get-files-using-find "tst_*.qml") expected-files))))))


(ert-deftest test-qml-integration--get-files-using-fd ()
  ;; Test getting regular qml files
  (test-fixture-setup
   "test-empty-project"
   (lambda ()
     (let ((expected-files nil))
       (should (seq-set-equal-p (qml-integration--get-files-using-fd ".qml$") expected-files)))))

  (test-fixture-setup
   "test-project"
   (lambda ()
     (let ((expected-files
            (mapcar 'expand-file-name '("path1/path1-file2.qml"
                                        "path1/path1-file1.qml"
                                        "path2/path2-file2.qml"
                                        "path2/path2-file1.qml"
                                        "ignore-path1/ignore-path1-file1.qml"
                                        "ignore-path2/ignore-path2-file1.qml"
                                        "tests/tst_test1.qml"
                                        "tests/tst_test2.qml"
                                        "tests/tst_test3.qml"))))
       (should (seq-set-equal-p (qml-integration--get-files-using-fd ".qml$") expected-files)))))

  ;; Test getting test files
  (test-fixture-setup
   "test-project"
   (lambda ()
     (let ((expected-files
            (mapcar 'expand-file-name '("tests/tst_test1.qml"
                                        "tests/tst_test2.qml"
                                        "tests/tst_test3.qml"))))
       (should (seq-set-equal-p (qml-integration--get-files-using-fd "\"tst_.*.qml$\"") expected-files))))))


(ert-deftest test-qml-integration--get-styles ()
  (let ((qml-integration-user-styles nil))
    (should (equal (qml-integration--get-styles) '("Fusion" "material" "Universal" "Plasma"))))

  (let ((qml-integration-user-styles '("mystyle1" "mystyle2")))
    (should (equal (qml-integration--get-styles) '("mystyle1" "mystyle2" "Fusion" "material" "Universal" "Plasma")))))


(ert-deftest test-qml-integration--get-style-string ()
  (let ((qml-integration-qt-quick-controls-style nil))
    (should (equal (qml-integration--get-style-string) "")))

  (let ((qml-integration-qt-quick-controls-style "some-style"))
    (should (equal (qml-integration--get-style-string) "QT_QUICK_CONTROLS_STYLE=some-style"))))


(ert-deftest test-qml-integration--get-qmlscene-run-command ()
  (let* ((qml-file "MyQmlFile.qml")
         (qml-integration-qml-root-folder "some-root-folder")
         (qml-integration-qmlscene-program "qmlscene")
         (qml-integration-import-directories '("." "imports"))
         (qml-integration-qmlscene-extra-args "--fullscreen")
         (qml-integration-qt-quick-controls-style "Fusion")
         (qml-integration-qt-install-bin-folder nil)
         (expected-command
          (format "cd %s && QT_QUICK_CONTROLS_STYLE=Fusion qmlscene -I . -I imports --fullscreen %s"
                  qml-integration-qml-root-folder
                  qml-file)))
    ;; When qml-integration-qt-install-bin-folder is nil
    (should (equal (qml-integration--get-qmlscene-run-command qml-file) expected-command))

    ;; When qml-integration-qt-install-bin-folder is set
    (setq qml-integration-qt-install-bin-folder "/some/path/")
    (setq expected-command
          (format "cd %s && QT_QUICK_CONTROLS_STYLE=Fusion /some/path/qmlscene -I . -I imports --fullscreen %s"
                  qml-integration-qml-root-folder qml-file))
    (should (equal (qml-integration--get-qmlscene-run-command qml-file) expected-command))

    ;; If qml-integration-qmlscene-program is an absolute path, then
    ;; qml-integration-qt-install-bin-folder is ignored
    (setq qml-integration-qmlscene-program "/some/otherpath/qmlscene")
    (setq expected-command
          (format "cd %s && QT_QUICK_CONTROLS_STYLE=Fusion /some/otherpath/qmlscene -I . -I imports --fullscreen %s"
                  qml-integration-qml-root-folder qml-file))
    (should (equal (qml-integration--get-qmlscene-run-command qml-file) expected-command))

    ;; If qml-integration-qmlscene-program is a relative path
    (setq qml-integration-qmlscene-program "otherpath/qmlscene")
    (setq expected-command
          (format "cd %s && QT_QUICK_CONTROLS_STYLE=Fusion /some/path/otherpath/qmlscene -I . -I imports --fullscreen %s"
                  qml-integration-qml-root-folder qml-file))
    (should (equal (qml-integration--get-qmlscene-run-command qml-file) expected-command))
    ))



(ert-deftest test_qml-integration--get-qmllint-run-command ()
  (let* ((qml-file "MyQmlFile.qml")
         (qml-integration-qml-root-folder "some-root-folder")
         (qml-integration-qmllint-program "qmllint")
         (qml-integration-import-directories '("." "imports"))
         (qml-integration-qmllint-extra-args "--strict")
         (qml-integration-qt-quick-controls-style "Fusion")
         (qml-integration-qt-install-bin-folder nil)
         (expected-command
          (format "cd %s && QT_QUICK_CONTROLS_STYLE=Fusion qmllint -I . -I imports --strict %s"
                  qml-integration-qml-root-folder
                  qml-file)))
    ;; When qml-integration-qml-root-folder is set
    (should (equal (qml-integration--get-qmllint-run-command qml-file) expected-command))

    ;; When qml-integration-qml-root-folder is nil
    (setq qml-integration-qml-root-folder nil)
    (setq expected-command
          (format "QT_QUICK_CONTROLS_STYLE=Fusion qmllint -I . -I imports --strict %s"
                  qml-file))
    (should (equal (qml-integration--get-qmllint-run-command qml-file) expected-command))

    ;; When qml-integration-qt-install-bin-folder is set
    (setq qml-integration-qt-install-bin-folder "/some/path/")
    (setq expected-command
          (format "QT_QUICK_CONTROLS_STYLE=Fusion /some/path/qmllint -I . -I imports --strict %s"
                  qml-file))
    (should (equal (qml-integration--get-qmllint-run-command qml-file) expected-command))

    ;; If qml-integration-qmllint-program is an absolute path
    (setq qml-integration-qmllint-program "/some/otherpath/qmllint")
    (setq expected-command
          (format "QT_QUICK_CONTROLS_STYLE=Fusion /some/otherpath/qmllint -I . -I imports --strict %s"
                  qml-file))
    (should (equal (qml-integration--get-qmllint-run-command qml-file) expected-command))
    ))


(ert-deftest test-qml-integration--get-qmltestrunner-run-command ()
  "Test the generation of the qmltestrunner run command."
  (let* ((qml-file "MyTestFile.qml")
         (qml-integration-qml-root-folder "some-root-folder")
         (qml-integration-qmltestrunner-program "qmltestrunner")
         (qml-integration-import-directories '("." "imports"))
         (qml-integration-qmltestrunner-extra-args "-silent")
         (qml-integration-qt-quick-controls-style "Fusion")
         (qml-integration-qt-install-bin-folder nil)
         (expected-command
          (format "cd %s && QT_QUICK_CONTROLS_STYLE=Fusion qmltestrunner -import . -import imports -silent -input %s"
                  qml-integration-qml-root-folder
                  qml-file)))
    ;; When qml-integration-qml-root-folder is set
    (should (equal (qml-integration--get-qmltestrunner-run-command qml-file) expected-command))

    ;; When qml-integration-qml-root-folder is nil
    (setq qml-integration-qml-root-folder nil)
    (setq expected-command
          (format "QT_QUICK_CONTROLS_STYLE=Fusion qmltestrunner -import . -import imports -silent -input %s"
                  qml-file))
    (should (equal (qml-integration--get-qmltestrunner-run-command qml-file) expected-command))

    ;; When qml-integration-qt-install-bin-folder is set
    (setq qml-integration-qt-install-bin-folder "/some/path/")
    (setq expected-command
          (format "QT_QUICK_CONTROLS_STYLE=Fusion /some/path/qmltestrunner -import . -import imports -silent -input %s"
                  qml-file))
    (should (equal (qml-integration--get-qmltestrunner-run-command qml-file) expected-command))

    ;; If qml-integration-qmltestrunner-program is an absolute path
    (setq qml-integration-qmltestrunner-program "/some/otherpath/qmltestrunner")
    (setq expected-command
          (format "QT_QUICK_CONTROLS_STYLE=Fusion /some/otherpath/qmltestrunner -import . -import imports -silent -input %s"
                  qml-file))
    (should (equal (qml-integration--get-qmltestrunner-run-command qml-file) expected-command))

    ;; When no QML file is not provided
    (setq expected-command
          (format "QT_QUICK_CONTROLS_STYLE=Fusion /some/otherpath/qmltestrunner -import . -import imports -silent"
                  qml-integration-qml-root-folder))
    (should (equal (qml-integration--get-qmltestrunner-run-command) expected-command))))


(provide 'qml-integration-tests)
;;; qml-integration-tests.el ends here
