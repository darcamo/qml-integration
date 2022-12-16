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


(ert-deftest test-qml-integration--get-qmlscene-import-directories-string ()
  (let ((qml-integration-import-directories '()))
    (should (equal (qml-integration--get-qmlscene-import-directories-string)
                   "")))
  (let ((qml-integration-import-directories '(".")))
    (should (equal (qml-integration--get-qmlscene-import-directories-string)
                   "-I .")))
  (let ((qml-integration-import-directories '("." "imports")))
    (should (equal (qml-integration--get-qmlscene-import-directories-string)
                   "-I . -I imports")))
  )


(ert-deftest test-qml-integration--get-qmltestrunner-import-directories-string ()
  (let ((qml-integration-import-directories '()))
    (should (equal (qml-integration--get-qmltestrunner-import-directories-string)
                   "")))
  (let ((qml-integration-import-directories '(".")))
    (should (equal (qml-integration--get-qmltestrunner-import-directories-string)
                   "-import .")))
  (let ((qml-integration-import-directories '("." "imports")))
    (should (equal (qml-integration--get-qmltestrunner-import-directories-string)
                   "-import . -import imports")))
  )


(ert-deftest qml-integration--get-fd-command-string()
  (should (equal
           (qml-integration--get-fd-command-string "query")
           (format "fd -t f %s %s" "query" (project-root (project-current))))))


;; TODO: Finish the tests


(provide 'qml-integration-tests)
;;; qml-integration-tests.el ends here
