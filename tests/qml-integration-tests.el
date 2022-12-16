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


(ert-deftest test-qml-integration--get-find-command-string()
  (let ((qml-integration-ignored-paths nil))
    (should (equal
             (qml-integration--get-find-command-string "query")
             (format "find %s -type f -iname \"query\"" (project-root (project-current))))))

  (let ((qml-integration-ignored-paths '("path1")))
    (should (equal
             (qml-integration--get-find-command-string "query")
             (format "find %s -not -path \"path1\" -type f -iname \"query\"" (project-root (project-current)))))
    )

  (let ((qml-integration-ignored-paths '("path1" "path2" "path3")))
    (should (equal
             (qml-integration--get-find-command-string "query")
             (format "find %s -not -path \"path1\" -not -path \"path2\" -not -path \"path3\" -type f -iname \"query\"" (project-root (project-current)))))
    )
  )


;; TODO: test-qml-integration-get-qml-files
;; TODO: test-qml-integration-get-qml-test-files


(ert-deftest test-qml-integration--get-styles ()
  (let ((qml-integration-user-styles nil))
    (should (equal (qml-integration--get-styles) '("Fusion" "material" "Universal" "Plasma")))
    )

  (let ((qml-integration-user-styles '("mystyle1" "mystyle2")))
    (should (equal (qml-integration--get-styles) '("mystyle1" "mystyle2" "Fusion" "material" "Universal" "Plasma")))))


;; TODO: test-qml-integration-choose-qml-style


(ert-deftest test-qml-integration--get-style-string ()
  (let ((qml-integration-qt-quick-controls-style nil))
    (should (equal (qml-integration--get-style-string) ""))
    )

  (let ((qml-integration-qt-quick-controls-style "some-style"))
    (should (equal (qml-integration--get-style-string) "QT_QUICK_CONTROLS_STYLE=some-style"))))


;; TODO: test-qml-integration-run-qmlscene
;; TODO: test-qml-integration-run-qmltestrunner





;; : find . -not -path "./build*" -not -path "*/.*" -type f | wc -l


;; : find . -type d \( -path "*/.*" -prune -o -path "./Documentation" -prune -o -path "./package" -prune -o -path "./sdk" -prune -o -path "./Tools" -prune -o -path "./trust_agent_ipc" -prune -o -path "./Dockerfile" -prune -o -path "./cmake" -prune -o -path "./licenses" -prune -o -path "./build*" -prune -o -path "./modules/_submodules" -prune -o -path "./modules/safe-c-library" -prune -o -path "./modules/logger" -prune -o -path "./modules/legacy" -prune -o -path "./modules/broker*" -prune -o -path "./modules/test*" -prune -o -path "./modules/edid_retriever" -prune -o -path "./modules/customization_example" -prune -o -path "./modules/vchan_plugins" -prune -o -path "./modules/tera_crypto" -prune -o -path "./modules/ndk_build" -prune -o -path "./modules/utils" -prune -o -path "./modules/customization" -prune -o -path "./modules/layer_4" -prune -o -path "./modules/android" -prune -o -path "./modules/high_performance_client" -prune -o -path "*/localization" -prune -o -path "*/translations" -prune -o -path "*/build" -prune \) -o -type f -name "*qml" | wc -l

;; : find . -type d \( -path "*/.*" -prune -o -path "./Documentation" -prune -o -path "./package" -prune -o -path "./sdk" -prune -o -path "./Tools" -prune -o -path "./trust_agent_ipc" -prune -o -path "./Dockerfile" -prune -o -path "./cmake" -prune -o -path "./licenses" -prune -o -path "./build*" -prune -o -path "./modules/_submodules" -prune -o -path "./modules/safe-c-library" -prune -o -path "./modules/logger" -prune -o -path "./modules/legacy" -prune -o -path "./modules/broker*" -prune -o -path "./modules/test*" -prune -o -path "./modules/edid_retriever" -prune -o -path "./modules/customization_example" -prune -o -path "./modules/vchan_plugins" -prune -o -path "./modules/tera_crypto" -prune -o -path "./modules/ndk_build" -prune -o -path "./modules/utils" -prune -o -path "./modules/customization" -prune -o -path "./modules/layer_4" -prune -o -path "./modules/android" -prune -o -path "./modules/high_performance_client" -prune -o -path "*/localization" -prune -o -path "*/translations" -prune -o -path "*/build" -prune \) -not -path "*/.*" -not -path "./build*" -o -type f -name "*qml" | wc -l



;; TODO: Finish the tests


(provide 'qml-integration-tests)
;;; qml-integration-tests.el ends here
