;;; test-python-environment.el --- Tests for python-environment.el

;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; test-python-environment.el is free software: you can redistribute
;; it and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; test-python-environment.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with test-python-environment.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'python-environment)

(defmacro pye-test-with-temp-env (&rest body)
  (declare (debug (&rest form))
           (indent 0))
  (let ((path (make-symbol "path")))
    `(let* ((,path (make-temp-file "pye-test-" t))
            (python-environment-root ,path))
       (unwind-protect
           (progn ,@body)
         (delete-directory ,path t)))))

(defmacro pye-deftest (name args &rest body)
  (declare (debug (&define :name test
                           name sexp [&optional stringp]
                           [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  `(ert-deftest ,name ,args
     (pye-test-with-temp-env
       ,@body)))

(pye-deftest pye-test-make-environment ()
  (deferred:sync! (python-environment-make)))

(pye-deftest pye-test-run ()
  (deferred:sync! (python-environment-run '("python" "--version"))))

(pye-deftest pye-test-run-block ()
  (python-environment-run-block '("python" "--version")))

(pye-deftest pye-test-block-error ()
  (let (noerror)
    (ignore-errors
      (python-environment-run-block '("python" "-c" "1/0"))
      (setq noerror t))
    (when noerror
      (error "error is NOT raised in `python-environment-run-block'"))))

(provide 'test-python-environment)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; test-python-environment.el ends here
