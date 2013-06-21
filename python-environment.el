;;; python-environment.el --- virtualenv API for Emacs Lisp

;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; python-environment.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; python-environment.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with python-environment.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'deferred)

(defvar python-environment--source-dir (if load-file-name
                                           (file-name-directory load-file-name)
                                         default-directory))

(defvar python-environment-root
  (expand-file-name "env" python-environment--source-dir))

(defvar python-environment-virtualenv "virtualenv")

(defun python-environment-make (&optional root)
  (deferred:process python-environment-virtualenv
    (or root python-environment-root)))

(defun python-environment-exist-p (&optional root)
  (file-exists-p (or root python-environment-root)))

(defun python-environment--existing (root &rest paths)
  (when paths
    (if (file-exists-p (expand-file-name (car paths)
                                         (or root python-environment-root)))
        (car paths)
      (python-environment--existing (cdr paths)))))

(defun python-environment-bin (program &optional root)
  (python-environment--existing root
                                (concat "bin/" program)
                                (concat "Script/" program)))

(defun python-environment-lib (program &optional root)
  (python-environment--existing root
                                (concat "lib/" program)
                                (concat "Lib/" program)))

(defun python-environment--run-1 (&optional command root)
  (apply #'deferred:process
         (python-environment-bin (car command) root)
         (cdr command)))

(defun python-environment-run (&optional command root)
  (if (python-environment-exist-p root)
      (python-environment--run-1 command root)
    (deferred:$
      (python-environment-make)
      (deferred:nextc it
        (lambda (_)
          (python-environment--run-1 command root))))))

(provide 'python-environment)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; python-environment.el ends here
