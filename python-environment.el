;;; python-environment.el --- virtualenv API for Emacs Lisp

;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Keywords: applications, tools
;; Version: 0
;; Package-Requires: ((deferred "0.3.1"))

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

(defconst python-environment-version "0.0.0")

(defvar python-environment--source-dir (if load-file-name
                                           (file-name-directory load-file-name)
                                         default-directory))

(defcustom python-environment-root
  (expand-file-name "env" python-environment--source-dir)
  "Path to default Python virtual environment."
  :group 'python-environment)

(defcustom python-environment-virtualenv "virtualenv"
  "virtualenv command to use."
  :group 'python-environment)

(defun python-environment--deferred-process (msg command)
  (message "%s..." msg)
  (deferred:$
    (apply #'deferred:process command)
    (deferred:watch it
      (apply-partially
       (lambda (msg _) (message "%s...Done" msg))
       msg))))

(defun python-environment-make (&optional root)
  "Make virtualenv at ROOT asynchronously and return the deferred object."
  (python-environment--deferred-process
   (format "Making virtualenv at %s" root)
   (list python-environment-virtualenv
         (or root python-environment-root))))

(defun python-environment-exist-p (&optional root)
  "Return non-`nil' if virtualenv at ROOT exists."
  (let ((bin (python-environment-bin root)))
    (and bin (file-exists-p bin))))

(defun python-environment--existing (root &rest paths)
  (when paths
    (let ((full-path (expand-file-name (car paths)
                                       (or root python-environment-root))))
      (if (file-exists-p full-path)
          full-path
        (python-environment--existing (cdr paths))))))

(defun python-environment-bin (path &optional root)
  "Return full path to ``ROOT/bin/PATH`` or ``ROOT/Script/PATH`` if exists.
``Script`` is used instead of ``bin`` in typical Windows case."
  (python-environment--existing root
                                (concat "bin/" path)
                                (concat "Script/" path)))

(defun python-environment-lib (path &optional root)
  "Return full path to ``ROOT/lib/PATH`` or ``ROOT/Lib/PATH`` if exists.
``Lib`` is used instead of ``lib`` in typical Windows case."
  (python-environment--existing root
                                (concat "lib/" path)
                                (concat "Lib/" path)))

(defun python-environment--run-1 (&optional command root)
  (python-environment--deferred-process
   (format "Running: %s" (mapconcat 'identity command " "))
   (cons (python-environment-bin (car command) root)
         (cdr command))))

(defun python-environment-run (command &optional root)
  "Run COMMAND installed in Python virtualenv located at ROOT.
If ROOT is not specified, shared virtual environment specified by
`python-environment-root' is used.

Use `python-environment-run-block' if you want to wait until
the command exit."
  (if (python-environment-exist-p root)
      (python-environment--run-1 command root)
    (deferred:$
      (python-environment-make root)
      (deferred:nextc it
        (apply-partially
         (lambda (command root _)
           (python-environment--run-1 command root))
         command root)))))

(defun python-environment-run-block (command &optional root)
  "Blocking version of `python-environment-run'.

.. warning:: This is experimental!"
  ;; FIXME: DON'T USE `deferred:sync!'!!!
  (deferred:sync! (python-environment-run command root)))

(provide 'python-environment)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; python-environment.el ends here
