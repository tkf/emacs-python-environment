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

(eval-when-compile (require 'cl))
(require 'deferred)

(defconst python-environment-version "0.0.0")

(defcustom python-environment-root
  (locate-user-emacs-file "python-environment")
  "Path to default Python virtual environment."
  :group 'python-environment)

(defcustom python-environment-virtualenv
  (list "virtualenv" "--system-site-packages")
  "virtualenv command to use."
  :group 'python-environment)

(defvar python-environment--verbose nil)

(defun python-environment--deferred-process (msg command)
  (message "%s..." msg)
  (deferred:$
    (apply #'deferred:process command)
    (deferred:watch it
      (apply-partially
       (lambda (msg output)
         (message "%s...Done" msg)
         (when python-environment--verbose
           (princ output)))
       msg))))

(defun python-environment-make (&optional root virtualenv)
  "Make virtualenv at ROOT asynchronously and return a deferred object.
If VIRTUALENV (list of string) is specified, it is used instead of
`python-environment-virtualenv'."
  (let ((path (convert-standard-filename (expand-file-name
                                          (or root python-environment-root)))))
    (python-environment--deferred-process
     (format "Making virtualenv at %s" path)
     (append (or virtualenv python-environment-virtualenv)
             (list path)))))

(defun python-environment-exists-p (&optional root)
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

(defun python-environment-run (command &optional root virtualenv)
  "Run COMMAND installed in Python virtualenv located at ROOT
asynchronously and return a deferred object.
If ROOT is not specified, shared virtual environment specified by
`python-environment-root' is used.
If VIRTUALENV (list of string) is specified, it is used instead of
`python-environment-virtualenv'.

Use `python-environment-run-block' if you want to wait until
the command exit."
  (if (python-environment-exists-p root)
      (python-environment--run-1 command root)
    (deferred:$
      (python-environment-make root virtualenv)
      (deferred:nextc it
        (apply-partially
         (lambda (command root _)
           (python-environment--run-1 command root))
         command root)))))

(defun python-environment-run-block (command &optional root virtualenv)
  "Blocking version of `python-environment-run'.

.. warning:: This is experimental!"
  ;; FIXME: DON'T USE `deferred:sync!'!!!
  (lexical-let (raised)
    (prog1
        (deferred:sync! (deferred:error
                          (python-environment-run command root virtualenv)
                          (lambda (err) (setq raised err))))
      (when raised
        (error raised)))))

(provide 'python-environment)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; python-environment.el ends here
