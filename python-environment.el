;;; python-environment.el --- virtualenv API for Emacs Lisp

;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Keywords: applications, tools
;; Version: 0.0.1alpha1
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

(defconst python-environment-version "0.0.1alpha1")

(defcustom python-environment-directory
  (locate-user-emacs-file ".python-environments")
  "Path to directory to store all Python virtual environments"
  :group 'python-environment)

(defcustom python-environment-default-root-name "default"
  "Default Python virtual environment name.
This is a name of directory relative to `python-environment-directory'.
Thus, typically the default virtual environment path is
``~/.emacs.d/.python-environments/default``."
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

(defun python-environment--blocking-process (msg command)
  (message "%s (SYNC)..." msg)
  (let ((exit-code
         (if python-environment--verbose
             (with-temp-buffer
               (apply #'call-process (car command)
                      nil                    ; INFILE (no input)
                      t                      ; BUFFER (output to this buffer)
                      nil                    ; DISPLAY (no refresh is needed)
                      (cdr command))
               (princ (buffer-string)))
           (apply #'call-process (car command) nil nil nil (cdr command)))))
    (message "%s (SYNC)...Done" msg)
    (unless (= exit-code 0)
      (error "Command %S exits with error code %S." command exit-code))))


(defun python-environment-root-path (&optional root)
  (expand-file-name (or root python-environment-default-root-name)
                    python-environment-directory))

(defun python-environment--make-with-runner (proc-runner root virtualenv)
  (let ((path (convert-standard-filename
               (python-environment-root-path root)))
        (virtualenv (or virtualenv python-environment-virtualenv)))
    (unless (executable-find (car virtualenv))
      (error "Program named %S does not exist." (car virtualenv)))
    (funcall proc-runner
             (format "Making virtualenv at %s" path)
             (append virtualenv (list path)))))

(defun python-environment-make (&optional root virtualenv)
  "Make virtualenv at ROOT asynchronously and return a deferred object.
If VIRTUALENV (list of string) is specified, it is used instead of
`python-environment-virtualenv'."
  (python-environment--make-with-runner
   #'python-environment--deferred-process
   root virtualenv))

(defun python-environment-make-block (&optional root virtualenv)
  "Blocking version of `python-environment-make'.
I recommend NOT to use this function in interactive commands.
For reason, see `python-environment-run-block'"
  (python-environment--make-with-runner
   #'python-environment--blocking-process
   root virtualenv))

(defun python-environment-exists-p (&optional root)
  "Return non-`nil' if virtualenv at ROOT exists."
  (let ((bin (python-environment-bin root)))
    (and bin (file-exists-p bin))))

(defun python-environment--existing (root &rest paths)
  (when paths
    (let ((full-path (expand-file-name (car paths)
                                       (python-environment-root-path root))))
      (if (file-exists-p full-path)
          full-path
        (python-environment--existing (cdr paths))))))

(defun python-environment-bin (path &optional root)
  "Return full path to \"ROOT/bin/PATH\" or \"ROOT/Script/PATH\" if exists.
``Script`` is used instead of ``bin`` in typical Windows case."
  (python-environment--existing root
                                (concat "bin/" path)
                                (concat "Script/" path)))

(defun python-environment-lib (path &optional root)
  "Return full path to \"ROOT/lib/PATH\" or \"ROOT/Lib/PATH\" if exists.
``Lib`` is used instead of ``lib`` in typical Windows case."
  (python-environment--existing root
                                (concat "lib/" path)
                                (concat "Lib/" path)))

(defun python-environment--run-with-runner (proc-runner command root)
  (funcall proc-runner
           (format "Running: %s" (mapconcat 'identity command " "))
           (cons (python-environment-bin (car command) root)
                 (cdr command))))

(defun python-environment--run-1 (&optional command root)
  (python-environment--run-with-runner
   #'python-environment--deferred-process
   command root))

(defun python-environment--run-block-1 (command root)
  (python-environment--run-with-runner
   #'python-environment--blocking-process
   command root))

(defun python-environment-run (command &optional root virtualenv)
  "Run COMMAND installed in Python virtualenv located at ROOT
asynchronously and return a deferred object.
If ROOT is not specified, shared virtual environment specified by
`python-environment-default-root-name' is used.
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
I recommend NOT to use this function in interactive commands.
Emacs users have more important things to than waiting for some
command to finish."
  (unless (python-environment-exists-p root)
    (python-environment-make-block root virtualenv))
  (python-environment--run-block-1 command root))

(provide 'python-environment)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; python-environment.el ends here
