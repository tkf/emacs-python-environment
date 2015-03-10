======================================
 Python virtualenv API for Emacs Lisp
======================================

.. sidebar:: Links:

   * `Repository
     <https://github.com/tkf/emacs-python-environment>`_ (at GitHub)
   * `Issue tracker
     <https://github.com/tkf/emacs-python-environment/issues>`_ (at GitHub)
   * `Travis CI
     <https://travis-ci.org/tkf/emacs-python-environment>`_ |build-status|
   * `MELPA
     <http://melpa.org/#/python-environment>`_ |melpa-badge|
   * `MELPA Stable
     <http://stable.melpa.org/#/python-environment>`_ |melpa-stable-badge|


Emacs integrates well with external tools written in languages other
than Emacs Lisp and thus use of these tools should be encouraged.
However, many people try to avoid using non-Emacs Lisp software tools
since it makes installation of their Emacs plugin hard.
python-environment.el solves this problem (only for the case the tool
is written in Python) by providing virtualenv API in Emacs Lisp so
that you can automate installation of tools written in Python.

Let's say you write Emacs plugin which uses Python script.  Then you
would want to setup Python modules for that script easily from Emacs.
This is how to do that using python-environment.el

.. code :: cl

   (require 'python-environment)

   (defun YOUR-PLUGIN-install-python-dependencies ()
     (interactive)
     (python-environment-run "pip" "install" "epc"))

In your plugin, if you want the path to command installed in the
virtualenv, do something like this

.. code :: cl

   (start-process NAME BUFFER
                  (python-environment-bin "COMMAND")
                  ...)

Path to access COMMAND may be different in \*nix and in Windows, but
python-environment.el finds the right path.


.. Build status badge
.. |build-status|
   image:: https://secure.travis-ci.org/tkf/emacs-python-environment.png
           ?branch=master
   :target: http://travis-ci.org/tkf/emacs-python-environment
   :alt: Build Status
.. |melpa-badge|
   image:: http://melpa.org/packages/python-environment-badge.svg
   :target: http://melpa.org/#/python-environment
   :alt: MELPA Badge
.. |melpa-stable-badge|
   image:: http://stable.melpa.org/packages/python-environment-badge.svg
   :target: http://stable.melpa.org/#/python-environment
   :alt: MELPA Stable Badge
