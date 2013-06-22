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


When you write Emacs plugin which uses Python script, you would want to
setup Python modules for that script easily from Emacs.  You can use
python-environment.el to do that::

   (require 'python-environment)

   (defun YOUR-PLUGIN-install-python-dependencies ()
     (interactive)
     (python-environment-run "pip" "install" "epc"))

In your plugin, if you want the path to command installed in the
virtualenv, do something like this::

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
