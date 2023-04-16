(load "package.lisp")
(load "utils.lisp")
(load "main.lisp")
(in-package :music-player)
(sb-ext:save-lisp-and-die "test" :executable t
		   :toplevel 'run-app)
