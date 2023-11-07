;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       "/home/david/")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :asdf)
(push #p"/home/david/.emacs.d/elpa/sly-20231009.2150/slynk/" ASDF:*CENTRAL-REGISTRY*)
(asdf:require-system :slynk)
(slynk:create-server :port 4006)
