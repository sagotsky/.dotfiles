    ;;; This is for GNU Emacs 22 
    (defun terminal-init-screen () 
      "Terminal initialization function for screen." 
      ;; Use the xterm color initialization code. 
      (load "term/xterm") 
      (xterm-register-default-colors) 
      (tty-set-up-initial-frame-faces)) 

    ;;; This is for GNU Emacs 21 
    (if (= 21 emacs-major-version) 
        (load "term/xterm-256color"))