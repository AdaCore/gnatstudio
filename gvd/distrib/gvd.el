
;;  This file provides a new command in Emacs that starts the gvd
;;  debugger.
;;  This command should be used just as the "gdb" command.
;;
;;  Try typing
;;      M-x gvd


(require 'gud)

(defun gvd (command-line)
  "Run the GVD debugger with COMMAND-LINE.
Special arguments are added to the command line so that an Emacs
window is directly used by GVD as its source editor."

  (interactive
   (list (read-from-minibuffer "Run gvd (like this): "
			       (if (consp gud-gdb-history)
				   (car gud-gdb-history)
				 "gvd ")
			       gdb-minibuffer-local-map nil
			       '(gud-gdb-history . 1))))

  ;;  Check that we are really using GVD
  
  (if (not (string-match "^[^ \t]*gvd" command-line))
      (error "This function only works with the gvd debugger"))

  ;;  Add the special arguments
  
  (let ((frame (make-frame '((visibility . nil)))))
    (set 'command-line
	 (concat
	  command-line " --editor-window="
	  (cdr (assoc 'outer-window-id (frame-parameters frame)))))
    (select-frame frame))
  (gdb command-line))
  
   
  