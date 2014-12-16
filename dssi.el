;;; dssi.el --- manage dssi instruments

;;; Author: Stefano Barbi
;;; Version: 0.0.1

;;; Commentary:
;; the dssi package is a front end for standard dssi utilities
;; 
;;

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'ido)
(require 'helm)
(require 'helm-adaptive)

(defvar dssi-plugin-description-regexp
  "Regexp that matches plugin description"
  (rx bol
      (1+ (any space))
      (group (1+ (not (any space))))
      (1+ (any space))
      (group (1+ anything))
      eol))

(defun dssi-parse-description (line)
  "Return nil if LINE is not a dssi description or an alist if matches."
  (when (string-match dssi-plugin-description-regexp line)
    (list (cons 'name (replace-match "\\1" nil nil line))
	  (cons 'description (replace-match "\\2" nil nil line))
	  (cons 'path ""))))

(defun dssi--list-plugins ()
  "Construct an alist of plugins in DSSI_PATH."
  (let ((lines (reverse (process-lines "dssi_list_plugins")))
	(res) (descriptions))
    (dolist (line lines res descriptions)
      (cond
       ((string-match-p "^warning:" line) nil)
       
       ((string-match dssi-plugin-description-regexp line)
	(setq descriptions (cons (dssi-parse-description line)
				 descriptions)))
       ((string-match "^/" line)
	(dolist (instr descriptions)
	  (setcdr (assoc 'path instr) line))
	(setq res (append descriptions res))
	(setq descriptions nil))))
    res))

(defun dssi-start-plugin (name alist)
  "NAME is the name of plugin.  ALIST is an alist obtained by calling `dssi-list-plugins'."
  (interactive
   (let ((alist (dssi--list-plugins)))
     (let ((name (ido-completing-read "DSSI plugin: "
				      (mapcar (lambda (el) (cdr (assq 'name el)))
					      alist))))
       (list name alist))))
  (let ((instr (find-if (lambda (el) (equal (cdr (assq 'name el)) name)) alist)))
    (when instr
      (start-process name
		     name
		     "jack-dssi-host"
		     "-a"
		     "-n"
		     (format "%s:%s" (cdr (assq 'path instr)) name)))))


(defvar dssi-plugin-alist)

(defvar helm-dssi-plugins
  '((name . "DSSI Plugins")
    (init . (lambda ()
	      (setq dssi-plugin-alist (dssi--list-plugins))))
    (candidates . (lambda ()
		    (mapcar (lambda (el) (cdr (assq 'name el))) dssi-plugin-alist)))
    (filtered-candidate-transformer
     helm-adaptive-sort)
    ))

(defun helm-dssi-plugins ()
  (interactive)
  (helm 'helm-dssi-plugins "*DSSI Plugins*"))


;;; dssi.el ends here
