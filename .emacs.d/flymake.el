;;; flymake.el -- a universal on-the-fly syntax checker

;; Copyright (C) 2003 Pavel Kobiakov

;; Author:  Pavel Kobiakov <pk_at_work@yahoo.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111, USA.

;;; Commentary:
;;
;; Flymake is a minor Emacs mode performing on-the-fly syntax
;; checks using the external syntax check tool (for C/C++ this
;; is usually the compiler)

(defcustom flymake-log-level -1
    "Logging level, only messages with level > flymake-log-level will not be logged
-1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"
	:group 'flymake
	:type 'integer
)

(defun flymake-log(level text &rest args)
    "Log a message with optional arguments"
	(if (<= level flymake-log-level)
		(let* ((msg (apply 'format text args)))
		    (message msg)
			;(with-temp-buffer
			;    (insert msg)
			;	(insert "\n")
			;   (flymake-save-buffer-in-file (current-buffer) "d:/flymake.log" t)  ; make log file name customizable
			;)
		)
    )
)

(defun ins-after(list pos val)
    "insert val into list after position pos"
	(let ((tmp (copy-sequence list))) ; (???) 
	    (setcdr (nthcdr pos tmp) (cons val (nthcdr (1+ pos) tmp)))
	    tmp
	)
)

(defun set-at(list pos val)
    "set val at position pos in list"
	(let ((tmp (copy-sequence list))) ; (???) 
	    (setcar (nthcdr pos tmp) val)
	    tmp
	)
)

(defun flymake-make-output-file-name(source-file-name &optional prefix)
    (unless (stringp source-file-name)
        (error "invalid file-name")
    )
	(or prefix
		(setq prefix "flymake")
	)
    (concat (file-name-sans-extension source-file-name)
	    "_" prefix
	    (and (file-name-extension source-file-name)
		 (concat "." (file-name-extension source-file-name))))
)

(defvar flymake-pid-to-names(makehash)
    "pid -> source buffer name, output file name mapping"
)

(defun flymake-reg-names(pid source-buffer-name patched-master-file-name patched-source-file-name)
    "Save into in pid map"
    (unless (stringp source-buffer-name)
        (error "invalid buffer name")
    )
	(puthash pid (list source-buffer-name patched-master-file-name patched-source-file-name) flymake-pid-to-names)
)

(defun flymake-get-source-buffer-name(pid)
    "Return buffer name stored in pid map"
	(nth 0 (gethash pid flymake-pid-to-names))
)

(defun flymake-get-patched-master-file-name(pid)
	(nth 1 (gethash pid flymake-pid-to-names))
)

(defun flymake-get-patched-source-file-name(pid)
	(nth 2 (gethash pid flymake-pid-to-names))
)

(defun flymake-unreg-names(pid)
    "Delete pid->buffer name mapping"
	(remhash pid flymake-pid-to-names)
)

(defun flymake-get-buffer-var(buffer var-name)
    "switch to buffer if necessary and return local variable var"
	(unless (bufferp buffer)
	    (error "invalid buffer")
	)
			
	(if (eq buffer (current-buffer))
		(symbol-value var-name)
	;else
	    (save-excursion
		    (set-buffer buffer)
			(symbol-value var-name)
		)
	)
)

(defun flymake-set-buffer-var(buffer var-name var-value)
    "switch to buffer if necessary and set local variable var-name to var-value"
	(unless (bufferp buffer)
	    (error "invalid buffer")
	)
			
	(if (eq buffer (current-buffer))
		(set var-name var-value)
	;else
	    (save-excursion
		    (set-buffer buffer)
			(set var-name var-value)
		)
	)
)

(defvar flymake-base-dir nil
    ""
)
(make-variable-buffer-local 'flymake-base-dir)

(defun flymake-get-buffer-base-dir(buffer)
    (flymake-get-buffer-var buffer 'flymake-base-dir)
)
(defun flymake-set-buffer-base-dir(buffer base-dir)
    (flymake-set-buffer-var buffer 'flymake-base-dir base-dir)
)

(defvar flymake-master-file-name nil)
(make-variable-buffer-local 'flymake-master-file-name)
(defun flymake-get-buffer-master-file-name(buffer)
    (flymake-get-buffer-var buffer 'flymake-master-file-name)
)
(defun flymake-set-buffer-master-file-name(buffer master-file-name)
    (flymake-set-buffer-var buffer 'flymake-master-file-name master-file-name)
)

(defvar flymake-include-dirs nil)
(make-variable-buffer-local 'flymake-include-dirs)
(defun flymake-get-buffer-include-dirs(buffer)
    (flymake-get-buffer-var buffer 'flymake-include-dirs)
)
(defun flymake-set-buffer-include-dirs(buffer include-dirs)
    (flymake-set-buffer-var buffer 'flymake-include-dirs include-dirs)
)

(defvar flymake-output-residual nil
  ""
)
(make-variable-buffer-local 'flymake-output-residual)
(defun flymake-get-buffer-output-residual(buffer)
    (flymake-get-buffer-var buffer 'flymake-output-residual)
)
(defun flymake-set-buffer-output-residual(buffer residual)
    (flymake-set-buffer-var buffer 'flymake-output-residual residual)
)

(defcustom flymake-allowed-file-name-masks '((".+\\.c$" 1) (".+\\.cpp$" 1) (".+\\.java$" 3)
											 (".+\\.h$" 2 (".+\\.cpp$" ".+\\.c$")
											     ("[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\"" 1 2))
											 (".+\\.idl$" 1)
											 (".+\\.odl$" 1)
											 (".+[0-9]+\\.tex$" 2 (".+\\.tex$")
											     ("[ \t]*\\input[ \t]*{\\(.*\\)\\(%s\\)}" 1 2 ))
											 (".+\\.tex$" 1)
											 )
    "*Files syntax checking is allowed for"
	:group 'flymake
	:type '(repeat (string integer string))
)
 
(defun flymake-get-file-name-mode-and-masks(file-name)
    "return the corresponding entry from flymake-allowed-file-name-masks"
    (unless (stringp file-name)
        (error "invalid file-name")
    )
	(let ((count           (length flymake-allowed-file-name-masks))
		  (idx             0)
		  (mode-and-masks  nil))
	    (while (and (not mode-and-masks) (< idx count))
    	    (if (string-match (nth 0 (nth idx flymake-allowed-file-name-masks)) file-name)
				(setq mode-and-masks (cdr (nth idx flymake-allowed-file-name-masks)))
			)
	        (setq idx (1+ idx))
		)
		(flymake-log 3 "file %s, allowed=%s" file-name (car mode-and-masks))
		mode-and-masks
	)
)

(defun flymake-can-syntax-check-file(file-name)
    "Determine whether we can syntax check file-name: nil if cannot, 1 if can, 2 if can via master file"
	(nth 0 (flymake-get-file-name-mode-and-masks file-name))
)

(defun flymake-get-master-file-masks(file-name)
    "return a list of master file name masks for a given file-name"
	(nth 1 (flymake-get-file-name-mode-and-masks file-name))
)

(defun flymake-get-include-regexp(file-name)
    "return a list (regexp path-idx name-idx)"
	(nth 2 (flymake-get-file-name-mode-and-masks file-name))
)

(defcustom flymake-program-name "make"
    "Return program name to perform syntax checking"
	:group 'flymake
	:type 'string
)	

(defun flymake-get-program-name()
    flymake-program-name
)

(defun flymake-get-program-args-imp(output-file-name base-dir)
    "create a command line for the syntax check command"
	(let* ((source-dir       (file-name-directory output-file-name))
		   (source-rel-path  (flymake-build-relative-path base-dir source-dir))
		   (sources          (file-name-nondirectory output-file-name))
		   (args               ()))

        (if (> (length source-rel-path) 0)
			(setq sources (concat source-rel-path "/" sources))
		)
	    (setq args (list "-s"
						 "-C"
						 base-dir
						 (concat "SOURCES=" sources)
						 "check-syntax"))
		args
	)
)

(defcustom flymake-get-program-args-function 'flymake-get-program-args-imp
    "return a list of make args given file-name-to-compile and basedir"
	:group 'flymake
	:type 'symbol
)

(defun flymake-get-program-args(output-file-name base-dir)
    (funcall flymake-get-program-args-function  output-file-name base-dir)
)	

(defcustom flymake-buildfile-name "Makefile"
    ""
	:group 'flymake
	:type 'string
)

(defcustom flymake-buildfile-dirs '("." "..")
    "dirs to look for buildfile"
	:group 'flymake
	:type '(repeat (string))
)

(defvar flymake-find-buildfile-cache (makehash 'equal))
(defun flymake-get-buildfile-from-cache(dir-name)
    (gethash dir-name flymake-find-buildfile-cache)
)
(defun flymake-add-buildfile-to-cache(dir-name buildfile)
    (puthash dir-name buildfile flymake-find-buildfile-cache)
)
(defun flymake-clear-buildfile-cache()
    (clrhash flymake-find-buildfile-cache)
)

(defun flymake-find-buildfile(source-dir-name dirs)
    "find buildfile (i.e. Makefile, build.xml, etc.) starting from current directory. Return its path or nil if not found"
	(if (flymake-get-buildfile-from-cache source-dir-name)
		(progn
 		    (flymake-get-buildfile-from-cache source-dir-name)
		)
	;else
		(let* ((buildfile-dir          nil)
			   (buildfile              nil)
			   (dir-count              (length dirs))
			   (dir-idx                0)
			   (found                  nil))

			(while (and (not found) (< dir-idx dir-count))

				(setq buildfile-dir (concat source-dir-name (nth dir-idx dirs)))
				(setq buildfile (concat buildfile-dir "/" flymake-buildfile-name))

				(when (file-exists-p buildfile)
					(setq found t)
				)

				(setq dir-idx (1+ dir-idx))
			)
			(if found
				(progn
					(flymake-log 3 "found buildfile at %s/%s" buildfile-dir flymake-buildfile-name)
					(flymake-add-buildfile-to-cache source-dir-name buildfile-dir)
					buildfile-dir
				)
			;else
				(progn
					(flymake-log 3 "buildfile for %s not found" source-dir-name)
					nil
				)
			)
		)
	)
)

(defun flymake-fix-path-name(name)
    "replace all occurences of '\' with '/'"
	(when name
		(let* ((new-name (expand-file-name (replace-regexp-in-string "[\\]" "/" name)))
			   (last-char (elt new-name (1- (length new-name)))))
			(setq new-name (replace-regexp-in-string "\\./" "" new-name))
			(if (equal "/" (char-to-string last-char))
				(setq new-name (substring new-name 0 (1- (length new-name))))
			)
			new-name
		)
	)
)

(defun flymake-same-files(file-name-one file-name-two)
    "t if file-name-one and file-name-two actually point to the same file"
	(equal (flymake-fix-path-name file-name-one) (flymake-fix-path-name file-name-two))
)

(defun flymake-get-common-prefix(string-one string-two)
    "return common prefix for two strings"
	(let* ((substr-len   1)
		   (done         nil)
		   (max-len      (min (length string-one) (length string-two))))

	    (when (and string-one string-two)
	        (while (and (not done) (<= substr-len max-len))
				(if (not (string=
							(substring string-one 0 substr-len)
							(substring string-two 0 substr-len)))
					(setq done t)
				;else
				    (setq substr-len (1+ substr-len))
				)
		    )
	    )
	    (if (equal 1 substr-len)
			nil
		;else
		    (substring string-one 0 (1- substr-len))
		)
	)
)

(defun flymake-build-relative-path(from-dir to-dir)
    "return rel: from-dir/rel == to-dir"
    (if (not (equal (elt from-dir 0) (elt to-dir 0)))
	    (error "first chars in paths %s, %s must be equal (same drive)" from-dir to-dir)
	;else
		(let* ((from        (flymake-fix-path-name from-dir))
			   (to          (flymake-fix-path-name to-dir))
			   (prefix      (flymake-get-common-prefix from to))
			   (from-suffix (substring from (length prefix)))
			   (up-count    (length (split-string from-suffix "[/]")))
			   (to-suffix   (substring to   (length prefix)))
			   (idx         0)
			   (rel         nil))

		    (if (and (> (length to-suffix) 0) (equal "/" (char-to-string (elt to-suffix 0))))
			    (setq to-suffix (substring to-suffix 1))
			)

		    (while (< idx up-count)
			    (if (> (length rel) 0)
					(setq rel (concat rel "/"))
				)
			    (setq rel (concat rel ".."))
				(setq idx (1+ idx))
			)
			(if (> (length rel) 0)
				(setq rel (concat rel "/"))
			)
			(if (> (length to-suffix) 0)
			   (setq rel (concat rel to-suffix))
			)

		    rel
		)
	)
)

(defcustom flymake-master-file-dirs '("." "./src" "./UnitTest")
    "dirs where to llok for master files"
	:group 'flymake
	:type '(repeat (string))
)

(defcustom flymake-master-file-count-limit 32
    "max number of master files to check"
	:group 'flymake
	:type 'integer
)

(defun flymake-find-possible-master-files(file-name master-file-dirs)
    "find (by name and location) all posible master files, which are .cpp and .c for and .h.
Files are searched for starting from the .h directory and max max-level parent dirs.
File contents are not checked."
    (let* ((dir-idx    0)
		  (dir-count  (length master-file-dirs))
		  (files  nil)
		  (done   nil)
		  (masks       (flymake-get-master-file-masks file-name))
		  (masks-count (length masks)))

	    (while (and (not done) (< dir-idx dir-count))
			(let* ((dir (concat (flymake-fix-path-name (file-name-directory file-name)) "/"	(nth dir-idx master-file-dirs)))
				   (masks-idx 0))
			    (while (and (not done) (< masks-idx masks-count))
			        (let* ((mask        (nth masks-idx masks))
						   (dir-files   (directory-files dir t mask))
						   (file-count  (length dir-files))
						   (file-idx    0))

			            (flymake-log 3 "dir %s, %d file(s) for mask %s" dir file-count mask)
				        (while (and (not done) (< file-idx file-count))
				            (when (not (file-directory-p (nth file-idx dir-files)))
					            (setq files (cons (nth file-idx dir-files) files))
						        (when (>= (length files) flymake-master-file-count-limit)
						            (flymake-log 3 "master file count limit (%d) reached" flymake-master-file-count-limit)
						            (setq done t)
						        )
				            )
				            (setq file-idx (1+ file-idx))
						)
			        )
					(setq masks-idx (1+ masks-idx))
				)
			)
			(setq dir-idx (1+ dir-idx))
		)
		(when files
		    (setq flymake-included-file-name (file-name-nondirectory file-name))
		    (setq files (sort files 'flymake-master-file-compare))
			(setq flymake-included-file-name nil)
		)
		(flymake-log 3 "found %d possible master file(s)" (length files))
		files
	)
)

(defvar flymake-included-file-name nil ; this is used to pass a parameter to a sort predicate below
    ""
)

(defun flymake-master-file-compare(file-one file-two)
    "used in sort to move most possible file names to the beginning of the list (File.h -> File.cpp moved to top"
    (and (equal (file-name-sans-extension flymake-included-file-name)
				(file-name-sans-extension (file-name-nondirectory file-one)))
		 (not (equal file-one file-two))
	)
)  

(defcustom flymake-check-file-limit 8192
    "max number of chars to look at when checking possible master file"
	:group 'flymake
	:type 'integer
)

(defun flymake-check-patch-master-file-buffer(master-file-temp-buffer
											master-file-name patched-master-file-name
											source-file-name patched-source-file-name
											include-dirs)
    "check whether master-file-name is indeed a master file for source-file-name.
For .cpp master file this means it includes source-file-name (.h).
If yes, patch a copy of master-file-name to include patched-source-file-name instead of source-file-name.
Whenether a buffer for master-file-name exists, use it as a source instead of reading master file from disk"
    (let* ((found                     nil)
		   (regexp-list               (flymake-get-include-regexp source-file-name))
		   (regexp                    (format (nth 0 regexp-list) ; "[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\""
											  (file-name-nondirectory source-file-name)))
		   (path-idx                  (nth 1 regexp-list))
		   (name-idx                  (nth 2 regexp-list))
		   (inc-path                  nil)
		   (inc-name                  nil)
		   (search-limit              flymake-check-file-limit))
	    (save-excursion
            (unwind-protect
				(progn
					(set-buffer master-file-temp-buffer)
					(when (> search-limit (point-max))
					    (setq search-limit (point-max))
					)
					(flymake-log 3 "checking %s against regexp %s" master-file-name regexp)
					(goto-char (point-min))
					(while (and (< (point) search-limit) (re-search-forward regexp search-limit t))
					    (flymake-log 3 "found possible match for %s" (file-name-nondirectory source-file-name))
						(setq inc-path (match-string path-idx))
						(setq inc-name (match-string name-idx))
						(when (string= inc-name (file-name-nondirectory source-file-name))
						    (flymake-log 3 "inc-path=%s inc-name=%s" inc-path inc-name)
							(when (flymake-check-include source-file-name inc-path inc-name include-dirs)
								(setq found t)
								(replace-match (file-name-nondirectory patched-source-file-name) t nil nil 2)
							)
						)
						(forward-line 1)
					)
					(when found
					    (flymake-save-buffer-in-file (current-buffer) patched-master-file-name)
					)
				)
                ;+(flymake-log 3 "killing buffer %s" (buffer-name master-file-temp-buffer))			     
			    (kill-buffer master-file-temp-buffer)
		    )
		)
	    ;+(flymake-log 3 "check-patch master file %s: %s" master-file-name found)
		(when found
		    (flymake-log 2 "found master file %s" master-file-name)
		)
	    found
	)
)

(defun flymake-read-file-to-temp-buffer(file-name)
    "isert contents of file-name into newly created temp buffer"
	(let* ((temp-buffer (get-buffer-create (generate-new-buffer-name (concat "flymake:" (file-name-nondirectory file-name))))))
	    (save-excursion
		    (set-buffer temp-buffer)
			(insert-file-contents file-name)
		)
		temp-buffer
	)
)

(defun flymake-copy-buffer-to-temp-buffer(buffer)
    "copy contents of buffer into newly created temp buffer"
	(let ((contents     nil)
		  (temp-buffer  nil))
        (save-excursion
		    (set-buffer buffer)
			(setq contents (buffer-string))

			(setq temp-buffer (get-buffer-create (generate-new-buffer-name (concat "flymake:" (buffer-name buffer)))))
			(set-buffer temp-buffer)
			(insert contents)
		)
		temp-buffer
	)
)

(defun flymake-check-include(source-file-name inc-path inc-name include-dirs)
    "t if source-file-name is the one found via include dirs using inc-path and inc-name"
	(if (file-name-absolute-p inc-path)
		(flymake-same-files source-file-name (concat inc-path "/" inc-name))
	;else
	    (let* ((count      (length include-dirs))
			   (idx        0)
			   (file-name  nil)
			   (found      nil))
		    (while (and (not found) (< idx count))
			    (setq file-name (concat (file-name-directory source-file-name) "/" (nth idx include-dirs)))
				(if (> (length inc-path) 0)
					(setq file-name (concat file-name "/" inc-path))
				)
				(setq file-name (concat file-name "/" inc-name))
				(when (flymake-same-files source-file-name file-name)
					(setq found t)
				)
			    (setq idx (1+ idx))
			)
			found
		)
	)
)

(defun flymake-find-buffer-for-file(file-name)
    "buffer if there exists a buffer visiting file-name, nil otherwise"
	(let ((buffer-name (get-file-buffer file-name)))
	    (if buffer-name
			(get-buffer buffer-name)
		)
	)
)	

(defun flymake-create-master-file(source-file-name patched-source-file-name)
    "save source-file-name with a different name, find master file, patch it and save it to."
	(let* ((possible-master-files     (flymake-find-possible-master-files source-file-name flymake-master-file-dirs))
		   (master-file-count         (length possible-master-files))
		   (idx                       0)
		   (temp-buffer               nil)
		   (master-file-name          nil)
		   (patched-master-file-name  nil)
		   (found                     nil))

	    (while (and (not found) (< idx master-file-count))
		    (setq master-file-name (nth idx possible-master-files))
			(setq patched-master-file-name (flymake-make-output-file-name master-file-name "flymake_master"))
		    (if (flymake-find-buffer-for-file master-file-name)
				(setq temp-buffer (flymake-copy-buffer-to-temp-buffer (flymake-find-buffer-for-file master-file-name)))
			;else
			    (setq temp-buffer (flymake-read-file-to-temp-buffer master-file-name))
			)
		    (setq found
				  (flymake-check-patch-master-file-buffer
					   temp-buffer
				       master-file-name
					   patched-master-file-name
					   source-file-name
					   patched-source-file-name
					   (flymake-get-include-dirs
						    (flymake-find-buildfile (file-name-directory master-file-name) flymake-buildfile-dirs))))
		    (setq idx (1+ idx))
		)
		(if found
			(list master-file-name patched-master-file-name)
		;else
		    (progn
			    (flymake-log 3 "none of %d master file(s) checked includes %s" master-file-count
						   (file-name-nondirectory source-file-name))
		        nil
			)
		)
	)
)

(defun flymake-save-buffer-in-file(buffer file-name)
    (or buffer
	    (error "invalid buffer")
	)
	(save-excursion
	    (save-restriction
		    (set-buffer buffer)
		    (widen)
		    (write-region (point-min) (point-max) file-name nil 566)
		)
	)
	(flymake-log 3 "saved buffer %s in file %s" (buffer-name buffer) file-name)
)

(defun flymake-save-string-to-file(file-name data)
    "save string data to file file-name"
	(write-region data nil file-name nil 566)
)

(defun flymake-read-file-to-string(file-name)
    "read file contents and return them as a string"
	(with-temp-buffer
	    (insert-file-contents file-name)
		(buffer-substring (point-min) (point-max))
	)
)

(defun flymake-process-filter(process output)
    "flymake process filter: parse output, highlight err lines"
	(let* ((pid               (process-id process))
		   (source-buffer     (get-buffer (flymake-get-source-buffer-name pid))))

	    (flymake-log 3 "received %d byte(s) of output from process %d" (length output) pid)
	    (when source-buffer
		    (let ((master-file-name (flymake-get-buffer-master-file-name source-buffer))
				  (patched-master-file-name  (flymake-get-patched-master-file-name pid))
				  (source-file-name (buffer-file-name source-buffer))
				  (patched-source-file-name (flymake-get-patched-source-file-name pid)))

			(flymake-parse-output-and-residual source-buffer output
											 master-file-name patched-master-file-name
											 source-file-name patched-source-file-name)
		    )
		)
	)
)

(defun flymake-process-sentinel(process event)
   "Sentinel for syntax check buffers"
   (if (memq (process-status process) '(signal exit))
	   (let*((exit-status       (process-exit-status process))
			 (command           (process-command process))
			 (pid               (process-id process))
  		     (source-buffer     (get-buffer (flymake-get-source-buffer-name pid)))
			 (patched-master-file-name  (flymake-get-patched-master-file-name pid))
			 (patched-source-file-name (flymake-get-patched-source-file-name pid)))

		   (flymake-log 2 "process %d exited with code %d" pid exit-status)

		   (flymake-safe-delete-file patched-master-file-name)
		   (flymake-safe-delete-file patched-source-file-name)

		   (flymake-unreg-names pid)
		   (delete-process process)

		   (when source-buffer
			   (save-excursion
				   (set-buffer source-buffer)
				   (flymake-set-buffer-base-dir source-buffer nil)
				   (flymake-set-buffer-is-running source-buffer nil)

				   (flymake-parse-residual source-buffer
										 (flymake-get-buffer-master-file-name source-buffer) patched-master-file-name
										 (buffer-file-name source-buffer) patched-source-file-name)

				   (flymake-set-buffer-err-info source-buffer (flymake-get-buffer-new-err-info source-buffer))
				   (flymake-set-buffer-new-err-info source-buffer nil)

				   (flymake-set-buffer-err-info source-buffer (flymake-fix-line-numbers
															 (flymake-get-buffer-err-info source-buffer)
															 1
															 (flymake-count-lines source-buffer)))
				   (flymake-delete-own-overlays source-buffer)
				   (flymake-highlight-err-lines source-buffer (flymake-get-buffer-err-info source-buffer))

			       (let ((err-count   (flymake-get-err-count (flymake-get-buffer-err-info source-buffer) "e"))
					     (warn-count  (flymake-get-err-count (flymake-get-buffer-err-info source-buffer) "w")))

					   (flymake-log 2 "%s: %d error(s), %d warning(s) in %.2f second(s)"
								  (buffer-name source-buffer) err-count warn-count
								  (- (float-time) (flymake-get-buffer-check-start-time source-buffer)))
					   (flymake-set-buffer-check-start-time source-buffer nil)
					   (if (and (equal 0 err-count) (equal 0 warn-count))
						   (if (equal 0 exit-status)
							   (flymake-report-status source-buffer "" "") ; PASSED
						   ;else
							   (if (not (flymake-get-buffer-check-was-interrupted source-buffer))
							       (flymake-report-fatal-status (current-buffer) "CFGERR"
								       (format "Configuration error has occured while running %s" command))
							   ;else
								   (flymake-report-status source-buffer nil "") ; "STOPPED"
							   )
						   )
					   ;else
						   (flymake-report-status source-buffer (format "%d/%d" err-count warn-count) "")
					   )
			       )
			   )
		   )
	    )
    ) 
)


(defun flymake-merge-residual(lines residual)
    (if residual
		(cons (concat residual (car lines)) (cdr lines))
	;else
	    lines
	)
)

(defun flymake-parse-output-and-residual(source-buffer output
													 master-file-name patched-master-file-name
													 source-file-name patched-source-file-name)
    "split output into lines, merge in residual if necessary"
	(let* ((lines-and-residual  (flymake-split-output output))
		   (lines               (nth 0 lines-and-residual))
		   (residual            (nth 1 lines-and-residual)))

	    (save-excursion
		    (set-buffer source-buffer)
			(setq lines (flymake-merge-residual lines (flymake-get-buffer-output-residual source-buffer)))
			(flymake-set-buffer-output-residual source-buffer residual)
			(flymake-set-buffer-new-err-info source-buffer (flymake-parse-err-lines
														  (flymake-get-buffer-new-err-info source-buffer)
														  lines
														  (flymake-get-buffer-base-dir source-buffer)
														  master-file-name patched-master-file-name
														  source-file-name patched-source-file-name))
		)
	)
)	

(defun flymake-parse-residual(source-buffer master-file-name patched-master-file-name
										  source-file-name patched-source-file-name)
    "parse residual if it's non empty"
    (save-excursion
        (set-buffer source-buffer)
	    (when (flymake-get-buffer-output-residual source-buffer)
			(flymake-set-buffer-new-err-info source-buffer (flymake-parse-err-lines
														   (flymake-get-buffer-new-err-info source-buffer)
														   (list (flymake-get-buffer output-residual(source-buffer)))
														   (flymake-get-buffer-base-dir source-buffer)
														   master-file-name patched-master-file-name
														   source-file-name patched-source-file-name))
		    (flymake-set-buffer-output-residual source-buffer nil)
		)
	)
)

(defvar flymake-err-info nil
    "sorted list of line numbers and lists of err info in the form (file, err-text)."
)
(make-variable-buffer-local 'flymake-err-info)
(defun flymake-get-buffer-err-info(buffer)
    (flymake-get-buffer-var buffer 'flymake-err-info)
)
(defun flymake-set-buffer-err-info(buffer err-info)
    (flymake-set-buffer-var buffer 'flymake-err-info err-info)
)
(defun flymake-er-make-er(line-no line-err-info-list)
    (list line-no line-err-info-list)
)
(defun flymake-er-get-line(err-info)
    (nth 0 err-info)
)
(defun flymake-er-get-line-err-info-list(err-info)
    (nth 1 err-info)
)

(defvar flymake-new-err-info nil
    "the same as flymake -err-info, effective when a syntax check is in progress"
)
(make-variable-buffer-local 'flymake-new-err-info)
(defun flymake-get-buffer-new-err-info(buffer)
    (flymake-get-buffer-var buffer 'flymake-new-err-info)
)
(defun flymake-set-buffer-new-err-info(buffer new-err-info)
    (flymake-set-buffer-var buffer 'flymake-new-err-info new-err-info)
)

;; getters/setters for line-err-info: (file, line, type, text).
(defun flymake-ler-make-ler(file line type text &optional full-file)
    (list file line type text full-file)
)
(defun flymake-ler-get-file(line-err-info)
    (nth 0 line-err-info)
)
(defun flymake-ler-get-line(line-err-info)
    (nth 1 line-err-info)
)
(defun flymake-ler-get-type(line-err-info)
    (nth 2 line-err-info)
)
(defun flymake-ler-get-text(line-err-info)
    (nth 3 line-err-info)
)
(defun flymake-ler-get-full-file(line-err-info)
    (nth 4 line-err-info)
)
(defun flymake-ler-set-file(line-err-info file)
    (flymake-ler-make-ler file
						(flymake-ler-get-line line-err-info)
						(flymake-ler-get-type line-err-info)
						(flymake-ler-get-text line-err-info)
						(flymake-ler-get-full-file line-err-info))
)
(defun flymake-ler-set-full-file(line-err-info full-file)
    (flymake-ler-make-ler (flymake-ler-get-file line-err-info)
						(flymake-ler-get-line line-err-info)
						(flymake-ler-get-type line-err-info)
						(flymake-ler-get-text line-err-info)
						full-file)	
)
(defun flymake-ler-set-line(line-err-info line)
    (flymake-ler-make-ler (flymake-ler-get-file line-err-info)
						line
						(flymake-ler-get-type line-err-info)
						(flymake-ler-get-text line-err-info)
						(flymake-ler-get-full-file line-err-info))	
)

(defun flymake-get-line-err-count(line-err-info-list type)
    "return number of errors of specified type - e or w"
	(let* ((idx        0)
		   (count      (length line-err-info-list))
		   (err-count  0))

	    (while (< idx count)
		    (when (equal type (flymake-ler-get-type (nth idx line-err-info-list)))
			    (setq err-count (1+ err-count))
			)
		    (setq idx (1+ idx))
		)
		err-count
	)
)

(defun flymake-get-err-count(err-info-list type)
    "return number of errors of specified type for the err-info-list"
	(let* ((idx        0)
		   (count      (length err-info-list))
		   (err-count  0))
        (while (< idx count)
		    (setq err-count (+ err-count (flymake-get-line-err-count (nth 1 (nth idx err-info-list)) type)))
		    (setq idx (1+ idx))
		)
		err-count
	)
)  

(defun flymake-fix-line-numbers(err-info-list min-line max-line)
    "replace line-numbers < min-line with min-line and > max-line with max-line - as some compilers might report line number outside the file being compiled"
	(let* ((count     (length err-info-list))
		   (err-info  nil)
		   (line      0))
	    (while (> count 0)
		    (setq err-info (nth (1- count) err-info-list))
			(setq line (flymake-er-get-line err-info))
			(when (or (< line min-line) (> line max-line))
				(setq line (if (< line min-line) min-line max-line))
				(setq err-info-list (set-at err-info-list (1- count)
											(flymake-er-make-er line
															  (flymake-er-get-line-err-info-list err-info))))
			)
		    (setq count (1- count))
		)
	)
	err-info-list
)

(defun flymake-highlight-err-lines(buffer err-info-list)
    "highlight err-lines in buffer using info from err-info-list"
	(save-excursion
	    (set-buffer buffer)
		(let* ((idx    0)
			   (count  (length err-info-list)))
			(while (< idx count)
				(flymake-highlight-line (car (nth idx err-info-list)) (nth 1 (nth idx err-info-list)))
				(setq idx (1+ idx))
			)
		)
	)
)

(defun flymake-overlay-p(ov)
     "Determine whether overlay was created by flymake"
     (and (overlayp ov) (overlay-get ov 'flymake-overlay))
)

(defun flymake-make-overlay(beg end tooltip-text face mouse-face)
    "Allocate a flymake overlay in range beg end"
	(when (not (flymake-region-has-flymake-overlays beg end))
		(let ((ov (make-overlay beg end nil t t)))
			(overlay-put ov 'face           face)
			(overlay-put ov 'mouse-face     mouse-face)
			(overlay-put ov 'help-echo      tooltip-text)
			(overlay-put ov 'flymake-overlay  t)
			(overlay-put ov 'priority 100)
			;+(flymake-log 3 "created overlay %s" ov)
			ov
		)
		(flymake-log 3 "created and overlay at (%d-%d)" beg end)
	)
)

(defun flymake-delete-own-overlays(buffer)
    "Delete all flymake overlays in buffer"
	(save-excursion
	    (set-buffer buffer)
		(let ((ov (overlays-in (point-min) (point-max))))
			(while (consp ov)
				(when (flymake-overlay-p (car ov))
					(delete-overlay (car ov))
					;+(flymake-log 3 "deleted overlay %s" ov)
				)
				(setq ov (cdr ov))
			)
		)
	)
)

(defun flymake-region-has-flymake-overlays(beg end)
    "t if specified regions has at least one flymake overlay, nil otrherwise"
	(let ((ov                  (overlays-in beg end))
		  (has-flymake-overlays  nil))
		(while (consp ov)
			(when (flymake-overlay-p (car ov))
			    (setq has-flymake-overlays t)
			)
			(setq ov (cdr ov))
		)
	)
)

(defface flymake-errline-face
;+   '((((class color)) (:foreground "OrangeRed" :bold t :underline t))
;+   '((((class color)) (:underline "OrangeRed"))
   '((((class color)) (:background "LightPink"))  
     (t (:bold t)))
   "Face used for marking error lines"
    :group 'flymake
)

(defface flymake-warnline-face
   '((((class color)) (:background "LightBlue2"))
     (t (:bold t)))
   "Face used for marking warning lines"
    :group 'flymake
)


(defun flymake-highlight-line(line-no line-err-info-list)
    "highlight line line-no in current buffer, perhaps use text from line-err-info-list to enhance highlighting"
	(goto-line line-no)
	(let* ((line-beg (line-beginning-position))
		   (line-end (line-end-position))
		   (beg      line-beg)
		   (end      line-end)
		   (tooltip-text (flymake-ler-get-text (nth 0 line-err-info-list)))
		   (face     nil))

	    (goto-char line-beg)
	    (while (looking-at "[ \t]")
		    (forward-char)
		)
		
		(setq beg (point))

		(goto-char line-end)
	    (while (and (looking-at "[ \t\r\n]") (> (point) 1))
		    (backward-char)
		)
		
		(setq end (1+ (point)))

		(when (<= end beg)
		    (setq beg line-beg)
			(setq end line-end)
		)
		(when (= end beg)
		    (goto-char end)
			(forward-line)
			(setq end (point))
		)
		(if (> (flymake-get-line-err-count line-err-info-list "e") 0)
			(setq face 'flymake-errline-face)
		;else
		    (setq face 'flymake-warnline-face)
		)
        (flymake-make-overlay beg end tooltip-text face nil)
	)
)

(defun flymake-parse-err-lines(err-info-list lines base-dir
										   master-file-name patched-master-file-name
										   source-file-name patched-source-file-name)
    "parse err lines, store info in err-info-list"
	(let* ((count              (length lines))
		   (idx                0)
		   (line-err-info      nil)
		   (real-file-name     nil))

	    (while (< idx count)
		    (setq line-err-info (flymake-parse-line (nth idx lines)))
			(when line-err-info
			    (setq real-file-name (flymake-get-real-file-name (flymake-ler-get-file line-err-info) base-dir
															   master-file-name patched-master-file-name 
															   source-file-name patched-source-file-name))
				(setq line-err-info (flymake-ler-set-full-file line-err-info real-file-name))

 			    (if (flymake-same-files real-file-name source-file-name)
				    (setq line-err-info (flymake-ler-set-file line-err-info nil))
				;else
				    (setq line-err-info (flymake-ler-set-file line-err-info (file-name-nondirectory real-file-name)))
				)

				(setq err-info-list (flymake-add-err-info err-info-list line-err-info))
			)
			(flymake-log 3 "parsed '%s', %s line-err-info" (nth idx lines) (if line-err-info "got" "no"))
			(setq idx (1+ idx))
		)
		err-info-list
	)
)

(defun flymake-split-output(output)
    "split output into lines, return last one as residual if it does not end with newline char. Returns ((lines) residual)"
	(when (and output (> (length output) 0))
        (let* ((lines (split-string output "[\n\r]+"))
			   (complete (equal "\n" (char-to-string (aref output (1- (length output))))))
	    	   (residual nil))
		    (when (not complete)
				(setq residual (car (last lines)))
			    (setq lines (butlast lines))
			)
 	        (list lines residual)
		)
	)
)

(eval-when-compile (require 'compile))
(defvar flymake-err-line-patterns  ; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
    (append
	 '(
    ; MS Visual C++ 6.0									   
	   ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \: \\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
	    1 3 nil 4)
	; jikes 
	   ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[0-9]+\:[0-9]+\:[0-9]+\: \\(\\(Error\\|Warning\\|Caution\\):[ \t\n]*\\(.+\\)\\)"
	    1 3 nil 4)
    ; MS midl
       ("midl[ ]*:[ ]*\\(command line error .*\\)"
		nil nil nil 1)
	  )
	 compilation-error-regexp-alist)
    "patterns for matching error/warning lines, (regexp file-idx line-idx err-text-idx)"
)
;(defcustom flymake-err-line-patterns 
;  '(
;    ; MS Visual C++ 6.0									   
;    ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \: \\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
;	    1 3 4)
;	; jikes 
;	("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[0-9]+\:[0-9]+\:[0-9]+\: \\(\\(Error\\|Warning\\|Caution\\):[ \t\n]*\\(.+\\)\\)"
;	    1 3 4))
;    "patterns for matching error/warning lines, (regexp file-idx line-idx err-text-idx)"
;	:group 'flymake
;	:type '(repeat (string number number number))
;)

(defun flymake-parse-line(line)
    "parse line to see whether it's an error of warning, return it's components or nil for no match"
	(let ((raw-file-name nil)
		  (line-no 0)
		  (err-type "e")
		  (err-text nil)
		  (count (length flymake-err-line-patterns))
		  (idx   0)
		  (matched nil))
		(while (and (< idx count) (not matched))
		    (when (string-match (car (nth idx flymake-err-line-patterns)) line)
			    (let* ((file-idx (nth 1 (nth idx flymake-err-line-patterns)))
					   (line-idx (nth 2 (nth idx flymake-err-line-patterns))))
				  
				    (setq raw-file-name (if file-idx (match-string file-idx line) nil))
				    (setq line-no       (if line-idx (string-to-int (match-string line-idx line)) 0))
				    (setq err-text      (if (> (length (nth idx flymake-err-line-patterns)) 4)
											(match-string (nth 4 (nth idx flymake-err-line-patterns)) line)
										  (flymake-patch-err-text (substring line (match-end 0)))))
					(or err-text (setq err-text "<no error text>"))
    				(if (and err-text (string-match "^warning" err-text))
		    			(setq err-type "w")
			    	)
				    (flymake-log 3 "parse line: file-idx=%s line-idx=%s file=%s line=%s text=%s" file-idx line-idx
								 raw-file-name line-no err-text)
				    (setq matched t)
				)
			)
			(setq idx (1+ idx))
  	    )
		(if matched
		   (flymake-ler-make-ler raw-file-name line-no err-type err-text)
		; else
		   ()
		)   
	)
)

(defun flymake-find-err-info(err-info-list line-no)
    "find (line-err-info-list pos) for specified line-no"
	(if err-info-list
		(let* ((line-err-info-list  nil)
			   (pos       0)
			   (count     (length err-info-list)))

			(while (and (< pos count) (< (car (nth pos err-info-list)) line-no))
				(setq pos (1+ pos))
			)
			(when (and (< pos count) (equal (car (nth pos err-info-list)) line-no))
			    (setq line-err-info-list (flymake-er-get-line-err-info-list (nth pos err-info-list)))
			)
			(list line-err-info-list pos)
		)
	;else
	    '(nil 0)
	)
)
	
(defun flymake-line-err-info-is-less-or-equal(line-one line-two)
    (or (string< (flymake-ler-get-type line-one) (flymake-ler-get-type line-two))
		(and (string= (flymake-ler-get-type line-one) (flymake-ler-get-type line-two))
			 (not (flymake-ler-get-file line-one)) (flymake-ler-get-file line-two)
		)
		(and (string= (flymake-ler-get-type line-one) (flymake-ler-get-type line-two))
			 (or (and      (flymake-ler-get-file line-one)       (flymake-ler-get-file line-two))
				 (and (not (flymake-ler-get-file line-one)) (not (flymake-ler-get-file line-two)))
			 )
		)
	)
)

(defun flymake-add-line-err-info(line-err-info-list line-err-info)
    "insert new err info favoring sorting: err-type e/w, filename nil/non-nill"
	(if (not line-err-info-list)
	    (list line-err-info)
    ;else
		(let* ((count  (length line-err-info-list))
			   (idx    0))
		    (while (and (< idx count) (flymake-line-err-info-is-less-or-equal (nth idx line-err-info-list) line-err-info))
			    (setq idx (1+ idx))
			)
			(cond ((equal 0     idx)    (setq line-err-info-list (cons line-err-info line-err-info-list)))
				  (t                    (setq line-err-info-list (ins-after line-err-info-list (1- idx) line-err-info)))
			)
			line-err-info-list
		)
	)
)

(defun flymake-add-err-info(err-info-list line-err-info)
    "add error info (file line type text) to err info list preserving sort order"
	(let* ((count               (length err-info-list))
		   (line-no             (if (flymake-ler-get-file line-err-info) 1 (flymake-ler-get-line line-err-info)))
		   (info-and-pos        (flymake-find-err-info err-info-list line-no))
		   (exists              (car info-and-pos))
		   (pos                 (nth 1 info-and-pos))
		   (line-err-info-list  nil)
		   (err-info            nil))

	    (if exists
			(setq line-err-info-list (flymake-er-get-line-err-info-list (car (nthcdr pos err-info-list))))
		)
		(setq line-err-info-list (flymake-add-line-err-info line-err-info-list line-err-info))

		(setq err-info (flymake-er-make-er line-no line-err-info-list))
		(cond (exists             (setq err-info-list (set-at err-info-list pos err-info)))
		      ((equal 0 pos)      (setq err-info-list (cons err-info err-info-list)))
			  (t                  (setq err-info-list (ins-after err-info-list (1- pos) err-info))) 
		)
		err-info-list
	)
)

(defun flymake-get-project-include-dirs-imp(basedir)
    "include dirs for the project current file belongs to"
	(if (flymake-get-project-include-dirs-from-cache basedir)
		(progn
		    (flymake-get-project-include-dirs-from-cache basedir)
		)
	;else
		(let* ((command-line  (concat "make -C\"" basedir "\" DUMPVARS=INCLUDE_DIRS dumpvars"))
			   (output        (shell-command-to-string command-line))
			   (lines         (split-string output "\n"))
			   (count         (length lines))
			   (idx           0)
			   (inc-dirs      nil))
			(while (and (< idx count) (not (string-match "^INCLUDE_DIRS=.*" (nth idx lines))))
			   (setq idx (1+ idx))
			)
			(when (< idx count)
				(let* ((inc-lines  (split-string (nth idx lines) " *-I"))
					   (inc-count  (length inc-lines)))
					(while (> inc-count 0)
						(when (not (string-match "^INCLUDE_DIRS=.*" (nth (1- inc-count) inc-lines)))
							(setq inc-dirs (cons (replace-regexp-in-string "\"" "" (nth (1- inc-count) inc-lines)) inc-dirs))
						)
						(setq inc-count (1- inc-count))
					)
				)
			)
		    (flymake-add-project-include-dirs-to-cache basedir inc-dirs)
			inc-dirs
		)
	)
)

(defcustom flymake-get-project-include-dirs-function 'flymake-get-project-include-dirs-imp
    "function used to get project inc dirs, one paramater: basedir name"
	:group 'flymake
	:type 'function
)

(defun flymake-get-project-include-dirs(basedir)
    (funcall flymake-get-project-include-dirs-function basedir)
)

(defun flymake-get-system-include-dirs()
    "system include dirs - from the 'INCLUDE' env setting"
	(split-string (getenv "INCLUDE") path-separator)
)

(defvar flymake-project-include-dirs-cache (makehash 'equal))
(defun flymake-get-project-include-dirs-from-cache(base-dir)
    (gethash base-dir flymake-project-include-dirs-cache)
)
(defun flymake-add-project-include-dirs-to-cache(base-dir include-dirs)
    (puthash base-dir include-dirs flymake-project-include-dirs-cache)
)
(defun flymake-clear-project-include-dirs-cache()
    (clrhash flymake-project-include-dirs-cache)
)

(defun flymake-get-include-dirs(base-dir)
    "dirs to use when resolving local filenames"
	(let* ((include-dirs (append (flymake-get-project-include-dirs base-dir) (flymake-get-system-include-dirs))))
	    include-dirs
	)
)
	
(defun flymake-get-real-file-name(file-name-from-err-msg base-dir
												   master-file-name patched-master-file-name
												   source-file-name patched-source-file-name)
    "return (short-name, full-name). Names are real, not patched"
	(let* ((real-name         nil))
	    (when (equal 0 (length file-name-from-err-msg))
		    (setq file-name-from-err-msg source-file-name)
		)
        (if (file-name-absolute-p file-name-from-err-msg)
			(setq real-name file-name-from-err-msg)
		;else
			(setq real-name (concat base-dir "/" file-name-from-err-msg))
		)

		(if (flymake-same-files real-name patched-master-file-name)
			(setq real-name (concat (file-name-directory real-name) (file-name-nondirectory master-file-name)))
		;else
		    (if (flymake-same-files real-name patched-source-file-name)
				(setq real-name (concat (file-name-directory real-name) (file-name-nondirectory source-file-name)))
			)
		)
		real-name
	)
)

(defun flymake-find-file(rel-file-name include-dirs)
    "iterate through include-dirs, return first 'include-dir/rel-file-name' that exists, or just rel-file-name if not"
	(let* ((count          (length include-dirs))
		   (idx            0)
		   (found          nil)
		   (full-file-name rel-file-name))

	    (while (and (not found) (< idx count))
		    (let* ((dir (nth idx include-dirs)))
			    (setq full-file-name  (concat dir "/" rel-file-name))
			    (when (file-exists-p full-file-name)
				    (setq done t)
			    )
			)
			(setq idx (1+ idx))
		)
		(if found
		    full-file-name
		;else
		    rel-file-name
	    )
	)
)
	
(defun flymake-restore-formatting(source-buffer)
    "Remove any formatting made by flymake"
)

(defun flymake-get-program-dir(buffer)
    "dir to start profram in"
	(unless (bufferp buffer)
	    (error "invlid buffer")
	)
	(save-excursion
	    (set-buffer buffer)
		default-directory
	)
)

(defun flymake-safe-delete-file(file-name)
    (when (and file-name (file-exists-p file-name))
	    (delete-file file-name)
		(flymake-log 1 "deleted file %s" file-name)
	)
)

(defcustom flymake-compilation-prevents-syntax-check t
    "if non-nil, syntax check won't be started in case compilation is running"
	:group 'flymake
	:type 'boolean
)

(defun flymake-start-syntax-check(buffer)
    "start syntax checking for buffer"
    (unless (bufferp buffer)
		(error "expected a buffer")
    )
	(save-excursion
	    (flymake-clear-buildfile-cache)
		(flymake-clear-project-include-dirs-cache)
		(flymake-set-buffer-check-was-interrupted buffer nil)
	    (set-buffer buffer)
		(when (and (not (flymake-get-buffer-is-running buffer))
				   (flymake-can-syntax-check-file (buffer-file-name buffer)))
		    (if (or (not flymake-compilation-prevents-syntax-check)
					(not (flymake-compilation-is-running))) ;+ (flymake-rep-ort-status buffer "COMP")
				(let* ((source-file-name  (buffer-file-name buffer))
					   (patched-source-file-name  (flymake-make-output-file-name source-file-name "flymake"))
					   (master-file-name          source-file-name)
					   (patched-master-file-name  nil)
					   (patched                   nil)
					   (mode                      (flymake-can-syntax-check-file source-file-name)))

					(cond
					     ((equal 2 mode)
						 (progn
							 (setq patched (flymake-create-master-file
										 source-file-name patched-source-file-name))
							 (when patched
							     (setq master-file-name (nth 0 patched))
							     (setq patched-master-file-name (nth 1 patched))
								 (flymake-save-buffer-in-file buffer patched-source-file-name)
							 )
						 ))

						 ((equal 3 mode)
					     (progn
						     (flymake-log 0 "saving %s" (buffer-name (current-buffer)))
						     (save-buffer) 
							 (setq patched-source-file-name nil)
						 ))

						 (t
						  (flymake-save-buffer-in-file buffer patched-source-file-name))
					)
					(if (and (equal 2 mode) (not patched))
						(progn
							(flymake-log 1 "cannot find master file for %s" source-file-name)
							(flymake-set-buffer-last-change-time buffer nil)
							(flymake-report-status buffer "!" "") ; NOMASTER
						)
					;else
					    (let* ((base-dir  (flymake-find-buildfile (file-name-directory master-file-name) flymake-buildfile-dirs)))
			            (if (not base-dir)
				            (progn
							    (flymake-safe-delete-file patched-master-file-name)
								(flymake-safe-delete-file patched-source-file-name)
					            (flymake-log 1 "no buildfile for %s" source-file-name)
							    (flymake-set-buffer-last-change-time buffer nil)
					            (flymake-report-fatal-status buffer "NOMK"
								    (format "No buildfile found for %s" source-file-name))
				            )
						;else  
						    (progn
							    (flymake-start-syntax-check-process buffer base-dir
																  master-file-name patched-master-file-name
																  source-file-name patched-source-file-name)
						        )
					        )
					    )
					)
			    )
		    )
   	    )
    )
)

(defun flymake-get-file-to-compile(patched-master-file-name patched-source-file-name source-file-name)
    (if patched-master-file-name
		patched-master-file-name
	;else
	    (if patched-source-file-name
			patched-source-file-name
		;else
		    source-file-name
		)
    )
)

(defun flymake-start-syntax-check-process(buffer base-dir
											   master-file-name patched-master-file-name
											   source-file-name patched-source-file-name)
    "start syntax check-process"

	(let* ((process          nil)
		   (file-to-compile  (flymake-get-file-to-compile patched-master-file-name
														  patched-source-file-name source-file-name))
		   (program-name     (flymake-get-program-name))
		   (program-args     (flymake-get-program-args file-to-compile base-dir)))
        (condition-case err
			(progn
				(setq process (get-process (apply 'start-process
												 "flymake-proc"
												 nil
												 program-name
												 program-args)))
				(set-process-sentinel process 'flymake-process-sentinel)
				(set-process-filter process 'flymake-process-filter)

				(flymake-reg-names(process-id process) (buffer-name buffer) patched-master-file-name patched-source-file-name)
				(flymake-set-buffer-base-dir buffer base-dir)
				(flymake-set-buffer-master-file-name buffer master-file-name)
				(flymake-set-buffer-is-running buffer t)
				(flymake-set-buffer-last-change-time buffer nil)
				(flymake-set-buffer-check-start-time buffer (float-time))

				(flymake-report-status buffer nil "*")
				(flymake-log 2 "started process %d, command=%s, dir=%s"
						   (process-id process) (process-command process) default-directory)
				process
			)
 	        (error
			    (let ((err-str (format "Failed to launch syntax check process '%s' with args %s: %s"
							 program-name program-args (error-message-string err))))
				    (flymake-log 0 err-str)
				    (flymake-safe-delete-file patched-master-file-name)
					(flymake-safe-delete-file patched-source-file-name)
					(flymake-set-buffer-last-change-time buffer nil)
					(flymake-report-fatal-status buffer "PROCERR" err-str)
		        )
	 	    )
	    )
	)
)

(defun flymake-kill-process(pid &optional rest)
    "kill process pid"
	(signal-process pid 9)
	(let* ((buffer-name (flymake-get-source-buffer-name	pid)))
	    (when (and buffer-name (get-buffer buffer-name))
	        (flymake-set-buffer-check-was-interrupted (get-buffer buffer-name) t)
		)
    )
	(flymake-log 1 "killed process %d" pid)
)

(defun flymake-stop-all-syntax-checks()
    "kill all syntax check processes"
	(interactive)
	(let ((pids  (copy-hash-table flymake-pid-to-names)))
	    (maphash 'flymake-kill-process pids)
	)
)

(defun flymake-compilation-is-running()
   (and (boundp 'compilation-in-progress)
		compilation-in-progress)
)

(defun flymake-compile()
    "kill all flymake syntax checks, start compilation"
    (interactive)
    (flymake-stop-all-syntax-checks)
    (call-interactively 'compile)
)

(defvar flymake-is-running nil
  "t if flymake syntax check process is running for the current buffer"
)
(make-variable-buffer-local 'flymake-is-running)
(defun flymake-get-buffer-is-running(buffer)
    (flymake-get-buffer-var buffer 'flymake-is-running)
)
(defun flymake-set-buffer-is-running(buffer is-running)
    (flymake-set-buffer-var buffer 'flymake-is-running is-running)
)  

(defvar flymake-timer nil
    "timer for starting syntax checks"
)
(make-variable-buffer-local 'flymake-timer)
(defun flymake-get-buffer-timer(buffer)
    (flymake-get-buffer-var buffer 'flymake-timer)
)
(defun flymake-set-buffer-timer(buffer timer)
    (flymake-set-buffer-var buffer 'flymake-timer timer)
)

(defvar flymake-last-change-time nil
    "time of last buffer change"
)
(make-variable-buffer-local 'flymake-last-change-time)
(defun flymake-get-buffer-last-change-time(buffer)
    (flymake-get-buffer-var buffer 'flymake-last-change-time)
)
(defun flymake-set-buffer-last-change-time(buffer change-time)
    (flymake-set-buffer-var buffer 'flymake-last-change-time change-time)
)

(defvar flymake-check-start-time nil
    "time at which syntax check was started")
(make-variable-buffer-local 'flymake-check-start-time)
(defun flymake-get-buffer-check-start-time(buffer)
    (flymake-get-buffer-var buffer 'flymake-check-start-time)
)
(defun flymake-set-buffer-check-start-time(buffer check-start-time)
    (flymake-set-buffer-var buffer 'flymake-check-start-time check-start-time)
)

(defvar flymake-check-was-interrupted nil
    "t if syntax check was killed by flymake-compile"
)
(make-variable-buffer-local 'flymake-check-was-interrupted)
(defun flymake-get-buffer-check-was-interrupted(buffer)
    (flymake-get-buffer-var buffer 'flymake-check-was-interrupted)
)
(defun flymake-set-buffer-check-was-interrupted(buffer interrupted)
    (flymake-set-buffer-var buffer 'flymake-check-was-interrupted interrupted)
)

(defcustom flymake-no-changes-timeout 0.5
    "time to wait after last change before starting compilation"
	:group 'flymake
	:type 'number
)

(defun flymake-on-timer-event(buffer)
    "start a syntax check for buffer if necessary"
	;+(flymake-log 3 "timer: running=%s, time=%s, cur-time=%s" (flymake-get-buffer-is-running buffer) (flymake-get-buffer-last-change-time buffer) (float-time))

	(when (and (bufferp buffer) (not (flymake-get-buffer-is-running buffer)))
	    (save-excursion
		    (set-buffer buffer)
			(when (and (flymake-get-buffer-last-change-time buffer)
					   (> (float-time) (+ flymake-no-changes-timeout (flymake-get-buffer-last-change-time buffer))))
			    ;+(flymake-set-buffer-last-change-time buffer nil)
			    (flymake-log 3 "starting syntax check as more than 1 second passed since last change")
			    (flymake-start-syntax-check buffer)
			)
		)
	)
)

(defun flymake-start-syntax-check-for-current-buffer()
    "run flymake-start-syntax-check for current buffer if it isn't already running"
    (interactive)
	(flymake-start-syntax-check (current-buffer))
)

(defun flymake-current-line-no()
    "return number of current line in current buffer"
	(interactive)
	(let ((beg  (point-min))
		  (end  (if (= (point) (point-max)) (point) (1+ (point)))))
	    (count-lines beg end)
	)
)

(defun flymake-get-line-count(buffer)
    "return number of lines in buffer"
	(unless (bufferp buffer)
	    (error "invalid buffer")
	)
	(save-excursion
	    (set-buffer buffer)
	    (count-lines (point-min) (point-max))
	)
)

(defun flymake-count-lines(buffer)
    "return number of lines in buffer"
	(save-excursion
	    (set-buffer buffer)
		(count-lines (point-min) (point-max))
	)
)

(defun flymake-current-row()
    "return current riw in current frame"
    (+ (car (cdr (window-edges)))
		 (count-lines (window-start) (point)))
)

(defun flymake-get-point-pixel-pos()
    "return point position in pixels: (x, y)"
	(let ((mouse-pos  (mouse-position))
		  (pixel-pos  nil)
		  (ret        nil))
	    (if (car (cdr mouse-pos))
		    (progn
    	        (set-mouse-position (selected-frame) (current-column) (flymake-current-row))
	            (setq pixel-pos (mouse-pixel-position))
	            (set-mouse-position (car mouse-pos) (car (cdr mouse-pos)) (cdr (cdr mouse-pos)))
		        (setq ret (list (car (cdr pixel-pos)) (cdr (cdr pixel-pos))))
             )
		;else
			(progn
			    (setq ret '(0 0))
			)
		)
		(flymake-log 3 "mouse pos is %s" ret)
		ret
	)
)

(defun flymake-display-err-menu-for-current-line()
   "Display a menu with errors/warnings for current line if it has errors and/or warnings"
   (interactive)
   (let* ((line-no             (flymake-current-line-no))
		  (line-err-info-list  (nth 0 (flymake-find-err-info (flymake-get-buffer-err-info (current-buffer)) line-no)))
		  (menu                (flymake-make-err-menu line-no line-err-info-list))
		  (choice              nil)
		  (mouse-pos           (flymake-get-point-pixel-pos))
		  (moved-mouse-pos     (list (car mouse-pos) (+ 10 (car (cdr mouse-pos)))))
		  (menu-pos            (list (flymake-get-point-pixel-pos) (selected-window))))
	   (if menu
		   (progn
		       (setq choice (x-popup-menu menu-pos menu))
			   (when (and choice (flymake-ler-get-file (nth choice line-err-info-list)))
				   (flymake-log 3 "choice=%d file=%s line=%d"
								choice
								(flymake-ler-get-full-file (nth choice line-err-info-list))
								(flymake-ler-get-line (nth choice line-err-info-list)))
				   (flymake-goto-file-and-line (flymake-ler-get-full-file (nth choice line-err-info-list))
											 (flymake-ler-get-line (nth choice line-err-info-list)))
			   )
		   )
	   ;else
		   (flymake-log 1 "no errors for line %d" line-no)
	   )
   )
)

(defun flymake-make-err-menu(line-no line-err-info-list)
   "Make a menu with errors/warnings from line-err-info"
   (let* ((menu  nil))
	   (when line-err-info-list
		   (let* ((count           (length line-err-info-list))
				  (menu-item-text  nil))
			   (while (> count 0)
				    (setq menu-item-text (flymake-ler-get-text (nth (1- count) line-err-info-list)))
					(if (flymake-ler-get-file (nth (1- count) line-err-info-list))
						(setq menu-item-text (concat menu-item-text
													 " - "
													 (flymake-ler-get-file (nth (1- count) line-err-info-list))
													 "("
													 (format "%d" (flymake-ler-get-line (nth (1- count) line-err-info-list)))
													 ")"))
					)

				    (setq menu (cons (cons menu-item-text (1- count)) menu))
				    (setq count (1- count)) 
			   )
			   (setq menu (cons "" menu))
			   (setq menu (list (format "Line %d: %d error(s), %d warning(s)" line-no
										(flymake-get-line-err-count line-err-info-list "e")
										(flymake-get-line-err-count line-err-info-list "w"))
								 menu))
			   (flymake-log 3 "created menu with %d item(s)" (length line-err-info-list))
		   )
	   )
	   menu
   )
)

(defun flymake-goto-file-and-line(file line)
    "try to get buffer for file and goto line line in it"
	(if (not (file-exists-p file))
		(flymake-log 1 "file %s does not exists" file)
	;else
	    (progn 
	        (find-file file)
		    (goto-line line)
		)
	)
)
;; flymake minor mode declarations

(defvar flymake-mode nil)
(make-variable-buffer-local 'flymake-mode) 

(defvar flymake-mode-line nil
    ""
)
(make-variable-buffer-local 'flymake-mode-line)
(defun flymake-get-buffer-mode-line(buffer)
    (flymake-get-buffer-var buffer 'flymake-mode-line)
)
(defun flymake-set-buffer-mode-line(buffer mode-line-string)
    (flymake-set-buffer-var buffer 'flymake-mode-line mode-line-string)
)

(defvar flymake-mode-line-e-w nil)
(make-variable-buffer-local 'flymake-mode-line-e-w)
(defun flymake-get-buffer-mode-line-e-w(buffer)
    (flymake-get-buffer-var buffer 'flymake-mode-line-e-w)
)	
(defun flymake-set-buffer-mode-line-e-w(buffer e-w)
    (flymake-set-buffer-var buffer 'flymake-mode-line-e-w e-w)
)	

(defvar flymake-mode-line-status nil)
(make-variable-buffer-local 'flymake-mode-line-status)
(defun flymake-get-buffer-mode-line-status(buffer)
    (flymake-get-buffer-var buffer 'flymake-mode-line-status)
)	
(defun flymake-set-buffer-mode-line-status(buffer status)
    (flymake-set-buffer-var buffer 'flymake-mode-line-status status)
)	

(defun flymake-report-status(buffer e-w &optional status)
    "show status in the mode line"
	(when (bufferp buffer)
		(save-excursion
			(set-buffer buffer)
			(when e-w
			    (flymake-set-buffer-mode-line-e-w buffer e-w)
			)
			(when status
			    (flymake-set-buffer-mode-line-status buffer status)
			)
			(let* ((mode-line " Flymake"))
			    (when (> (length (flymake-get-buffer-mode-line-e-w buffer)) 0)
				    (setq mode-line (concat mode-line ":"  (flymake-get-buffer-mode-line-e-w buffer)))
				)
				(setq mode-line (concat mode-line (flymake-get-buffer-mode-line-status buffer)))
				(flymake-set-buffer-mode-line buffer mode-line)
			    (force-mode-line-update)
			)
		)
	)
)

(defun flymake-display-warning(warning)
    "display a warning to the user"
	(message-box warning)
)

(defun flymake-report-fatal-status(buffer status warning)
    "display a warning and switch flymake mode OFF"
	(flymake-display-warning (format "Flymake: %s. Flymake will be switched OFF" warning))
	(save-excursion
	    (set-buffer buffer)
		(flymake-mode 0)
		(flymake-log 1 "switched OFF Flymake mode for buffer %s due to fatal status %s, warning %s"
					 (buffer-name buffer) status warning)
	)
)

(defun flymake-mode(&optional arg)
    "toggle flymake-mode"
	(interactive)
	(let ((old-flymake-mode flymake-mode))

	    (setq turn-on
			(if (null arg)
				(not flymake-mode)
			;else
				(> (prefix-numeric-value arg) 0))
		)

		(if turn-on
			(if (flymake-can-syntax-check-file (buffer-file-name))
			    (flymake-mode-on)
			;else
			    (flymake-log 2 "flymake cannot check syntax in buffer %s" (buffer-name))
			)
		;else
			(flymake-mode-off)
		)
		(force-mode-line-update)
	)
)

;;;###autoload
(unless (assq 'flymake-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(flymake-mode flymake-mode-line) minor-mode-alist))
)

;;;###autoload
(defun flymake-mode-on()
    "turn flymake mode on"
	(when (not flymake-mode)
		(make-local-variable 'after-change-functions)
		(setq after-change-functions (cons 'flymake-after-change-function after-change-functions))
		;+(add-hook 'after-save-hook 'flymake-after-save-hook)
		;+(add-hook 'find-file-hooks 'flymake-find-file-hook)

		(flymake-report-status (current-buffer) "" "")

		(flymake-set-buffer-timer (current-buffer) (run-at-time t 1 'flymake-on-timer-event (current-buffer)))
		
		(setq flymake-mode t)
		(flymake-log 1 "flymake mode turned ON for buffer %s" (buffer-name (current-buffer)))
		(when flymake-start-syntax-check-on-find-file
		    (flymake-start-syntax-check-for-current-buffer) ; will be started by on-load hook
	    )
	)
)

;;;###autoload
(defun flymake-mode-off()
    "turn flymake mode off"
	(when flymake-mode
		(setq after-change-functions (delq 'flymake-after-change-function  after-change-functions))
		;+(remove-hook 'after-save-hook (function flymake-after-save-hook) t)
		;+(remove-hook 'find-file-hooks (function flymake-find-file-hook) t)
		
		(flymake-delete-own-overlays (current-buffer))

		(when (flymake-get-buffer-timer (current-buffer))
		    (cancel-timer (flymake-get-buffer-timer (current-buffer)))
		    (flymake-set-buffer-timer (current-buffer) nil)
		)

		(flymake-set-buffer-is-running (current-buffer) nil)
		
		(setq flymake-mode nil)
	    (flymake-log 1 "flymake mode turned OFF for buffer %s" (buffer-name (current-buffer)))
	)
)

(defcustom flymake-start-syntax-check-on-newline t
    "start syntax check if newline char was added/removed from the buffer"
	:group 'flymake
	:type 'boolean
)

(defun flymake-after-change-function(start stop len)
    "Start syntax check for current buffer if it isn't already running"
	;+(flymake-log 0 "setting change time to %s" (float-time))
	(let((new-text (buffer-substring start stop)))
	    (when (and flymake-start-syntax-check-on-newline (equal new-text "\n"))
			(flymake-log 3 "starting syntax check as new-line has been seen")
			(flymake-start-syntax-check-for-current-buffer)
		)
		(flymake-set-buffer-last-change-time (current-buffer) (float-time))
	)
)

(defun flymake-after-save-hook()
    (if (local-variable-p 'flymake-mode) ; (???) other way to determine whether flymake is active in buffer being saved?
        (flymake-log 3 "not starting syntax check")
		;+(flymake-log 3 "starting syntax check as buffer was saved")
        ;+(flymake-start-syn-tax-check-for-current-buffer) ; cannot start check if mode 3 (to temp copies) is active - (???)
    )
)

(defcustom flymake-start-syntax-check-on-find-file t
    "statr syntax check on find file"
	:group 'flymake
	:type 'boolean
)

(defun flymake-find-file-hook()
    (when flymake-start-syntax-check-on-find-file
        (flymake-log 3 "starting syntax check on file open")
	    (flymake-start-syntax-check-for-current-buffer)
    )		
)

(defun flymake-get-first-err-line-no(err-info-list)
    "return first line-no with error"
	(when err-info-list
	    (flymake-er-get-line (car err-info-list))
	)
)

(defun flymake-get-last-err-line-no(err-info-list)
    "return last line-no with error"
	(when err-info-list
	    (flymake-er-get-line (nth (1- (length err-info-list)) err-info-list))
	)
)

(defun flymake-get-next-err-line-no(err-info-list line-no)
    "return next line with erroe"
	(when err-info-list
	    (let* ((count  (length err-info-list))
			   (idx    0))
		    (while (and (< idx count) (>= line-no (flymake-er-get-line (nth idx err-info-list))))
			    (setq idx (1+ idx))
			)
			(if (< idx count)
				(flymake-er-get-line (nth idx err-info-list))
			)
		)
	)
)

(defun flymake-get-prev-err-line-no(err-info-list line-no)
    "return prev line with error"
	(when err-info-list
	    (let* ((count (length err-info-list)))
		    (while (and (> count 0) (<= line-no (flymake-er-get-line (nth (1- count) err-info-list))))
			    (setq count (1- count))
			)
		    (if (> count 0)
		        (flymake-er-get-line (nth (1- count) err-info-list))
	 	    )
		)
	)
)

(defun flymake-skip-whitespace()
    "move forward until nonwhitespace is reached"
	(while (looking-at "[ \t]")
        (forward-char)
    )
)

(defun flymake-goto-line(line-no)
    "goto-line, then skip whitespace"
	(goto-line line-no)
	(flymake-skip-whitespace)
)

(defun flymake-goto-next-error()
    "go to next error in err ring"
	(interactive)
	(let ((line-no (flymake-get-next-err-line-no (flymake-get-buffer-err-info (current-buffer)) (flymake-current-line-no))))
	    (when (not line-no)
		    (setq line-no (flymake-get-first-err-line-no (flymake-get-buffer-err-info (current-buffer))))
			(flymake-log 1 "passed end of file")			
		)
		(if line-no
		    (flymake-goto-line line-no)
		;else
		    (flymake-log 1 "no errors in current buffer")
		)
	)
)  

(defun flymake-goto-prev-error()
    "go to prev error in err ring"
	(interactive)
	(let ((line-no (flymake-get-prev-err-line-no (flymake-get-buffer-err-info (current-buffer)) (flymake-current-line-no))))
	    (when (not line-no)
		    (setq line-no (flymake-get-last-err-line-no (flymake-get-buffer-err-info (current-buffer))))
			(flymake-log 1 "passed beginning of file")
		)
		(if line-no
		    (flymake-goto-line line-no)
		;else
 		    (flymake-log 1 "no errors in current buffer")
		)
	)
)  

(defun flymake-patch-err-text(string)
    (if (string-match "^[\n\t :0-9]*\\(.*\\)$" string)
		(match-string 1 string)
	;else
	    string
	)
)

(provide 'flymake)
