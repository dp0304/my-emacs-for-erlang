;;; flymake-utest.el -- unit test for flymake.el

;; Copyright (C) 2003 Pavel Kobiakov

;; Author: Pavel Kobiakov <pk_at_work@yahoo.com>

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

(require 'flymake)

;;;; helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flymaketest-make-output-buffer(&optional name)
    (or name
	(setq name "*flymaketest*"))
    (let ((buffer (get-buffer-create name))
          (output-file-name (flymake-make-output-file-name "flymaketest")))
        (set-buffer buffer)
        (write-file output-file-name)
	    buffer
    )
)

(defun assert(value)
    "raise an error if value is nil"
    (unless value
        (error "assertion failed, value is nil")
	)
)

(defun assert-message(value message &rest args)
    "raise an error and print a message if value is nil"
    (unless value
	  (apply 'error message args)
	)
)

(defun assert-equal(value-one value-two)
    "raise an error if value-one != value-two"
    (unless (equal value-one value-two)
        (error "assertion failed, expected %s, got %s" value-one value-two)
	)
)

;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-flymake-save-source-buffer-name()
    (flymake-reg-names 123 "saved-buf" "master-file" "source-file")
    (assert (equal "saved-buf" (flymake-get-source-buffer-name 123)))
    (assert (equal "master-file" (flymake-get-patched-master-file-name 123)))
    (assert (equal "source-file" (flymake-get-patched-source-file-name 123)))
	(flymake-unreg-names 123)
	(assert (equal nil (flymake-get-source-buffer-name 123)))
    (assert (equal nil (flymake-get-patched-master-file-name 123)))	
)

(defun test-flymake-make-output-file-name()
    (let* ((test-file-names '( ("test.cpp"  "test_flymake.cpp")
			       ("test"      "test_flymake") ))
	   (count (length test-file-names)))
        (while (> count 0)
	    (assert (equal (nth 1 (nth (1- count) test-file-names))
			   (flymake-make-output-file-name (car (nth (1- count) test-file-names)))))
	    (setq count (1- count))
	)
    )
)

(defun test-flymake-can-syntax-check-file()
    (let* ((test-data '(("Makefile" nil nil)
		        ("Test.cpp" 1 nil)
				("Test.c"  1 nil)
		        ("Test.h" 2 (".+\\.cpp$" ".+\\.c$"))
		        ("Test.java" 3 nil)))
	   (count      (length test-data)))
        (while (> count 0)
		    (flymake-log 0 "checking %s" (car (nth (1- count) test-data)))
	        (assert-equal  (nth 1 (nth (1- count) test-data))
						   (flymake-can-syntax-check-file (car (nth (1- count) test-data))))
	        (assert-equal (nth 2 (nth (1- count) test-data))
						  (flymake-get-master-file-masks (car (nth (1- count) test-data))))
  	        (setq count (1- count))
	    )
    )
)

(defun test-flymake-save-buffer-in-output-file()
  (save-excursion
	(let ((buffer (flymaketest-make-output-buffer))
	      (saved-file)
	      (saved-buffer))

	  (set-buffer buffer)
	  (unwind-protect
		  (progn
			(erase-buffer)
			(insert "test line 1\n")
			(insert "test line 2\n")
	    
		    (setq saved-file (make-temp-file "flymake"))
			(flymake-save-buffer-in-file (current-buffer) saved-file)
		    (find-file saved-file)
		    (setq saved-buffer (current-buffer))
			(assert (equal "test line 1\ntest line 2\n" (buffer-string)))
			)
		(delete-file (buffer-file-name buffer))
		(set-buffer buffer)
		(set-buffer-modified-p nil)
		(kill-buffer buffer)
		(and saved-buffer (kill-buffer saved-buffer))
		(and saved-file (delete-file saved-file))
	    )
	  )
    )
  )

(defun test-flymake-start-syntax-check()
    (let* ((buffer        (flymaketest-make-output-buffer))
	   (started-proc  (flymake-start-syntax-check buffer)))
        (kill-buffer buffer)
        (assert (not started-proc))
        ;-(assert (>= (process-id started-proc) 0))	
    )
)

(defun test-flymake-parse-line()
    (let*((test-data '( ("a.cpp(2) : fatal error C1004: unexpected end of file found"
						     "a.cpp"            2 "e" "fatal error C1004: unexpected end of file found" nil)

						("E:\buff\01\a.cpp(2) : fatal error C1004: unexpected end of file found"
						     "E:\buff\01\a.cpp" 2 "e" "fatal error C1004: unexpected end of file found" nil)

						("a.cpp(3) : error C2065: 'a' : undeclared identifier"
						     "a.cpp"            3 "e" "error C2065: 'a' : undeclared identifier" nil)

						("a.cpp(4) : warning C4700: local variable 'a' used without having been initialized"
						     "a.cpp"            4 "w" "warning C4700: local variable 'a' used without having been initialized" nil)

					    ("this line has nothing to do with err-info"
						     )))
		  (count (length test-data)))

	    (while (> count 0)
		    (let* ( (line               (car (nth (1- count) test-data)))
				    (err-info           (flymake-parse-line line))
					(expected-err-info  (cdr (nth (1- count) test-data))))
  			    (assert-equal expected-err-info err-info)
			)
		    (setq count (1- count))    
		)
    )
)

(defun test-flymake-save-var-in-buffer()
    "test buffer-local vars"
    (setq flymake-output-residual "123")
	(assert-equal flymake-output-residual "123")
	(setq flymake-output-residual nil)
	(assert-equal nil flymake-output-residual)
)

(defun test-flymake-split-output()
    (let* ((test-data '( ("line1\nline2\n"      ("line1" "line2") nil)
 	                     ("line3\nline4\nlin"   ("line3" "line4") "lin")
						 (""                    () nil )
						 (nil                   () nil )))
		   (count (length test-data)))
	    (while (> count 0)
		    (let ((result (flymake-split-output (car (nth (1- count) test-data)))))
			     (assert-equal (nth 1 (nth (1- count) test-data)) (nth 0 result))
			     (assert-equal (nth 2 (nth (1- count) test-data)) (nth 1 result))
			)
			(setq count (1- count))
		)
	)
)

(defun test-flymake-find-err-info()
    (let* ((err-info-cont '((4  ((nil   "e" "error0")))
							(5  (("a.h" "e" "error")))
						    (10 (("a.h" "e" "error2")))))

		   (test-data '((3  (nil 0))
						(5  ((("a.h" "e" "error")) 1))
						(7  (nil 2))
						(12 (nil 3))))

		   (count (length test-data)))

	      (while (> count 0)
			   (flymake-log 0 "line %s" (car (nth (1- count) test-data)))
			   (assert-equal '(nil 0) (flymake-find-err-info nil (car (nth (1- count) test-data))))
			   (flymake-log 0 "line %s #2" (car (nth (1- count) test-data)))			   
			   (assert-equal (nth 1 (nth (1- count) test-data))
							 (flymake-find-err-info err-info-cont (car (nth (1- count) test-data))))			   
               (setq count (1- count))
		  )
	)
)

(defun test-flymake-add-line-err-info()
    (let* ((test-data '(  ((("a.cpp" 10 "e" "error")) ("a.cpp" 10 "e" "error")   (("a.cpp" 10 "e" "error") ("a.cpp" 10 "e" "error")))
						  ((("a.cpp" 11 "e" "error")) ("a.cpp" 11 "w" "warn")   (("a.cpp" 11 "e" "error") ("a.cpp" 11 "w" "warn")))
						  (((nil 12 "e" "error"))     ("a.cpp" 12 "e" "error2") ((nil 12 "e" "error") ("a.cpp" 12 "e" "error2")))
						  ((("a.cpp" 14 "e" "error")) (nil 14 "w" "warn")       (("a.cpp" 14 "e" "error") (nil 14 "w" "warn")))
						  ((("a.cpp" 15 "e" "error")) (nil 15 "e" "err")        ((nil 15 "e" "err") ("a.cpp" 15 "e" "error")))
						  ((("a.cpp" 10 "e" "error")) ("a.cpp" 10 "e" "error2") (("a.cpp" 10 "e" "error") ("a.cpp" 10 "e" "error2")))
						  ((("a.cpp" 10 "e" "error2")) ("a.cpp" 10 "e" "error") (("a.cpp" 10 "e" "error2") ("a.cpp" 10 "e" "error")))						  
					   ))
		   (count (length test-data)))
	    (while (> count 0)
		    (flymake-log 0 "line %d: %s + %s" count (car   (nth (1- count) test-data)) (nth 1 (nth (1- count) test-data)))
		    (assert-equal (nth 2 (nth (1- count) test-data))
						  (flymake-add-line-err-info (car   (nth (1- count) test-data))
												   (nth 1 (nth (1- count) test-data))))
			(setq count (1- count))
		)
	)
)


(defun test-flymake-add-err-info()
    (let* ((test-data '( ( ()                         ; before
						   (nil 10 "e" "error")       ; err-info
						   ((10 ((nil 10 "e" "error")))) ; after
						 )
						 ( ((10 ((nil 10 "e" "error"))))
						   (nil 9 "e" "error2")
						   ((9 ((nil 9 "e" "error2"))) (10 ((nil 10 "e" "error"))))
						 )
						 ( ((10 ((nil 10 "e" "error"))))
						   (nil 10 "e" "error2")
						   ((10 ((nil 10 "e" "error") (nil 10 "e" "error2"))))
						 )
					   ))
		   (count (length test-data)))
	    (while (> count 0)
		    (let* ((before    (nth 0 (nth (1- count) test-data)))
				   (err-info  (nth 1 (nth (1- count) test-data)))
				   (after     (nth 2 (nth (1- count) test-data))))
			    (assert-equal after (flymake-add-err-info before err-info))
			)
		    (setq count (1- count))
		)
    )
)

(defun test-flymake-merge-residual()
    (let* ((test-data '( (("ine 1" "line 2") "l"     ("line 1" "line 2"))
						 (("line 1")         nil     ("line 1"))
						 (()                "line 1" ("line 1"))
					   ))
		   (count (length test-data)))
         (while (> count 0)
		     (assert-equal (nth 2 (nth (1- count) test-data))
						   (flymake-merge-residual (nth 0 (nth (1- count) test-data)) (nth 1 (nth (1- count) test-data))))
			 (setq count (1- count))
		 )
	 )
)

(defun test-flymake-parse-err-lines()
    (let* ((test-data '(
						 ( ()                                                                   ; before
						   ("a.cpp(2) : fatal error C1004: unexpected end of file found")       ; lines
                           "d:/projects/a.cpp"                                                  ; master file name
						   "d:/projects/a.cpp"                                                  ; source-file-name
						   "d:/projects"                                                        ; base-dir
						   ((2 ((nil 2 "e" "fatal error C1004: unexpected end of file found" "d:/projects/a.cpp"))))  ; after
						 )
						 ( ()                                                                   ; before
						   ("d:/projects/a.cpp(2) : fatal error C1004: unexpected end of file found")       ; lines
						   "d:/projects/a.cpp"
						   "d:/projects/a.cpp"
						   "d:/projects"
						   ((2 ((nil 2 "e" "fatal error C1004: unexpected end of file found" "d:/projects/a.cpp"))))  ; after
						 )
						 ( ()                                                                  
						   ("b.cpp(2) : fatal error C1004: unexpected end of file found")
						   "d:/projects/a.cpp"
						   "d:/projects/a.cpp"
						   "d:/projects"
						   ((1 (("b.cpp" 2 "e" "fatal error C1004: unexpected end of file found" "d:/projects/b.cpp"))))
						 )
						 ( ()                                                                  
						   ("src/File.cpp(2) : fatal error C1004: unexpected end of file found")
						   "d:/projects/src/File.cpp"
						   "d:/projects/src/File.cpp"
						   "d:/projects"
						   ((2 ((nil 2 "e" "fatal error C1004: unexpected end of file found" "d:/projects/src/File.cpp"))))
						 )
						 ( ()                                                                  
						   ("File.h(2) : fatal error C1004: unexpected end of file found")
						   "d:/projects/src/File.cpp"
						   "d:/projects/File.h"
						   "d:/projects"
						   ((2 ((nil 2 "e" "fatal error C1004: unexpected end of file found" "d:/projects/File.h"))))
						 )
						 ( ()                                                                  
						   ("src/File_flymake_master.cpp(2) : fatal error C1004: unexpected end of file found")
						   "d:/projects/src/File.cpp"
						   "d:/projects/File.h"
						   "d:/projects"
						   ((1 (("File.cpp" 2 "e" "fatal error C1004: unexpected end of file found" "d:/projects/src/File.cpp"))))
						 )
						 ( ()                                                                  
						   ("src/AnotherFile_flymake.cpp(2) : fatal error C1004: unexpected end of file found")
						   "d:/projects/src/AnotherFile.cpp"
						   "d:/projects/src/AnotherFile.cpp"
						   "d:/projects"
						   ((2 ((nil 2 "e" "fatal error C1004: unexpected end of file found" "d:/projects/src/AnotherFile.cpp"))))
						 )
						 ( ()                                                                  
						   ()
						   ()
						   ()
						   ()
						   ()
						 )
					   ))
		   (count (length test-data)))
	    (while (> count 0)
		    (let* ((before            (nth 0 (nth (1- count) test-data)))
				   (lines             (nth 1 (nth (1- count) test-data)))
				   (master-file-name  (nth 2 (nth (1- count) test-data)))
				   (patched-master-file-name (flymake-make-output-file-name (if master-file-name master-file-name "") "flymake_master"))
				   (source-file-name  (nth 3 (nth (1- count) test-data)))
				   (patched-source-file-name (flymake-make-output-file-name (if source-file-name source-file-name "") "flymake"))
				   (base-dir          (nth 4 (nth (1- count) test-data)))
				   (after             (nth 5 (nth (1- count) test-data))))

                (flymake-log 0 "testing %d" (1- count))
			    (assert-equal after (flymake-parse-err-lines before lines base-dir
														   master-file-name patched-master-file-name
														   source-file-name patched-source-file-name))
			)
		    (setq count (1- count))
		)
    )
)

(test-flymake-parse-err-lines)

(defun test-flymake-get-real-name()
    (let* ((test-data'(
					   ("a.cpp"             "d:/projects" "d:/projects/a.cpp")
					   ("d:/projects/a.cpp" "d:/projects" "d:/projects/a.cpp")
					   ("d:/projects/b.cpp" "d:/projects" "d:/projects/b.cpp"))
					  )
		   (count (length test-data)))
	    (while (> count 0)
		    (flymake-log 0 "current file is %s" (nth 0 (nth (1- count) test-data)))
		    (assert-equal (nth 2 (nth (1- count) test-data))
						  (flymake-get-real-file-name
						      (nth 0 (nth (1- count) test-data))
							  (nth 1 (nth (1- count) test-data))
							  "d:/projects/a.cpp" "d:/projects/a_flymake_master.cpp"
							  "d:/projects/a.cpp" "d:/projects/a_flymake.cpp"))
							  
		    (setq count (1- count))
		)
	)
)

(defun test-flymake-get-line-err-count()
    (let* ((test-data '(
						 (((nil nil "e" "error"))                  "e" 1)
						 (((nil nil "e" "error"))                  "w" 0)
						 (((nil nil "e" "error") (nil nil "w" "warn")) "w" 1)
						 (()                                       "e" 0)
						 ))
		   (count (length test-data)))
        (while (> count 0)
		    (assert-equal (nth 2 (nth (1- count) test-data))
						  (flymake-get-line-err-count (nth 0 (nth (1- count) test-data))
											   (nth 1 (nth (1- count) test-data))))
		    (setq count (1- count))
		)
	)
)

(defun test-flymake-err-line-navigation()
    (let* ((err-info-cont '((4  ((nil   "e" "error0")))
							(5  (("a.h" "e" "error")))
						    (10 (("a.h" "w" "warning")))))

		   (test-data '(( 3    4  nil)
						( 5   10    4)
						( 7   10    5)
						(12  nil   10 )))

		   (count (length test-data)))

        (assert-equal nil (flymake-get-first-err-line-no nil))
		(assert-equal nil (flymake-get-last-err-line-no nil))

    	(assert-equal  4 (flymake-get-first-err-line-no err-info-cont))
		(assert-equal 10 (flymake-get-last-err-line-no err-info-cont))
		
	    (while (> count 0)
		    (assert-equal (nth 1 (nth (1- count) test-data))
						  (flymake-get-next-err-line-no err-info-cont (nth 0 (nth (1- count) test-data))))
		    (assert-equal (nth 2 (nth (1- count) test-data))
						  (flymake-get-prev-err-line-no err-info-cont (nth 0 (nth (1- count) test-data))))
			(setq count (1- count))
		)
	)
)

(defun test-flymake-same-files()
    (assert (flymake-same-files "d:/a" "d:/a"))
	(assert (flymake-same-files "d:/projects/sources/../a.cpp" "d:/projects/./a.cpp"))
	(assert (not (flymake-same-files "d:/projects/a.cpp" "d:/a.cpp")))
	(assert (flymake-same-files "d:/DOCUME~1/KOP~1.NOV/LOCALS~1/Temp//flymake/File.h"
							  "d:/DOCUME~1/KOP~1.NOV/LOCALS~1/Temp//flymake//./File.h"))
)

(defun test-flymake-getset-buffer-var()
    (flymake-set-buffer-var (current-buffer) 'foo 10)
	(assert-equal 10 (flymake-get-buffer-var (current-buffer) 'foo))
)

(defun test-flymake-line-err-info-accessors()
    (let* ((line-err-info '("a.cpp" 110 "e" "error")))
	    (assert-equal "a.cpp" (flymake-ler-get-file line-err-info))
		(assert-equal 110     (flymake-ler-get-line line-err-info))
		(assert-equal "e"     (flymake-ler-get-type line-err-info))
		(assert-equal "error" (flymake-ler-get-text line-err-info))

		(setq line-err-info (flymake-ler-set-file line-err-info nil))
		(assert-equal nil (flymake-ler-get-file line-err-info))

		(setq line-err-info (flymake-ler-set-line line-err-info 220))
		(assert-equal 220 (flymake-ler-get-line line-err-info))
	)
)

(defun test-flymake-make-err-menu()
)

(defun test-flymake-fix-path-name()
    (assert-equal "d:/projects" (flymake-fix-path-name "d:\\projects\\"))
    (assert-equal "d:/projects" (flymake-fix-path-name "d:/projects/"))
    (assert-equal "d:/projects" (flymake-fix-path-name "d:/projects"))
)

(defun test-flymake-get-common-prefix()
    (assert-equal "abc" (flymake-get-common-prefix "abc" "abcde"))
    (assert-equal "a"   (flymake-get-common-prefix "a" "a"))
    (assert-equal nil   (flymake-get-common-prefix "cba" "abc"))
)

(defun test-flymake-build-relative-path()
    (let* ((test-data  '( ("d:/projects/a" "d:/projects/b" "../b")
						  ("d:/c/a"        "d:/projects/b" "../../projects/b")
						  ("d:"            "d:/a"          "a")
						  ("d:/a"          "d:/a"          nil)
						  ("d:/a/"         "d:/a"          nil)
						  ("d:/a/b/c/b"    "d:/e"          "../../../../e")
						  ))
		   (count      (length test-data)))
	    (while (> count 0)
		    (assert-equal (nth 2 (nth (1- count) test-data))
						  (flymake-build-relative-path
						   (nth 0 (nth (1- count) test-data))
						   (nth 1 (nth (1- count) test-data))))
		    (setq count (1- count))
		)
		;(???) use condition to test for errors: d:/a + e:/a
	)
)

(defun test-flymake-fix-path-line-numbers()
    (assert-equal '(
					  (1 ((nil 0 "e" "error" "d:/a.cpp")))
					  (10 ((nil 10 "e" "error" "d:/a.cpp")))
					  (100 ((nil 101 "e" "error" "d:/a.cpp")))
				   )
				  (flymake-fix-line-numbers
				   '(
					 (0 ((nil 0 "e" "error" "d:/a.cpp")))
					 (10 ((nil 10 "e" "error" "d:/a.cpp")))
					 (101 ((nil 101 "e" "error" "d:/a.cpp")))
					 ) 1 100))
)

(defun test-flymake-save-and-read-to-string()
    (let* ((test-data "Some text\nMore text")
		   (temp-file-name (make-temp-file "test-flymake-save-and-read-to-string")))
	    (unwind-protect
			(progn
			    (delete-file temp-file-name)
				(flymake-save-string-to-file temp-file-name test-data)
				(assert-equal test-data (flymake-read-file-to-string temp-file-name))
			)
		    (delete-file temp-file-name)
		)
	)
)

(defun test-flymake-check-patch-master-file()
    (let* ((test-data  '(
						 ("#include \"File.h\"\n#include \"AnotherFile.h\""
						  t
						  "#include \"File_flymake.h\"\n#include \"AnotherFile.h\"")
						 ("#include \"File2.h\"\n#include \"AnotherFile.h\""
						  nil
						  nil))
						 )
		   (count      (length test-data))
		   (dir                (concat temporary-file-directory "/flymake"))
		   (include-dirs  '("." ".."))
		   (cpp-file-name      (concat dir "/File.cpp"))
		   (h-file-name        (concat dir "/File.h"))
		   (mod-h-file-name    (concat dir "/File_flymake.h"))
		   (mod-cpp-file-name  (concat dir "/File_flymake_master.cpp")))

	    (if (not (file-exists-p dir))
		   (make-directory dir)
		)
		
		; (???) add unwind-protect which deletes all temp files and dir

	    (while (> count 0)
		    (let* ((cpp-file-data      (nth 0 (nth (1- count) test-data)))
				   (mod-cpp-file-data  (nth 2 (nth (1- count) test-data)))
				   (exists             (nth 1 (nth (1- count) test-data)))
				   (cpp-file-buffer    nil)
				   (cpp-file-buffer-name nil)
				   (found                nil))

				(flymake-save-string-to-file cpp-file-name cpp-file-data)
				(setq cpp-file-buffer (flymake-read-file-to-temp-buffer cpp-file-name))
				(setq cpp-file-buffer-name (buffer-name cpp-file-buffer))

			    (setq found (flymake-check-patch-master-file-buffer
									cpp-file-buffer
									cpp-file-name mod-cpp-file-name
									h-file-name mod-h-file-name include-dirs))

				(assert (equal nil (get-buffer cpp-file-buffer-name)))
				(if (not exists)
					(assert-equal nil found)
				;else
				    (progn
						(assert-equal mod-cpp-file-data (flymake-read-file-to-string mod-cpp-file-name))
					)
				)
			    (if (file-exists-p cpp-file-name) (delete-file cpp-file-name))
				(if (file-exists-p mod-cpp-file-name) (delete-file mod-cpp-file-name))
			)
		    (setq count (1- count))
		)
		(delete-directory dir)
	)
)

;;;; test-suite
(defun flymake-test-suite()
    (test-flymake-can-syntax-check-file)
    (test-flymake-save-source-buffer-name)
    (test-flymake-make-output-file-name)
    (test-flymake-save-buffer-in-output-file)
    (test-flymake-start-syntax-check)
	(test-flymake-parse-line)
	(test-flymake-save-var-in-buffer)
	(test-flymake-split-output)
	(test-flymake-find-err-info)
	(test-flymake-add-line-err-info)
	(test-flymake-add-err-info)
	(test-flymake-merge-residual)
	(test-flymake-parse-err-lines)
	(test-flymake-get-real-name)
	(test-flymake-get-line-err-count)
	(test-flymake-err-line-navigation)
	(test-flymake-same-files)
	(test-flymake-getset-buffer-var)
	(test-flymake-line-err-info-accessors)
	(test-flymake-fix-path-name)
	(test-flymake-get-common-prefix)
	(test-flymake-build-relative-path)
	(test-flymake-fix-path-line-numbers)
	(test-flymake-save-and-read-to-string)
	(test-flymake-check-patch-master-file)
	(message "all tests successfully passed")
)

(flymake-test-suite)

(load "D:/projects/spp/Sources/Tools/VCAddIns/flymk/flymake.el")
(eval-buffer nil nil)

;;;; temporary code
(setq foo " #include \"../lwlog.h\"")
(string-match "[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*\\)\\(lwlog.h\\)\"" foo)
(match-string 2 foo)
(match-string 1 foo)
(replace-match "File_flymake.h" t nil foo 2)

(file-name-absolute-p "")

(if (string-match "t" nil) nil)
(setq foo (flymake-get-point-pixel-pos))
(setq foo (list (car foo) (+ 10 (car (cdr foo)))))
