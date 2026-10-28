;; fix pgplot postscript files to put bounding box
;; at start by replacing "%%BoundingBox: (atend)" with
;; the value found at end of file.  This is necessary to make
;; cropping easier for pdf files (and is generally good practice).

(in-package pgplot)

(defun fix-ps-bounding-box (psfile &key (error-on-fail t))
  (when (not (probe-file psfile))
    (error "fix-ps-bounding-box: file ~A not found" psfile))
  ;; find the bounding box line
  (block retblock
    (let ((bbox-line
	    (with-open-file (s psfile :direction :input)
	      (loop for line = (read-line s nil nil)
		    until (not line)
		    when (and (> (length line) 14)
			      (equalp (subseq line 0 14) "%%BoundingBox:")
			      (not (search "(atend)" line)))
		      do (return line)
		    finally
		       (if error-on-fail
			   (error "Failed to find %%BoundingBox in ~A" psfile)
			   (return-from retblock nil)))))
	  ;;
	  (tmpfile  (format nil "~A_TEMPORARY_BBOX" psfile)))
      ;; replace the %%BoundingBox: (atend) line with the real one
      ;; into a tmpfile
      (with-open-file (sin psfile :direction :input)
	(with-open-file (sout tmpfile :direction :output :if-exists :supersede)
	  (loop for line = (read-line sin nil nil)
		until (not line)
		do (if (and (> (length line) 14)
			    (equalp (subseq line 0 14) "%%BoundingBox:")
			    (search "(atend)" line))
		       (write-line bbox-line sout)
		       (write-line line sout)))))
      ;; move the tmpfile over the original ps file
      (when (probe-file tmpfile)
	(delete-file psfile)
	(rename-file tmpfile psfile)))
    (return-from retblock t)))
    
    

	
