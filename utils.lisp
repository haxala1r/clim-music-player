(in-package :music-player)

(defun directory-form-p (path)
  (and path
       (not (pathname-name path))
       (not (pathname-type path))))

(defun to-directory-form (path)
  "Turn a pathname to directory form. (for cross-implementation compatability)"
  (let ((path (pathname path)))
    (if (or
	 (eql (pathname-name path) :wild)
	 (eql (pathname-type path) :wild))
	(error "Cannot convert wild paths"))
    (if (directory-form-p path)
	path
	(make-pathname :directory (append (or (pathname-directory path) (list :relative))
					  (list (pathname-name path)))
		       :name nil
		       :type nil
		       :defaults path))))

(defun get-mp3s-under (dir)
  (let ((dir (to-directory-form dir)))
    (directory (make-pathname :defaults dir
			      :name :wild
			      :type "mp3"))))
