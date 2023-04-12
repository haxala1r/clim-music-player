(in-package :music-player)
(defclass track ()
    ((file-path
      :initform (error "Track file required")
      :initarg :file-path
      :accessor file-path)
     (album
      :initarg :album
      :accessor album)
     (artist
      :initarg :artist
      :accessor artist)
     (title
      :initarg :title
      :accessor title)
     (year
      :initarg :year
      :accessor year)))

(defun make-track (p)
  (let* ((real-path (probe-file p))
	 (tmp-st (if real-path
		     (restart-case
			 (make-mp3-streamer (namestring real-path))
		       (skip-track () nil))
		     nil))
	 (tags (if tmp-st
		   (mpg123:get-tags-from-handle (mixalot-mp3::mpg123-handle tmp-st))
		   nil)))
    (when tmp-st
      (mp3-streamer-release-resources tmp-st)
      (make-instance 'track
		     :file-path (print (namestring real-path))
		     :title (or (getf tags :title) "NO TITLE")
		     :album (or (getf tags :album) "NO ALBUM")
		     :artist (or (getf tags :artist) "UNKNOWN")
		     :year (or (getf tags :year) "-")))))


(defun print-track (track stream)
  (format stream "'~a' by '~a' in '~a'" (title track) (artist track) (album track)))

(define-presentation-type track-presentation ()
  :inherit-from 'track)

(defun app-display (frame pane)
  (loop for i in (track-list frame)
	do (with-output-as-presentation (pane i 'track-presentation)
	     (print-track i pane)
	     (terpri pane))))

(define-application-frame app-frame ()
  ((track-list
    :initform nil
    :initarg :track-list
    :accessor track-list)
   (mixer
    :initarg :mixer
    :reader mixer))
  (:pointer-documentation t)
  (:panes
   (app :application :display-function 'app-display
	:height 400 :width 400)
   (int :interactor :height 200 :width 400))
  (:layouts
   (default (vertically ()
	      app int))))

(define-app-frame-command (com-add-track :name t) ((file-path 'string))
  (let ((track (make-track file-path)))
    (if track
	(push track (track-list *application-frame*)))))

(define-app-frame-command (com-play-track :name t) ((track 'track-presentation))
  (mixer-add-streamer (mixer *application-frame*) (make-mp3-streamer (file-path track))))

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

(define-app-frame-command (com-add-directory :name t) ((dir-path 'string))
  "Adds every mp3 file under a given directory (non-recursively)"
  (loop for i in (get-mp3s-under dir-path)
	for track = (make-track i)
	if track do (push track (track-list *application-frame*))))

(defvar *main-mixer* nil)
(defun run-app ()
  "Sets up sound and runs "
  (unless *main-mixer*
    (setf *main-mixer* (create-mixer)))
  (run-frame-top-level (make-instance 'app-frame
				      :track-list nil
				      :mixer *main-mixer*)))
