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


(defun format-track (track stream)
  (format stream "'~a' by '~a' in '~a'" (title track) (artist track) (album track)))

(defun format-pathname (name stream)
  "Prints the pathname's name and type only, to make things a little more human-readable."
  (if (directory-form-p name)
      (format stream "~a/" (first (last (pathname-directory name))))
      (if (pathname-type name)
	  (format stream "~a.~a" (pathname-name name) (pathname-type name))
	  (format stream "~a" (pathname-name name)))))

(define-presentation-type track-presentation ()
  :inherit-from 'track)

(define-presentation-type pathname-presentation ()
  :inherit-from 'pathname)

(defclass directory-view (view)
  ((current-dir :initform (uiop/os:getcwd)
		:initarg :current-dir
		:accessor current-dir)))

(defclass track-view (view)
  ())

(define-application-frame app-frame ()
  ((track-list
    :initform nil
    :initarg :track-list
    :accessor track-list)
   (mixer
    :initarg :mixer
    :reader mixer)
   (dirview
    :initform (make-instance 'directory-view)
    :accessor dirview))
  (:pointer-documentation t)
  (:menu-bar t)
  (:panes
   (app :application :display-function 'app-display
		     :height 400 :width 400 :default-view (make-instance 'track-view))
   (int :interactor :height 200 :width 400)
   )
  (:layouts
   (default (vertically ()
	      app int))))

(defgeneric app-display-with-view (frame pane view))

(defun app-display (frame pane)
  (app-display-with-view frame pane (stream-default-view pane)))

(defmethod app-display-with-view (frame pane (view track-view))
  (loop for i in (track-list frame)
	do (with-output-as-presentation (pane i 'track-presentation)
	     (print-track i pane)
	     (terpri pane))))

(defmethod app-display-with-view (frame pane (view directory-view))
  (loop for i in (append
		  (list
		   (uiop:truename* (merge-pathnames #p"../" (current-dir view)))
		   (current-dir view))
		  (directory (make-pathname :defaults (current-dir view)
					    :name :wild
					    :type :wild)))
	do (with-output-as-presentation (pane i 'pathname-presentation)
	     (princ i pane)
	     (terpri pane))))

(define-app-frame-command (com-directory-view :name t :menu t) ()
  (setf (stream-default-view *standard-output*)
	(dirview *application-frame*)))

(define-app-frame-command (com-track-view :name t :menu t) ()
  (setf (stream-default-view *standard-output*)
	(make-instance 'track-view)))

(define-app-frame-command (com-change-directory :name t) ((path 'pathname-presentation))
  (setf (current-dir (dirview *application-frame*)) path))

(define-app-frame-command (com-add-track :name t) ((file-path 'pathname-presentation))
  (let ((track (make-track (namestring file-path))))
    (if track
	(push track (track-list *application-frame*)))))

(define-app-frame-command (com-play-track :name t) ((track 'track-presentation))
  (mixer-add-streamer (mixer *application-frame*) (make-mp3-streamer (file-path track))))

(define-app-frame-command (com-add-directory :name t) ((dir-path 'pathname-presentation))
  "Adds every mp3 file under a given directory (non-recursively)"
  (loop for i in (get-mp3s-under dir-path)
	for track = (make-track i)
	if track do (push track (track-list *application-frame*))))

(define-app-frame-command (com-stop-all :name t) ()
  (mixer-remove-all-streamers (mixer *application-frame*)))

(defvar *main-mixer* nil)
(defun run-app ()
  "Sets up sound and runs "
  (unless *main-mixer*
    (setf *main-mixer* (create-mixer)))
  (run-frame-top-level (make-instance 'app-frame
				      :track-list nil
				      :mixer *main-mixer*)))
