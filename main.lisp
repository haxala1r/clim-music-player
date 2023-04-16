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
   ;(int :interactor :height 200 :width 400)
   )
  (:layouts
   (default (vertically ()
	      app))))

(defgeneric app-display-with-view (frame pane view))

(defun app-display (frame pane)
  (app-display-with-view frame pane (stream-default-view pane)))

(defmethod app-display-with-view (frame pane (view track-view))
  (loop for i in (track-list frame)
	do (with-output-as-presentation (pane i 'track-presentation)
	     (format-track i pane)
	     (terpri pane))))

(defmethod app-display-with-view (frame pane (view directory-view))
  (loop for i in (append
		  (list
		   (uiop:pathname-parent-directory-pathname (current-dir view))
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
  (if (uiop:directory-exists-p path)
      (setf (current-dir (dirview *application-frame*)) (uiop:truename* path))))

(defun add-track-from-file (path)
  "Adds a single file as track"
  (let ((track (make-track (namestring path))))
    (if track
	(push track (track-list *application-frame*)))))

(defun add-directory (path)
  "Adds every mp3 file under a given directory (non-recursively)"
  (loop for i in (get-mp3s-under path)
	do (add-track-from-file i)))

(define-app-frame-command (com-add-track :name t) ((path 'pathname-presentation))
  "Takes a pathname, adds it as a track if it's a file, adds all tracks under it if it's a directory"
  (if (uiop:directory-exists-p path)
      (add-directory path)
      (if (uiop:file-exists-p path)
	  (add-track-from-file path)
	  (error "File/directory not found"))))

(define-app-frame-command (com-play-track :name t) ((track 'track-presentation))
  (mixer-add-streamer (mixer *application-frame*) (make-mp3-streamer (file-path track))))

(define-app-frame-command (com-play-track-from-pathname :name t) ((track 'pathname-presentation))
  (mixer-add-streamer (mixer *application-frame*) (make-mp3-streamer (namestring (uiop:truename* track)))))

(define-app-frame-command (com-stop-all :name t) ()
  (mixer-remove-all-streamers (mixer *application-frame*)))

(define-gesture-name :primary :pointer-button-press (:left))
(define-gesture-name :add-track :pointer-button-press (:left :control))
(define-gesture-name :play-track :pointer-button-press (:left :meta))
(define-gesture-name :stop-all-music :pointer-button-press (:middle))

(define-presentation-to-command-translator change-directory
    (pathname-presentation com-change-directory app-frame :gesture :primary) (obj)
  "This translator makes it so that left-clicking on a path triggers a directory change."
  (list obj))

(define-presentation-to-command-translator play-track
    (track-presentation com-play-track app-frame :gesture :primary) (obj)
  "Plays a track from track presentation"
  (list obj))

(define-presentation-to-command-translator play-track-from-path
    (pathname-presentation com-play-track-from-pathname app-frame :gesture :play-track) (obj)
  "Plays a track from pathname presentation"
  (list obj))

(define-presentation-to-command-translator add-track
    (pathname-presentation com-add-track app-frame :gesture :add-track)
    (obj)
  (list obj))

(define-presentation-to-command-translator stop-all-music
    (blank-area com-stop-all app-frame :gesture :stop-all-music) (obj)
  nil)


(defvar *main-mixer* nil)
(defun run-app ()
  "Sets up sound and runs the app."
  (unless *main-mixer*
    ; Normally, we could just (let) this, but when testing it creates a mixer every time you call this 
    ; and that becomes a real big memory hog. To avoid, we just create one - in case run-app is called
    ; multiple times in one process (such as when developing and debugging)
    (setf *main-mixer* (create-mixer)))
  (run-frame-top-level (make-instance 'app-frame
				      :track-list nil
				      :mixer *main-mixer*)))
