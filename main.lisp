(in-package :music-player)

; to avoid undefined symbol/function/class/whatever errors, load in this order:
;  package.lisp -> utils.lisp -> tracks.lisp -> music.lisp -> main.lisp

(define-presentation-type track-presentation ()
  :inherit-from 'track)

(define-presentation-type pathname-presentation ()
  :inherit-from 'pathname)

(defclass directory-view (view)
  ((current-dir :initform (uiop/os:getcwd)
		:initarg :current-dir
		:accessor current-dir)))

(defclass track-view (view)
  ((name
    :initform "(No Name)"
    :initarg :name
    :accessor name)
   (track-list
    :initform nil
    :initarg :track-list
    :accessor track-list))) ; views a single tracklist/playlist
(define-presentation-type track-view-pres ()
  :inherit-from 'track-view) ; this is so that we can make track-views printable/clickable. see below.

; the playlists attribute holds track-view elements that are switched to when clicked.
; the current track view acts like a scratch buffer, you can add any track you want, then save it
; as a playlist.
(defclass playlist-view (view)
  ((playlists
    :initarg :playlists
    :initform nil
    :accessor playlists)))

(define-application-frame app-frame ()
  ((mixer
    :initarg :mixer
    :reader mixer)
   (track-scratch
    :initform (make-instance 'track-view)
    :accessor track-scratch)
   (dirview
    :initform (make-instance 'directory-view)
    :accessor dirview)
   (playlist-view
    :initform (make-instance 'playlist-view)
    :accessor playlist-view))
  (:pointer-documentation t)
  (:menu-bar t)
  (:panes
   (app :application :display-function 'app-display
		     :height 400 :width 600 :default-view (make-instance 'track-view))
   (scratch :application :display-function 'app-display
			 :height 200 :width 600 :default-view (make-instance 'track-view))
   (current :application :display-function 'currently-playing-display
	    :height 200 :width 600)
   (stop-button :push-button :activate-callback 'pause-callback
			     :label "Pause")
   )
  (:layouts
   (default (vertically ()
	      app
	      (horizontally ()
		scratch current)
	      (horizontally ()
		stop-button)))))

(defun currently-playing-display (frame pane)
  (declare (ignorable frame))
  (if (get-currently-playing)
      (describe (get-currently-playing) pane)))

(defun pause-callback (&rest rest)
  (declare (ignorable rest))
  (with-slots (mixer) *application-frame*
    (if (music-paused-p mixer)
	(unpause-music mixer)
	(pause-music mixer))))

(defgeneric app-display-with-view (frame pane view))

(defun app-display (frame pane)
  (app-display-with-view frame pane (stream-default-view pane)))

(defmethod app-display-with-view (frame pane (view track-view))
  (loop for i in (track-list (track-scratch frame))
	do (with-output-as-presentation (pane i 'track-presentation)
	     (format-track i pane)
	     (terpri pane))))

(defmethod app-display-with-view (frame pane (view playlist-view))
  (loop for i in (playlists view)
	for j upfrom 0
	do (with-output-as-presentation (pane i 'track-view-pres)
	     (format pane "~a. ~a~%" j (name i)))))

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


; Change views
(define-app-frame-command (com-directory-view :name t :menu t) ()
  (setf (stream-default-view *standard-output*)
	(dirview *application-frame*)))

(define-app-frame-command (com-track-view :name t) ()
  "This is unused now that 'track view' is it's own pane... to be removed later"
  (setf (stream-default-view *standard-output*)
	(track-scratch *application-frame*)))

(define-app-frame-command (com-playlists-view :name t :menu t) ()
  (setf (stream-default-view *standard-output*)
	(playlist-view *application-frame*)))

; Commands
(define-app-frame-command (com-change-directory :name t) ((path 'pathname-presentation))
  (if (uiop:directory-exists-p path)
      (setf (current-dir (dirview *application-frame*)) (uiop:truename* path))))

(defun add-track-from-file (path)
  "Adds a single file as track"
  (let ((track (make-track (namestring path))))
    (if track
	(push track (track-list (track-scratch *application-frame*))))))

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
  (setf *current-playlist* (track-list (track-scratch *application-frame*)))
  (setf *current-position* (position track (track-list (track-scratch *application-frame*))))
  (play-next (mixer *application-frame*)))

(define-app-frame-command (com-play-track-from-pathname :name t) ((track 'pathname-presentation))
  (mixer-add-streamer (mixer *application-frame*) (make-mp3-streamer (namestring (uiop:truename* track)))))

(define-app-frame-command (com-stop-all :name t) ()
  (cancel-all-music (mixer *application-frame*)))

(define-app-frame-command (com-pause :name t) ()
  (pause-music (mixer *application-frame*)))

(define-app-frame-command (com-unpause :name t) ()
  (unpause-music (mixer *application-frame*)))

(define-app-frame-command (com-switch-playlist :name t) ((track-view 'track-view-pres))
  (setf (track-scratch *application-frame*) track-view))

(define-app-frame-command (com-add-playlist :name t :menu t) ()
  (setf (name (track-scratch *application-frame*))
	(accepting-values ()
	  (accept 'string)))
  (push (track-scratch *application-frame*) (playlists (playlist-view *application-frame*))))

(define-app-frame-command (com-clear-scratch :name t :menu t) ()
  (setf (track-scratch *application-frame*) (make-instance 'track-view)))


; Gestures that map to the above commands
(define-gesture-name :primary :pointer-button-press (:left))
(define-gesture-name :add-track :pointer-button-press (:left :control))
(define-gesture-name :play-track :pointer-button-press (:left :meta))
(define-gesture-name :stop-all-music :pointer-button-press (:middle))
(define-gesture-name :switch-playlist :pointer-button-press (:left))

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
  "M-Clicking on a directory adds all mp3s under that directory, and clicking on an mp3
file adds that file to the track list."
  (list obj))

(define-presentation-to-command-translator stop-all-music
    (blank-area com-stop-all app-frame :gesture :stop-all-music) (obj)
  nil)

(define-presentation-to-command-translator switch-playlist
    (track-view-pres com-switch-playlist app-frame :gesture :switch-playlist) (obj)
  (list obj))

(defvar *main-mixer* nil)
(defun run-app ()
  "Sets up sound and runs the app."
  (unless *main-mixer*
    ; Normally, we could just (let) this, but when testing it creates a new mixer every time you call this 
    ; and that becomes a real big memory hog. To avoid that, we just create one - in case run-app is called
    ; multiple times in one process (such as when developing and debugging)
    (setf *main-mixer* (create-mixer)))
  (run-frame-top-level (make-instance 'app-frame
				      :track-list nil
				      :mixer *main-mixer*)))
