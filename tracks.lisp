(in-package :music-player)

; A track class as well as functions to manipulate them.

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

(defun get-track-streamer (track)
  (make-mp3-streamer (file-path track)))

(defun lo-play-track (mixer track)
  "Low level function to tell mixalot to *immediately* play this track. Don't call directly, please..."
  (mixer-add-streamer mixer (get-track-streamer track)))

(defun format-track (track stream)
  (format stream "'~a' by '~a' in '~a'" (title track) (artist track) (album track)))
