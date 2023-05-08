(in-package :music-player)

; Here we're making a small change to mixalot's streamer-cleanup so that
; whenever one track is finished, it automatically plays the next in the list.
; AFAICS, this is impossible to accomplish otherwise (without some sort of timing mechanism,
; which I can't be bothered with...)

; Also some more business logic can be found here in the form of the functions for actually playing shit.
(defparameter *current-playlist* nil
  "Holds the current playlist.")
(defparameter *current-position* 0)
(defparameter *playing* nil)

(defmacro post-incf (expr &optional (n 1))
  (let ((var (gensym)))
    `(let ((,var ,expr))
       (incf ,expr ,n)
       ,var)))

(defun play-next (mixer)
  "Assumes *current-playlist* and *current-position* are set to sensible values beforehand..."
  (if (< *current-position* (length *current-playlist*))
      (progn
	(lo-play-track mixer (nth (incf *current-position*) *current-playlist*))
	(setf *playing* t))
      (progn
	(setf *current-playlist* nil)
	(setf *current-position* 0)
	(setf *playing* nil))))


(defmethod streamer-cleanup :after (stream mixer)
  "AFTER the actual streamer cleanup function is called (and the resources are freed), starts playing the next streamer on the list.
Note: This might be kind of problematic if there is more than one streamer currently on the mixer - as this will then be called multiple times and there aren't any locks or anything... "
  (if *playing*
      (play-next mixer)))

(defun cancel-all-music (mixer)
  "Deletes the entire to-play list, as well as the currently playing streamer."
  (setf *playing* nil)
  (setf *current-playlist* nil)
  (setf *current-position* 0)
  (mixer-remove-all-streamers mixer))

(defun skip-to-next (mixer)
  (mixer-remove-all-streamers mixer))

(defun skip-to-previous (mixer)
  (decf *current-position* 2)
  (mixer-remove-all-streamers mixer))

(defun get-currently-playing ()
  (unless (null *current-playlist*)
    (nth *current-position* *current-playlist*)))

(defun pause-meta (mixer fun)
  "All of the pause/unpause/paused-p functions use pretty much exactly the same code, with just a different function call. So this high-order function is here to take advantage of that. Such a shame this docstring takes bigger space than this function saves."
  (unless (null (mixer-stream-list mixer))
    (funcall fun (first (mixer-stream-list mixer)) mixer)))

(defun pause-music (mixer)
  (pause-meta mixer #'streamer-pause))
(defun music-paused-p (mixer)
  (pause-meta mixer #'streamer-paused-p))
(defun unpause-music (mixer)
  (pause-meta mixer #'streamer-unpause))
