(require :asdf)
(require :mcclim)
(require :mixalot)
(require :mixalot-mp3)

(defpackage :music-player
  (:use
   :clim :clim-lisp
   :mixalot :mixalot-mp3)
  (:export
   lo-play-track
   get-mp3s-under
   run-app))
