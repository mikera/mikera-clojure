(ns mc.image
  (:import [mikera.image Generator ImageUtils Colours])
  (:import [mikera.util Rand Maths])
  (:import [java.util Arrays])
  (:import [java.awt Image Graphics])
  (:import [java.awt.image BufferedImage]))


(defn show 
  ([^Image img]
	  "Show an image in a new frame"
	  (ImageUtils/display img))
  ([^String title ^Image img]
	  "Show an image in a new or existing frame with a given title"
	  (ImageUtils/display title img)))



