(ns mc.image
  (:import [mikera.image Generator ImageUtils Colours])
  (:import [mikera.util Rand Maths])
  (:import [java.util Arrays])
  (:import [java.awt Image Graphics Dimension])
  (:import [java.awt.image BufferedImage])
  (:import [javax.swing JPanel]))

(defn rgb [r g b]
  (Colours/getRGBClamped (double r) (double g) (double b)))

(defn argb [a r g b]
  (Colours/getARGBClamped (double a)  (double r) (double g) (double b)))

(defn show 
  ([^Image img]
	  "Show an image in a new frame"
	  (ImageUtils/display img))
  ([^String title ^Image img]
	  "Show an image in a new or existing frame with a given title"
	  (ImageUtils/display title img)))

(defn image-panel [^Image img]
  (let [panel (proxy [JPanel] []
						    (paint [^Graphics g]
						      (.drawImage g img 0 0)))]
    (.setPreferredSize panel (Dimension. (.getWidth img) (.getHeight img)))
    panel))

(defn buffered-image [width height]
  "Create a default buffered image of the specified size in pxels"
  (Generator/newImage (int width) (int height)))

(defn create-image [f width height] 
  (let [bi ^BufferedImage (buffered-image width height)
        xfactor (double (/ 1.0 width))
        yfactor (double (/ 1.0 height))]
	  (dotimes [y height]
	    (dotimes[x width]
	      (.setRGB bi x y (f (* x xfactor) (* y yfactor)))))
    bi))

(defmacro image-function-2D [form]
  `(fn name#
     ([^double ~'x ^double ~'y]
       ~form)
     ([^double ~'x ^double ~'y ^double ~'z]
       (name# ~'x ~'y 0))))


(defn sampler 
  "Return a function that samples from a given image"
  ([^BufferedImage image]
    (let [width (.getWidth image)
          height (.getHeight image)]
    (image-function-2D 
      (.getRGB (int (* x width)) (int (* y height)))))))




