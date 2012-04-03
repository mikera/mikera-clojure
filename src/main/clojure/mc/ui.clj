(ns mc.ui
  (:import 
    (javax.swing JComponent JFrame JButton SwingUtilities)
    (java.awt Color Component Insets Graphics Dimension Rectangle)
    (java.awt.image BufferedImage)
    (javax.swing.border AbstractBorder)))

; (set! *warn-on-reflection* true)

(defn set-insets [^Insets insets border-size]
	(do 
    (set! (. insets top) border-size)
    (set! (. insets bottom) border-size)
    (set! (. insets left) border-size)
    (set! (. insets right) border-size)))

(defn make-border [^BufferedImage image border-size]
  (proxy [AbstractBorder] []
    (getBorderInsets [^Component c ^Insets insets]
      (set-insets insets border-size))
	  (paintBorder [^Component c ^Graphics g x y width height]
	    (let [bounds (.getBounds c)
	          w (.width bounds)
	          h (.height bounds)
	          bs border-size
	          tw (.getWidth image)
	          th (.getHeight image)] 
	      (.drawImage g image 0 0 bs (- h bs) 0 0 bs (- h bs) nil)
        (.drawImage g image bs 0 (- w bs) bs bs 0 (- w bs) bs nil)
	      (.drawImage g image 0 (- h bs) (- w bs) h 0 (- th bs) (- w bs) th nil)
	      (.drawImage g image (- w bs) 0 w (- h bs) (- tw bs) 0 tw (- h bs) nil)
	      (.drawImage g image (- w bs) (- h bs) w h (- tw bs) (- th bs) tw th nil)))))

; (def b (mc.ui/make-border (mc.resource/load-image "/images/bevel.png") 4))
; (. c setBorder b)

(def frames (atom {}))

(defn new-frame [title]
  (doto (JFrame. title)
    (.setSize (Dimension.  480 360))
    (.setVisible true)))

(defn show-component 
  ([^Component c]
    (show-component c nil))
  ([^Component c title]
    (SwingUtilities/invokeLater
      (fn []
		    (if title
			    (let [^JFrame frame (or (@frames title) (new-frame title))]
				      (.removeAll (.getContentPane frame))
		          (.add frame c)
				      (.validate frame)
		          (swap! frames assoc title frame)
			        frame)
		      (let [^JFrame frame (new-frame "Test Window")]
			      (doto frame
			        (.add c)
			        (.setSize (Dimension.  480 360)))
		        (swap! frames assoc title frame) ))))))

; (mc.ui/show-component c)

(defn make-coloured-box [^Color col]
  (proxy [JComponent] []
    (paintComponent [^Graphics g]
      (let [bounds (.getBounds ^JComponent this)
            w (.width bounds)
            h (.height bounds)] 
        (.setColor g col)
        (.fillRect g 0 0 w h)))))

; (def c (mc.ui/make-coloured-box (java.awt.Color. 10 100 150)))


(defn drawTiledImage [^Graphics g ^BufferedImage image]
  (let [^Rectangle bounds (.getClipBounds g)
        iw (.getWidth image)
        ih (.getHeight image)
        xmax (inc (int (/ (.width bounds) iw)))
        ymax (inc (int (/ (.height bounds) ih)))]
		(dotimes [y ymax]
		   (dotimes [x xmax]
          (.drawImage g image (+ (.x bounds) (* x iw)) (+ (.y bounds) (* y ih)) nil)))))
