(ns mc.resource
  (:import [java.awt.image BufferedImage])
  (:import [java.awt Font]))

(defn ^ClassLoader context-class-loader []
  (.getContextClassLoader (Thread/currentThread)))


;(defn ^java.awt.image.BufferedImage load-image [resource-name]
;  (javax.imageio.ImageIO/read (.getResource javax.imageio.ImageIO resource-name)))


(defn ^java.awt.image.BufferedImage load-image [resource-name]
  (javax.imageio.ImageIO/read (.getResource (context-class-loader) resource-name)))

(defn ^java.awt.Font load-font [resource-name]
  (java.awt.Font/createFont java.awt.Font/TRUETYPE_FONT (.getResourceAsStream (context-class-loader) resource-name)))