(ns mc.resource)


(defn load-image [resource-name]
  (javax.imageio.ImageIO/read (.getResource javax.imageio.ImageIO resource-name)))

(defn load-font [resource-name]
  (java.awt.Font/createFont java.awt.Font/TRUETYPE_FONT (.getResourceAsStream javax.imageio.ImageIO resource-name)))