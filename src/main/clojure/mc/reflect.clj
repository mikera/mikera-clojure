(ns mc.reflect)

(defn construct [klass & args]
  "Construct a new class instance dynamically at runtime"
  (.newInstance
    (.getConstructor klass (into-array java.lang.Class (map type args)))
    (object-array args)))