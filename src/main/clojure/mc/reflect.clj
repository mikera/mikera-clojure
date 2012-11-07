(ns mc.reflect)

(defn construct [klass & args]
  "Construct a new class instance dynamically at runtime"
  (.newInstance
    (.getConstructor klass (into-array java.lang.Class (map type args)))
    (object-array args)))


 (defn build-constructor [klass & types]
    "Return a construtor function for the given class"
    (let [constructor (.getConstructor klass (into-array java.lang.Class types))]
      (fn [& args]
        (.newInstance constructor args))))
