(ns mc.csv
  (:import au.com.bytecode.opencsv.CSVReader)
  (:require mc.resource))

 
(defn ^au.com.bytecode.opencsv.CSVReader get-csv-reader [file]
  (new au.com.bytecode.opencsv.CSVReader 
    (new java.io.InputStreamReader 
      (.getResourceAsStream (mc.resource/context-class-loader) file))))