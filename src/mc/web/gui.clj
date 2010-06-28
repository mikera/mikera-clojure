(ns mc.web.gui)

( set! *warn-on-reflection* true)

(use 'compojure)

(defn button [params]
  ()
  
  )






(defroutes my-app
  (GET "/"
    (html 
			[:h1 "Hello World!!"]
      [:body "This is some text"]))
	(GET "/*"
    (or (serve-file (params :*)) :next))
  (ANY "*"
    [404 "Page not found"])
)

(run-server {:port 80}
  "/*" (servlet my-app))
