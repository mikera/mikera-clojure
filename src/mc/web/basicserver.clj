(ns mc.web.basicserver)

(set! *warn-on-reflection* true)


(use 'compojure)

(defroutes my-app
  (GET "/*"
    (html 
			[:h1 "Hello World!!"]
      [:body "This is some text"]))
  (ANY "*"
    [404 "Page not found"])
)

(run-server {:port 80}
  "/*" (servlet my-app))



