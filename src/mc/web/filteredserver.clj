(ns mc.web.filteredserver)

(set! *warn-on-reflection* true)


(def joid-filter-config
  (proxy [javax.servlet.FilterConfig] []
    (getInitParameter 
      [name] 
      ({:saveInCookie true} name))))

(def joid-filter 
  (let [jf (org.verisign.joid.consumer.OpenIdFilter.)]
    (do
      (.init jf joid-filter-config)
      jf)))

(def pass-through-filter 
  (proxy [javax.servlet.Filter] []
    (doFilter
      [request response #^javax.servlet.FilterChain filterchain]
      (do
        (.doFilter filterchain request response)))))

 
(defn filter-chain 
  [#^javax.servlet.Servlet servlet]
	(proxy [javax.servlet.FilterChain] []
    (doFilter
      [request response]
      (.service servlet request response))))

(defn filtered-servlet 
  [#^javax.servlet.Filter servlet-filter handler]
  (let [#^javax.servlet.Servlet base-servlet (servlet handler)
        the-filter-chain (filter-chain base-servlet)]
    (proxy [javax.servlet.http.HttpServlet] []
	    (service 
	      [request response] 
	      (.doFilter servlet-filter request response the-filter-chain))
      (init 
			  [config] 
			  (.init base-servlet config)))))

(defroutes my-app
  (GET "/*"
    (html 
			[:h1 "Hello Foo!!"]))
  (ANY "*"
    [404 "Page not found"])
)


(run-server {:port 80}
  "/*" (filtered-servlet pass-through-filter my-app))



