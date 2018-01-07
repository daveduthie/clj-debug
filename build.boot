(merge-env!
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure "1.9.0"]])

(require '[clj-debug.core :as core])

(deftask dev
  "Start a development server"
  []
  (println "DEV'ing...")
  (comp
   (cider)
   (repl :server true)
   (wait)))

(deftask rep
  "Run a client REPL"
  []
  (repl :client true))

(deftask run
  "Run the application"
  []
  (core/-main))
