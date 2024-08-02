(defproject georgetown "0.0.1"
  :source-paths ["src" "dev-src"]
  :dependencies [[org.clojure/clojure "1.11.0"]
                 [io.bloomventures/omni "0.34.0"]
                 [http-kit "2.8.0"]
                 [io.bloomventures/commons "0.14.8"]
                 [io.github.escherize/huff "0.2.12"]
                 [ring/ring-defaults "0.5.0"]

                 ;; db
                 [datalevin "0.9.5"]
                 [com.taoensso/nippy "3.3.0"]
                 [io.airlift/aircompressor "0.26"]]
  :jvm-opts [;; datalevin
             "--add-opens=java.base/java.nio=ALL-UNNAMED"
             "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"]
  :main georgetown.core)
