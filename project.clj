(defproject georgetown "0.0.1"
  :source-paths ["src"]
  :java-source-paths ["java-src"]
  :dependencies [[org.clojure/clojure "1.11.0"]
                 ;; >= 1.12 required by or-tools cp-sat: older clojurescript pulls
                 ;; closure-compiler-unshaded, which bundles an unrelocated protobuf 3
                 ;; that shadows protobuf-java on the classpath
                 ;; same exclusions omni used for its own clojurescript dep
                 [org.clojure/clojurescript "1.12.42"
                  :exclusions [com.cognitect/transit-clj
                               com.fasterxml.jackson.core/jackson-core]]

                 ;; optimization
                 ;; leiningen does not resolve ortools-java's os-activated maven profiles,
                 ;; so the per-platform natives are listed explicitly
                 [com.google.ortools/ortools-java "9.15.6755"]
                 [com.google.ortools/ortools-darwin-aarch64 "9.15.6755"]
                 [com.google.ortools/ortools-linux-x86-64 "9.15.6755"]

                 [io.bloomventures/omni "0.34.0"]
                 [http-kit "2.8.0"]
                 [io.bloomventures/commons "0.14.11"]
                 [io.github.escherize/huff "0.2.12"]
                 [ring/ring-defaults "0.5.0"]
                 [com.hyperfiddle/rcf "20220926-202227"]
                 [tada "0.3.0"]
                 [jarohen/chime "0.3.3"]
                 [com.draines/postal "2.0.3"]
                 [applied-science/js-interop "0.4.2"]

                 ;; db
                 [com.github.rafd/dat "0.0.1-20260705-0"]
                 [datalevin "1.0.0"]
                 [com.taoensso/nippy "3.3.0"]
                 [io.airlift/aircompressor "0.26"]]
  :main georgetown.core
  :plugins [[io.bloomventures/omni "0.34.0"]]
  :omni-config georgetown.server.omni-config/omni-config
  :profiles {:dev
             {:source-paths ["dev-src"]}
             :uberjar
             {:aot [georgetown.core]
              :prep-tasks ["javac"
                           ["omni" "compile"]
                           "compile"]}})
