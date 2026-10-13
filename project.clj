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
                 [com.github.rafd/dat "0.0.1-20261013-0"]
                 [datascript "1.8.1"]
                 [com.taoensso/nippy "3.7.0"]
                 ;; telemetry
                 [com.taoensso/telemere "1.2.1"]
                 ;; telemere needs encore >= 3.159
                 ;; (else bloom commons' timbre 4 and nippy 3.3 pull in an old encore);
                 ;; timbre 4 does not load with that encore
                 [com.taoensso/encore "3.161.0"]
                 [com.taoensso/timbre "6.5.0"]]
  :main georgetown.core
  :plugins [[io.bloomventures/omni "0.34.0"]]
  :omni-config georgetown.server.omni-config/omni-config
  :profiles {:dev
             {:source-paths ["dev-src"]}
             :datalevin
             {:dependencies [[org.datalevin/datalevin-embedded "1.0.2"]
                             [io.airlift/aircompressor "0.26"]]
              :jvm-opts ["--add-opens=java.base/java.nio=ALL-UNNAMED"
                         "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"]}
             :profiling
             {:jvm-opts ["-Djdk.attach.allowAttachSelf"]
              :dependencies [[com.clojure-goes-fast/clj-async-profiler "1.8.0"]]}
             :uberjar
             {:aot [georgetown.core]
              :prep-tasks ["javac"
                           ["omni" "compile"]
                           "compile"]}})
