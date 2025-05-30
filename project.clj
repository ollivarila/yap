(defproject yap "0.1.0-SNAPSHOT"
  :description "YAP"
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main ^:skip-aot yap.core
  :source-paths ["src" "examples"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
