(ns test
  (:require  [cljs.test :refer-macros [run-all-tests]]
             [pb.core-test :as test]))

(defn run []
  (run-all-tests #"pb.*-test"))

(run)

