(ns test
  (:require  [cljs.test :refer-macros [run-all-tests]]))

(defn ^:export run []
  (run-all-tests #"pb.*-test"))
