(ns pb.core-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [pb.core :as p]))

(deftest smoke
  (is (= 1  1)))

(deftest draw
  (is (= (count (:standard (p/draw)))
         5)))
