(ns forge.import.svg
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [svg-clj.main :as svg]
            [svg-clj.path :as path]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]))
