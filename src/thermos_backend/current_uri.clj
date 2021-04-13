;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.current-uri)

(def ^:dynamic *current-uri* nil)

(defn wrap-current-uri [h]
  (fn [r]
    (binding [*current-uri* (:uri r)]
      (h r))))
