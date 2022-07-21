;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.db.json-functions
  (:require [honeysql.core :as sql]
            [honeysql.types :as sql-types]
            [cheshire.core :as json]))

(defn build-object [& kvs]
  {:pre [(even? (count kvs))]}
  (apply sql/call
   :json_build_object
   (map-indexed
    #(if (even? %1)
       (name %2) ;; TODO it would be nice to inline this
       %2)
    kvs)))

(defn agg [v]
  (sql/call :json_agg v))

(defn ->json [v]
  (sql-types/call
   :cast
   (json/encode v)
   :json))
