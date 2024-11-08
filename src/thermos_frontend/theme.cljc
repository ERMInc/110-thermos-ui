;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.theme)

#?(:cljs
   (defn create-stripes [fg bg on off]
     (let [canvas (js/document.createElement "canvas")
           context (.getContext canvas "2d")

           side (int (* (Math/sqrt 2) (+ on off)))

           line (fn [x y x1 y1]
                  (.beginPath context)
                  (.moveTo context x y)
                  (.lineTo context x1 y1)
                  (.stroke context))
           ]

       (set! (.. canvas -width) side)
       (set! (.. canvas -height) side)

       (set! (.. context -fillStyle) bg)

       (.beginPath context)
       (.rect context 0 0 side side)
       (.fill context)

       (set! (.. context -lineWidth) on)
       (set! (.. context -strokeStyle) fg)
       (set! (.. context -lineCap) "square")

       (line (- side) (- side)
             (* 2 side) (* 2 side))


       (line (- side) 0
             side (* 2 side))

       (line 0 (- side)
             (* 2 side) side)

       canvas)))


(def blue "#00ACE6")
(def red "#ED3027")
(def green "#0a2b14") ;; dark green
(def teal "#20c997") ;; teal
(def white "#ffffff")
(def light-grey "#bbbbbb")
(def dark-grey "#555555")
(def cyan "#ffd700")
(def magenta "#ff00ff")
(def peripheral-yellow "#ffd700")
(def peripheral-yellow-light "#eedd82")
(def in-solution-orange "#c84500")
(def supply-orange "#dd6600")
(def green "#32cd32")
(def beige "#d7c8aa")

(def blue-light "#bbeeff")
(def red-light "#ffbbbb")
(def cyan-light "#ffd700")
(def magenta-light "#ffaaff")
(def in-solution-orange-light "#ffccaa")
(def green-light "#ddffdd")
(def beige-light "#ffddcc")

#?(:cljs
   (def white-light-grey-stripes
     (create-stripes "#fff" light-grey 4 8)))

#?(:cljs
   (def white-dark-grey-stripes
     (create-stripes "#fff" dark-grey 4 8)))

#?(:cljs
   (def blue-light-grey-stripes
     (create-stripes blue light-grey 4 8)))

#?(:cljs
   (def blue-dark-grey-stripes
     (create-stripes blue dark-grey 4 8)))

(def icon [:img {:width 35 :height 35 :src "/ERM_crop.png"}])
