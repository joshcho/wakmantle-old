(ns app.printer
  ;; #?(:cljs (:require-macros [app.printer :refer [peek-n]]))
  #?(:cljs
     (:refer-clojure :exclude [count time trace assert]))
  )

#?(:cljs
   (do
     (def log (.-log js/console))
     (def warn (.-warn js/console))
     (defn info [& rest]
       (when true
         (js/console.log
          (str "%c " (apply str rest) " ")
          "color: blue; background-color: #F0F0F0;")))
     (def group (.-group js/console))
     (def groupCollapsed (.-groupCollapsed js/console))
     (def groupEnd (.-groupEnd js/console))
     (def assert (.-assert js/console))
     (def table (.-table js/console))
     (def dir (.-dir js/console))
     (def count (.-count js/console))
     (def time (.-time js/console))
     (def trace (.-trace js/console))
     (defn colog
       ([string]
        (colog string "black"))
       ([string bg-color]
        (colog string bg-color "white"))
       ([string bg-color color]
        (js/console.log (str "%c " string " ")
                        (str "color: " color
                             "; font-weight: bold; background-color: "
                             bg-color ";"))))))

#?(:cljs
   (do
     (def peek-atom (atom {}))
     (comment
       (peek-reset))))
#?(:clj
   (do
     (defmacro peek-reset
       ([]
        `(reset! peek-atom {}))
       ([& things]
        `(swap! peek-atom assoc ~(str things) 0)))
     (defmacro peek-unary [thing]
       `(log (clojure.string/replace
              (str "%c" '~thing)
              "clojure.core/deref"
              "@")
             "background: #D3D3D3" ~thing))
     (defmacro peek [& things]
       (if (> (count things) 1)
         `(do
            (js/console.group "")
            ~@(map (fn [thing] `(peek-unary ~thing)) things)
            (js/console.groupEnd))
         `(peek-unary ~(first things))))
     (defmacro peek-n [n & things]
       `(when (pos?
               (do
                 (when-not (get @peek-atom ~(str things))
                   (swap! peek-atom assoc ~(str things) 0))
                 (- ~n (get @peek-atom ~(str things)))))
          (swap! peek-atom update ~(str things) inc)
          (peek ~@things)))
     (defmacro group-n [n name & body]
       `(when (pos?
               (do
                 (when-not (get @peek-atom ~name)
                   (swap! peek-atom assoc ~name 0))
                 (- ~n (get @peek-atom ~name))))
          (swap! peek-atom update ~name inc)
          (group (str ~name " " (get @peek-atom ~name)))
          ~@body
          (groupEnd)))
     (defmacro group-collapsed-n [n name & body]
       `(when (pos?
               (do
                 (when-not (get @peek-atom ~name)
                   (swap! peek-atom assoc ~name 0))
                 (- ~n (get @peek-atom ~name))))
          (swap! peek-atom update ~name inc)
          (groupCollapsed (str ~name " " (get @peek-atom ~name)))
          ~@body
          (groupEnd)))
     (defmacro peek-once [thing]
       `(peek-n 1 ~thing))))
