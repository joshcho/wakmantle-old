(ns app.central
  #?(:cljs (:require-macros [app.central :refer [defstate]]))
  (:require
   contrib.str
   #?(:clj
      [clojure.tools.macro :refer [macrolet symbol-macrolet]])
   [clojure.string :as str]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   #?(:clj [app.utils :as u :refer [concatv me me- in? error]]
      :cljs [app.utils :as u :refer [concatv me me- in?]])))

#?(:clj

   (do
     (comment
       (let [m '{hard    true
                 won     false
                 gave-up false}]
         #?(:clj (initialize-atoms m))
         (defcenter '{hard    true
                      won     false
                      gave-up false}))
       (if *clojure-version*
         (def a 17)
         nil)

       (defmacro conditional-def
         "bool-sym is a symbol that is bound to a boolean value.

            (def a true)
            (conditional-def a !b 3)
            !b => 3
            (def a false)
            (conditional-def a !b 3)
            !b => Unable to resolve symbol"
         [bool-sym bang-sym initial]
         (let [current-ns (symbol (namespace ::x))]
           `(do
              (when ~bool-sym
                (def ~bang-sym ~initial))
              ;; we need ns-unmap since def always creates a symbol
              (when-not ~bool-sym
                (ns-unmap '~current-ns '~bang-sym)))))

       (defmacro host=
         "check if host is the specified keyword. nil defaults to :client
  In CLJ:  (host= :server) => true
  In CLJS: (host= :client) => true"
         [host-keyword]
         (assert (or (= host-keyword :server)
                     (= host-keyword :client)))
         `(boolean (or (and ~(= host-keyword :server) *clojure-version*)
                       (and ~(not= host-keyword :server) (not *clojure-version*)))))

       (defmacro defstate [state-sym ctr-m]
         (letfn [(un-ctr [ctr-m]
                   (dissoc ctr-m :ctr/host))]
           (let [!defs        (atom [])
                 m            (un-ctr ctr-m)
                 def-atom-sym (gensym)]
             (letfn [(bang [sym]
                       (symbol (str "!" sym)))
                     (exposing-non-atom? [k]
                       (= (first (str k)) \_))
                     (remove-exposing [k]
                       (symbol (subs (str k) 1)))
                     (gen-connected-map [ctr-m]
                       (let [m (un-ctr ctr-m)]
                         (into {}
                           (mapcat (fn [[k v]]
                                     (if (keyword? k)
                                       [[k v]]
                                       (if (exposing-non-atom? k)
                                         (let [k (remove-exposing k)]
                                           [[(keyword k) k]])
                                         (if (map? v)
                                           [[(keyword k) k]]
                                           [[(keyword k) k]
                                            [(keyword (bang k)) (bang k)]]))))
                                   m))))
                     (gen-defs! [ctr-m !defs parent-host]
                       (let [m            (un-ctr ctr-m)
                             current-host (or (:ctr/host ctr-m)
                                              parent-host)
                             new-defs
                             `((let [~def-atom-sym (host= ~current-host)]
                                 ~@(remove
                                    nil?
                                    (concatv
                                     (map (fn [[k v]]
                                            (cond
                                              (map? v)
                                              (do
                                                (gen-defs! v !defs (or (:ctr/host ctr-m) parent-host))
                                                `(e/def ~k ~(gen-connected-map v)))
                                              (and (symbol? k)
                                                   (not (exposing-non-atom? k)))
                                              `(conditional-def ~def-atom-sym
                                                                ~(bang k) (atom ~v))))
                                          m)
                                     (map (fn [[k v]]
                                            (when (and (symbol? k)
                                                       (not (map? v)))
                                              (if (exposing-non-atom? k)
                                                `(e/def ~(remove-exposing k) ~v)
                                                `(e/def ~k
                                                   (~(if (= current-host
                                                            :server)
                                                       'e/server
                                                       'e/client)
                                                    (e/watch ~(bang k))))
                                                #_(and (exposing-non-atom? k)
                                                       (not (map? v)))
                                                #_(if (exposing-non-atom? k)
                                                    `(~(remove-exposing k) ~v)
                                                    `(~(bang k) (atom ~v)))
                                                )))
                                          m)))))]
                         (swap! !defs concatv new-defs)))]
               ;; defaults to client if :ctr/host not provided
               (let [host-keyword (or (:ctr/host ctr-m) :client)]
                 (gen-defs! m !defs host-keyword))
               `(do
                  ~@(deref !defs)
                  (e/def ~state-sym ~(gen-connected-map ctr-m)))))))

       (me
        (defstate ctr-state
          {global-state nil
           user-state   2
           play-state   nil
           whish-state  nil}))

       #?(:cljs
          (def !global-state (atom nil))
          (def !user-state (atom 2)))
       (e/def global-state (e/watch !global-state))
       (e/def user-state (e/watch !user-state))
       (e/def ctr-state {:global-state global-state
                         :user-state   user-state})

       (if-cljs (+ 1 2)
         (+ 2 3))
       (defn def-in-host [host sym val]
         (if (= host :server)
           #?(:clj (eval `(def ~sym ~val)))
           #?(:cljs
              (eval `(def ~sym ~val)))))

       (defmacro testos []
         #?(:clj `(+ 1 2)
            :cljs `(+ 2 3)))


       )
     (comment

       (me (defcenter {:server true
                       won     true}))

       (defcenter center
         {:host :client
          flags {hard    true
                 won     false
                 gave-up false}})
       (defcenter center
         {hard    true
          won     false
          gave-up false})
       (defmacro def-symbol [sym val]
         `(def ~(eval sym) ~val))
       (def a 'bcd)
       (def ^:dynamic tempo 'tempo)
       (me
        (def-symbol a 3))
       (defn def-in-host [host sym val]
         (binding [tempo 'bcd]
           (def-symbol tempo 3)))
       (def-symbol a 5)
       (defn def-in-host [host sym val]

         (if (= host :client)
           #?(:cljs
              (binding [tempo 3]
                (def-symbol tempo 3)))
           #?(:clj
              (def-symbol tempo 3))))


       (do
         #?(:cljs
            (def !hard (atom true))
            (def !won (atom false))
            (def !gave-up (atom false)))
         (e/def hard (e/client (e/watch !hard)))
         (e/def won (e/client (e/watch !won)))
         (e/def gave-up (e/client (e/watch !gave-up)))

         (e/def flags {:hard     hard
                       :!hard    (e/client !hard)
                       :won      won
                       :!won     (e/client !won)
                       :gave-up  gave-up
                       :!gave-up (e/client !gave-up)})))

     (defmacro letm [state-map & body]
       (let [!bindings (atom [])]
         (letfn [(bang [sym]
                   (symbol (str "!" sym)))
                 (exposing-non-atom? [k]
                   (= (first (str k)) \_))
                 (remove-exposing [k]
                   (symbol (subs (str k) 1)))
                 (gen-connected-map [m]
                   (into {}
                     (mapcat (fn [[k v]]
                               (if (keyword? k)
                                 [k v]
                                 (if (exposing-non-atom? k)
                                   (let [k (remove-exposing k)]
                                     [[(keyword k) k]])
                                   (if (map? v)
                                     [[(keyword k) k]]
                                     [[(keyword k) k]
                                      [(keyword (bang k)) (bang k)]]))))
                             m)))
                 (gen-bindings! [m !bindings init?]
                   ;; call with init? as true
                   (let [new-bindings
                         (concatv
                          (mapcat (fn [[k v]]
                                    (cond
                                      (map? v)
                                      (do
                                        (gen-bindings! v !bindings false)
                                        [k (gen-connected-map v)])
                                      (and (symbol? k)
                                           (not (exposing-non-atom? k)))
                                      `(~(bang k) (atom ~v))
                                      (and (keyword? k) init?)
                                      (error "We don't allow keywords at base map for letm.")))
                                  m)
                          (mapcat (fn [[k v]]
                                    (when (and (symbol? k)
                                               (not (map? v)))
                                      (if (exposing-non-atom? k)
                                        `(~(remove-exposing k) ~v)
                                        [k `(e/watch ~(bang k))]
                                        #_(and (exposing-non-atom? k)
                                               (not (map? v)))
                                        #_(if (exposing-non-atom? k)
                                            `(~(remove-exposing k) ~v)
                                            `(~(bang k) (atom ~v)))
                                        )))
                                  m))]
                     (swap! !bindings concatv new-bindings)))]
           (gen-bindings! state-map !bindings true)
           (assert (= (count (take-nth 2 @!bindings))
                      (count (set (take-nth 2 @!bindings))))
                   "Some bindings are duplicate.")
           `(let ~(deref !bindings)
              ~@body))))))
