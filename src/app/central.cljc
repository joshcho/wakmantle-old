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
     (defmacro lit-map
       "(lit-map [a b c]) => {:a a :b b :c c}"
       [syms]
       `(hash-map ~@(mapcat (fn [k] [(keyword k) k]) syms)))

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

       (defmacro defstate
         [state-sym ctr-m]
         (let [conditional-def-sym (gensym)
               dash-prefix         "--"]
           (letfn [(un-ctr [ctr-m]
                     (dissoc ctr-m :ctr/host))
                   (bang [sym]
                     (symbol (str "!" sym)))
                   (dash [sym]
                     (symbol (str dash-prefix sym)))
                   (underbar? [sym]
                     (= (first (str (symbol sym))) \_))
                   (bang-or-dash-bang? [sym]
                     (or
                      (= (first (str (symbol sym))) \!)
                      (when (< (count (str (symbol sym)))
                               (count dash-prefix))
                        (= (nth (str (symbol sym)) (count dash-prefix)) \!))))
                   (remove-underbar [sym]
                     (symbol (subs (str (symbol sym)) 1)))
                   (maybe-remove-underbar [k]
                     (if (underbar? k)
                       (remove-underbar k)
                       k))
                   (gen-binding-defs! [m current-host !binding-defs]
                     (->> m
                       (filter (comp keyword? first))
                       (map (fn [[k v]] [(symbol k) v]))
                       (mapv (fn [[k v]]
                               (if (underbar? k)
                                 (swap! !binding-defs conj `(e/def ~(dash (remove-underbar k)) ~v))
                                 (do
                                   (when-not (map? v)
                                     (swap! !binding-defs conj
                                            `(let [~conditional-def-sym (host=
                                                                         ~current-host)]
                                               (conditional-def ~conditional-def-sym
                                                                ~(dash (bang k)) (atom ~v)))))
                                   (swap! !binding-defs conj `(e/def ~(dash k)
                                                                (e/client (e/watch ~(dash (bang k))))))))))))
                   (gen-bindings! [m !bindings]
                     (->> m
                       (filter (comp keyword? first))
                       (map (fn [[k v]] [(symbol k) v]))
                       (mapv (fn [[k v]]
                               (if (underbar? k)
                                 (swap! !bindings conj [(remove-underbar k) (dash (remove-underbar k))])
                                 (do
                                   (when-not (map? v)
                                     (swap! !bindings conj [(bang k) (dash (bang k))]))
                                   (swap! !bindings conj [k (dash k)])))))))
                   (get-keysyms [m]
                     (vec
                      (mapcat
                       (fn [[k v]]
                         (let [sym (symbol k)]
                           (if (underbar? sym)
                             (if (keyword? k)
                               [(dash (remove-underbar sym))]
                               [(remove-underbar sym)])
                             (if (keyword? k)
                               (if (map? v)
                                 [(dash sym)]
                                 [(dash sym) (dash (bang sym))])
                               (if (map? v)
                                 [sym]
                                 [sym (bang sym)])))))
                       m)))
                   (gen-defs! [m map-name current-host !defs !bindings]
                     (let [full-def-requiring-pairs
                           (->> m
                             (filter (fn [[k v]]
                                       (not (or (map? v)
                                                (underbar? (symbol k))
                                                (keyword? k))))))]
                       (swap! !defs concatv
                              (mapcat (fn [[k v]]
                                        `((let [~conditional-def-sym (host=
                                                                      ~current-host)]
                                            (conditional-def ~conditional-def-sym
                                                             ~(bang k) (atom ~v))
                                            (e/def ~k (~(if (= current-host :client)
                                                          'e/client
                                                          'e/server)
                                                       (e/watch ~(bang k)))))))
                                      full-def-requiring-pairs)))
                     (let [partial-def-requiring-pairs
                           (->> m
                             (filter (fn [[k v]]
                                       (and (underbar? (symbol k))
                                            (not (or (map? v)
                                                     (keyword? k)))))))]
                       (swap! !defs concatv
                              (map (fn [[k v]]
                                     `(e/def ~(remove-underbar k)
                                        (symbol-macrolet ~(apply concatv @!bindings)
                                          ~v)))
                                   partial-def-requiring-pairs)))
                     (swap! !defs conj `(e/def ~map-name
                                          ~(->> (map (fn [keysym]
                                                       [(if (and
                                                             (<
                                                              (count dash-prefix)
                                                              (count (str keysym)))
                                                             (= (subs (str keysym) 0
                                                                      (count dash-prefix))
                                                                dash-prefix))
                                                          (keyword (subs (str keysym)
                                                                         (count dash-prefix)))
                                                          (keyword keysym))
                                                        (if (bang-or-dash-bang? keysym)
                                                          `(~(if (= current-host :client)
                                                               'e/client
                                                               'e/server)
                                                            ~keysym)
                                                          keysym)])
                                                     (get-keysyms m))
                                             (into {})))))
                   (get-expr [map-name ctr-m parent-host parent-bindings]
                     (let [m                 (un-ctr ctr-m)
                           !binding-defs     (atom [])
                           !defs             (atom [])
                           !bindings         (atom parent-bindings)
                           name-submap-pairs (->> m
                                               (filter (comp map? second))
                                               (map (fn [[k v]]
                                                      [(if (underbar? k)
                                                         (remove-underbar k)
                                                         (symbol k))
                                                       v])))
                           current-host      (or (:ctr/host ctr-m) parent-host)]
                       (gen-binding-defs! m current-host !binding-defs)
                       (gen-bindings! m !bindings)
                       (gen-defs! m map-name current-host !defs !bindings)
                       `(do
                          ;; ~(deref !bindings)
                          ~@(deref !binding-defs)
                          ~@(map (fn [[map-name m]]
                                   (get-expr map-name m current-host @!bindings))
                                 name-submap-pairs)
                          ~@(deref !defs)
                          )))]
             ;; default to :client for host
             (get-expr state-sym ctr-m :client []))))

       (defmacro defstate [_ _2]
         `(symbol-macrolet [anst "abcd"]
            (do
              (e/def game-state {:ans anst})
              (e/def ctr-state {:game-state game-state}))))

       (defmacro defstate
         ;; has bugs where top-level keywords don't work
         "define global tiered state.
            (defstate ctr-state
              {global-state {:ctr/host        :server
                             n-todays-answers 7}
               :play-state  {:ctr/host    :client
                             _recent-guess nil}})
          now global-state, n-todays-answers, and recent-guess are all visible as globals. play-state is not visible, as it's a keyword. global-state and play-state are maps. n-todays-answers is (e/watch !n-todays-answers) where !n-todays-answers is (atom 7). recent-guess is not a reactive atom since it has _ in front of it."
         [state-sym ctr-m]
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
                     (gen-connected-map [ctr-m parent-host]
                       (let [m                 (un-ctr ctr-m)
                             !bindings         (atom [])
                             !conditional-defs (atom [])
                             current-host      (or (:ctr/host ctr-m)
                                                   parent-host)
                             connected-map
                             (into {}
                               (mapcat (fn [[k v]]
                                         ;; this should be refactored, easy to make a mistake
                                         (if (keyword? k)
                                           (if (exposing-non-atom? (symbol k))
                                             (let [k (remove-exposing (symbol k))]
                                               (swap! !bindings concatv
                                                      [k (if (map? v)
                                                           (gen-connected-map v current-host)
                                                           v)])
                                               [[(keyword k) k]])
                                             (let [k (symbol k)]
                                               (swap! !bindings concatv
                                                      [k (if (map? v)
                                                           (gen-connected-map v current-host)
                                                           v)])
                                               (if (map? v)
                                                 [[(keyword k) k]]
                                                 [[(keyword k) k]
                                                  [(keyword (bang k)) (bang k)]])))
                                           (if (exposing-non-atom? k)
                                             (let [k (remove-exposing k)]
                                               [[(keyword k) k]])
                                             (if (map? v)
                                               [[(keyword k) k]]
                                               [[(keyword k) k]
                                                [(keyword (bang k)) (bang k)]]))))
                                       m))]
                         (gen-defs! ctr-m !conditional-defs !defs parent-host)
                         (if-not (empty? @!bindings)
                           `(let ~(deref !bindings)
                              ~@(deref !conditional-defs)
                              ~connected-map)
                           `(do
                              ~@(deref !conditional-defs)
                              ~connected-map))))
                     (gen-defs! [ctr-m !conditional-defs !defs parent-host]
                       (let [m            (un-ctr ctr-m)
                             current-host (or (:ctr/host ctr-m)
                                              parent-host)]
                         (doseq [[k v] m]
                           (when (symbol? k)
                             (cond
                               (map? v)
                               (swap! !defs conj
                                      `(e/def ~k ~(gen-connected-map v current-host)))
                               (and (symbol? k)
                                    (not (exposing-non-atom? k)))
                               (swap! !conditional-defs conj
                                      `(let [~def-atom-sym (host= ~current-host)]
                                         (conditional-def ~def-atom-sym
                                                          ~(bang k) (atom ~v))))))
                           (when (and (symbol? k)
                                      (not (map? v)))
                             (swap! !defs conj
                                    (if (exposing-non-atom? k)
                                      `(e/def ~(remove-exposing k) ~v)
                                      `(e/def ~k
                                         (~(if (= current-host :server)
                                             'e/server
                                             'e/client)
                                          (e/watch ~(bang k))))))))))]
               ;; defaults to client if :ctr/host not provided
               (let [host-keyword (or (:ctr/host ctr-m) :client)]
                 ;; (gen-defs! m !conditional-defs !defs host-keyword)
                 `(do
                    (e/def ~state-sym ~(gen-connected-map ctr-m host-keyword))
                    ~@(deref !defs)))))))

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
