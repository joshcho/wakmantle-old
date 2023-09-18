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

     (defmacro defstate
       "define global tiered state.
            (defstate ctr-state
              {global-state {:ctr/host        :server
                             n-todays-answers 7}
               :play-state  {:ctr/host    :client
                             _recent-guess nil}})
          now global-state, n-todays-answers, and recent-guess are all visible as globals. play-state is not visible, as it's a keyword. global-state and play-state are maps. n-todays-answers is (e/watch !n-todays-answers) where !n-todays-answers is (atom 7). recent-guess is not a reactive atom since it has _ in front of it."
       ([state-sym ctr-m]
        `(defstate ~state-sym ~ctr-m nil))
       ([state-sym ctr-m component-sym]
        (let [conditional-def-sym (gensym)
              dash-prefix         "--"
              !component-exprs    (atom [])]
          (letfn
              [(un-ctr [ctr-m]
                 (dissoc ctr-m
                         :ctr/host
                         :ctr/expr))
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
               (gen-binding-defs! [m current-host !binding-defs !bindings]
                 ;; generates dashed defs
                 (->> m
                   (filter (comp keyword? first))
                   (map (fn [[k v]] [(symbol k) v]))
                   (mapv (fn [[k v]]
                           (if (underbar? k)
                             (swap! !binding-defs conj `(e/def ~(dash (remove-underbar k))
                                                          (symbol-macrolet ~(apply concatv @!bindings)
                                                            ~v)))
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
                 ;; generates bindings, essentially replacing non-dashed with dashed in values
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
                 ;; generate definitions
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
               (gen-component-expr! [expr !component-exprs !bindings]
                 (swap! !component-exprs
                        conj `(symbol-macrolet ~(apply concatv @!bindings)
                                ~expr)))
               (get-expr [map-name ctr-m parent-host parent-bindings]
                 ;; the basic helper function for generating exprs
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
                   (gen-bindings! m !bindings)
                   (gen-binding-defs! m current-host !binding-defs !bindings)
                   (gen-defs! m map-name current-host !defs !bindings)
                   (when (and component-sym (:ctr/expr ctr-m))
                     (gen-component-expr! (:ctr/expr ctr-m) !component-exprs !bindings))
                   `(do
                      ;; ~(deref !bindings)
                      ~@(deref !binding-defs)
                      ~@(mapv (fn [[map-name m]]
                                (get-expr map-name m current-host @!bindings))
                              name-submap-pairs)
                      ~@(deref !defs)
                      )))]
            ;; default to :client for host
            `(do
               ~(get-expr state-sym ctr-m :client [])
               ~(when component-sym
                  `(e/defn ~component-sym []
                     (e/client
                      ~@(deref !component-exprs)))))))))

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
