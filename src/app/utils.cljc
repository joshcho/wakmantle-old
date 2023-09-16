;; ^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns app.utils
  #?(:cljs (:require-macros [app.utils :refer [textarea* relay-atom range* input*
                                               ;; e-relay-atom
                                               ;; selected-task-relay-atom
                                               ]]))
  (:require
   contrib.str
   #?(:cljs
      [goog.string :as gstring])
   #?(:clj
      [clojure.tools.macro :refer [macrolet symbol-macrolet]])
   #?(:cljs
      [goog.string.format :as format])
   [hyperfiddle.rcf :refer [tests]]
   [clojure.string]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   ;; [clojure.core.match :refer [match]]
   [hyperfiddle.electric-ui4 :as ui]
   [malli.core :as m]
   [clojure.core.match :refer [match]]
   #?(:clj
      [clojure.test :refer [function?]])
   ;; #?(:clj [app.tx :as tx])
   #?(:clj
      [reduce-fsm :as fsm])
   [malli.dev.pretty :as pretty]
   [malli.util :as mu]
   ))


(declare in?)
(declare concatv)
(declare map-keys)
(declare map-vals)
(declare mb->)
(declare mb->>)
(declare mb->>>)
(malli.registry/set-default-registry!
 (-> m/default-registry (malli.registry/schemas)
     (assoc :coll (m/-collection-schema
                   {:type  :java-collection,
                    :pred  coll?
                    :empty []}))))

#?(:clj
   (do
     (defn error
       ([msg data]
        (throw (ex-info msg data)))
       ([msg]
        (error msg {})))

     (defmacro me [expr]
       `(clojure.walk/prewalk
         (fn [x#] (if (qualified-symbol? x#)
                    (symbol (name x#))
                    x#))
         (macroexpand-1 '~expr)))
     (defmacro me- [expr]
       `(macroexpand-1 '~expr))

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
              ~@body))))

     (comment
       (defstate-server
         {history []
          })
       (defstate-client
         {flags {hard-mode (atom false)
                 won       (atom false)
                 gave-up   (atom false)}
          })
       #?(:clj
          (do



            )))

     (do

       (def type->schema
         (macrolet [(quote-map
                     [m]
                     (into {}
                       (map (fn [[k v]]
                              `['~k ~v])
                            m)))]
           (letfn [(relax-and-add-strict
                     [m]
                     (->> m
                       (map (fn [[sym schema]]
                              [[sym [:maybe schema]]
                               [(symbol (str sym "!")) schema]]))
                       (apply concat)
                       (into m)))]
             (-> {n    [:int]
                  v    [:coll :any]
                  v1   [:coll :any]
                  v2   [:coll :any]
                  coll [:coll :any]
                  xs   [:coll :any]
                  s    [:string]
                  s1   [:string]
                  s2   [:string]
                  i    [:int]
                  m    [:map]
                  bool [:boolean]
                  vars [:coll :any]}
               quote-map
               relax-and-add-strict))))

       (defn strict-type? [t]
         (and
          (type->schema t)
          (= \! (last (str t)))))

       (defn union-type? [t]
         (and
          (type->schema t)
          (not (= \! (last (str t))))))

       (defn valid-type? [t]
         (boolean (type->schema t))))

     (def ^:dynamic *reporter* (pretty/thrower (pretty/-printer {:colors nil})))

     (defmacro massert
       "Assert that `x` validates against schema `?schema`, or throws ExceptionInfo.
   The clojure.core/*assert* constant controls whether assertion are checked."
       ([?schema x f]
        (if *assert*
          `(let [x# ~x]
             (if (m/validate ~?schema x#)
               x#
               (error "Schema mismatch."
                      (merge
                       {:var '~x}
                       (when ~f
                         {:fn ~f})
                       (mu/explain-data ~?schema x#)))
               ;; (*reporter* ::m/explain (mu/explain-data ~?schema x#))
               ))
          x))
       ([?schema x]
        `(massert ~?schema ~x nil)))

     (defmacro >fn [vars & body]
       (assert (= (count vars) (count (set vars)))
               "Duplicate vars")
       (let [var->schema
             (fn [var]
               (match [var]
                 ;; t can be a function symbol, too
                 [([x t] :seq)]
                 (mb->>> (type->schema t)
                   nil? (constantly
                         (error "Not a valid type in >fn."
                                {:type  t
                                 :value x})))
                 ;; if t is not a type, then we just return nil
                 [(t :guard symbol?)]
                 (type->schema t)
                 :else (error
                        "Not a symbol nor pair"
                        {:data var})))
             schema-asserts
             (->> vars
               (map (fn [v] [v (var->schema v)]))
               (remove (comp nil? second))
               (mapv  (fn [[v schema]]
                        `(massert ~schema ~(mb->>> v list? first)))))
             soft-check-expr-from-var
             (fn [x]
               (when (and (not (list? x)) (union-type? x))
                 `(when (nil? ~x)
                    (#?(:clj prn
                        :cljs js/console.log)
                     (str "Passed in nil for " '~x " in >fn")))))
             soft-check-exprs
             (->> vars
               (map soft-check-expr-from-var)
               (remove nil?))]
         `(fn ~(mapv #(mb->>> % list? first) vars)
            ~@schema-asserts
            ~@soft-check-exprs
            ~@body)))

     (defmacro >defn [sym vars & body]
       (assert (= (count vars) (count (set vars)))
               "Duplicate vars")
       (let [var->schema
             (fn [var]
               (match [var]
                 ;; t can be a function symbol, too
                 [([x t] :seq)]
                 (mb->>> (type->schema t)
                   nil? (constantly
                         (error "Not a valid type in >fn."
                                {:type  t
                                 :value x})))
                 ;; if t is not a type, then we just return nil
                 [(t :guard symbol?)]
                 (type->schema t)
                 :else (error
                        "Not a symbol nor pair"
                        {:data var})))
             schema-asserts
             (->> vars
               (map (fn [v] [v (var->schema v)]))
               (remove (comp nil? second))
               (mapv  (fn [[v schema]]
                        `(massert ~schema ~(mb->>> v list? first)
                                  ~sym))))
             soft-check-expr-from-var
             (fn [x]
               (when (and (not (list? x)) (union-type? x))
                 `(when (nil? ~x)
                    (#?(:clj prn
                        :cljs js/console.log)
                     (str "Passed in nil for " '~x " in >fn")))))
             soft-check-exprs
             (->> vars
               (map soft-check-expr-from-var)
               (remove nil?))]
         `(defn ~sym ~(mapv #(mb->>> % list? first) vars)
            ~@schema-asserts
            ~@soft-check-exprs
            ~@body)))))

#?(:clj
   (do
     (defmacro mb->
       "(mb-> 3
      number? inc
      false inc
      true (* 2)) => 8"
       [val & pred-fn-pairs]
       (let [pairs (partition 2 pred-fn-pairs)]
         (reduce (fn [v [pred fs]]
                   `(if (if (boolean? ~pred)
                          ~pred (~pred ~v))
                      ~(if (list? fs)
                         (concat
                          (list (first fs) v)
                          (rest fs))
                         (list fs v))
                      ~v))
                 val
                 pairs)))
     (defmacro mb->>
       "See mb->."
       [val & pred-fn-pairs]
       (let [pairs (partition 2 pred-fn-pairs)]
         (reduce (fn [v [pred fs]]
                   `(if (if (boolean? ~pred)
                          ~pred (~pred ~v))
                      ~(if (list? fs)
                         (concat fs (list v))
                         (list fs v))
                      ~v))
                 val
                 pairs)))
     (defmacro mb->>>
       "Inspired by ->>> in kezban."
       [val & pred-fn-pairs]
       (let [pairs (partition 2 pred-fn-pairs)]
         (reduce (fn [v [pred f]]
                   `(if (if (boolean? ~pred)
                          ~pred (~pred ~v))
                      (~f ~v)
                      ~v))
                 val
                 pairs)))))

(defn map-vals [f m] (reduce-kv (fn [m k v] (assoc m k (f v))) {} m))

(defn map-keys [f m] (reduce-kv (fn [m k v] (assoc m (f k) v)) {} m))

(defn atom? [x]
  #?(:clj
     (instance? clojure.lang.Atom x)))


(defn is-number [s]
  (boolean (re-matches #"-?\d+" s)))

#?(:clj
   (do
     (defmacro dropdown [xs F & body]
       `(dom/ul
          (e/for [x# ~xs]
            (dom/li
              (ui/button
                (e/fn []
                  (new ~F x#))
                (dom/text x#)
                ~@body)))))
     (defmacro ->>>
       "Takes a set of functions and value at the end of the arguments.
   Returns a result that is the composition
   of those funtions.Applies the rightmost of fns to the args(last arg is the value/input!),
   the next fn (left-to-right) to the result, etc."
       [& form]
       `((comp ~@(reverse (rest form))) ~(first form)))
     (defmacro arrows
       ([!value cmds]
        `(arrows ~!value ~cmds false))
       ([!value cmds reverse?]
        `(let [!n# (atom (count ~cmds))]
           (dom/on "keydown"
                   (e/fn [e]
                     (when (u/in? '("ArrowUp" "ArrowDown")
                                  (.-key e))
                       (cond (= (.-key e)
                                ~(if reverse?
                                   "ArrowDown"
                                   "ArrowUp"))
                             (swap! !n#
                                    #(let [it#   (atom %)
                                           cont# (atom true)
                                           val#  (if (= @it# (count ~cmds))
                                                   ""
                                                   (nth ~cmds @it#))]
                                       (while (and (< 0 @it#) @cont#)
                                         (swap! it# dec)
                                         (when (not= (nth ~cmds @it#)
                                                     val#)
                                           (reset! cont# false)))
                                       @it#))
                             (= (.-key e)
                                ~(if reverse?
                                   "ArrowUp"
                                   "ArrowDown"))
                             (swap! !n#
                                    #(let [it#   (atom %)
                                           cont# (atom true)
                                           val#  (if (= @it# (count ~cmds))
                                                   ""
                                                   (nth ~cmds @it#))]
                                       (while (and (< @it# (count ~cmds)) @cont#)
                                         (swap! it# inc)
                                         (when (not= (if (= @it# (count ~cmds))
                                                       ""
                                                       (nth ~cmds @it#))
                                                     val#)
                                           (reset! cont# false)))
                                       @it#)))
                       (let [val# (if (= @!n# (count ~cmds))
                                    "" (nth ~cmds @!n#))]
                         (set! (.-value dom/node) val#)
                         (reset! ~!value val#)
                         (set! (.-selectionStart dom/node)
                               (count val#))
                         (set! (.-selectionEnd dom/node)
                               (count val#))
                         (.preventDefault e)))))
           ;; this can't be enter since enter clears value
           (dom/on "keydown"
                   (e/fn [e#]
                     (when (= "Enter" (.-key e#))
                       (reset! !n# (count ~cmds))))))))
     (defmacro enter [F]
       `(dom/on "keydown"
                (e/fn [e#]
                  (when (= "Enter" (.-key e#))
                    (.preventDefault e#)
                    (when-some [v# (contrib.str/empty->nil (-> e# .-target .-value))]
                      (new ~F v#)
                      (set! (.-value dom/node) ""))))))))

(defmacro cond-let
  [& clauses]
  (if (= (count clauses) 1)
    (first clauses)
    (do
      (when-let [[binding then & rest] (seq clauses)]
        (assert (vector? binding) "cond-let is ill-formed")
        `(if-let ~binding
           ~then
           (cond-let ~@rest))))))

(defn index-of [coll val]
  (first (keep-indexed #(when (= %2 val) %1) coll)))

(defn positions
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

#?(:clj
   (defmacro prog1
     "The `retform` is evaluated, followed by the `body`. The value of the
   form is returned, so the point of `body` should be to have side-effects.

       (defn pop! [k]
          (prog1 (top k)
            (alter! k clojure.core/pop)))

   The name is a homage to older Lisps.
  "
     [retform & body]
     `(let [retval# ~retform]
        ~@body
        retval#)))

#?(:clj
   (defmacro cmt [& rest]
     `(comment ~@rest)))

(defn remove-nth [n coll]
  (let [[front back] (split-at n coll)]
    (concat front (rest back))))

;; (fsm/defsm-inc parse-map
;;   ;; :key is constructed, then used until value is received in full.
;;   ;; then key is dissoc'd.
;;   [[:start \{ -> :before-key]
;;    [:before-key
;;     \" -> :within-key]
;;    [:within-key
;;     \\ -> :within-key-esc
;;     \" -> {:action (fn [m & _]
;;                      (assoc m (:key m) ""))} :before-colon
;;     _ -> {:action (fn [m c & _]
;;                     (update m :key str c))} :within-key]
;;    [:within-key-esc
;;     _ -> {:action (fn [m c & _]
;;                     (update m :key str c))} :within-key]
;;    [:before-colon
;;     \: -> :before-value]
;;    [:before-value
;;     \" -> :within-value]
;;    [:within-value
;;     \\ -> :within-value-esc
;;     \" -> {:action (fn [m & _]
;;                      (dissoc m :key))} :before-key
;;     _ -> {:action (fn [m c & _]
;;                     (update m (:key m) str c))} :within-value]
;;    [:within-value-esc
;;     _ -> {:action (fn [m c & _]
;;                     (update m (:key m) str c))} :within-value]])

;; (def fsm-state (atom (parse-map {})))

;; (for [c "{\n \"text-id\": \"4\",\n  \"line-to-insert-after\": \"0\""]
;;   (do
;;     (swap! fsm-state fsm/fsm-event c)
;;     (:value @fsm-state)))

{:nextjournal.clerk/visibility {:code :hide :result :hide}}

(defn replace-nth [lst n x]
  (map-indexed (fn [i v] (if (= i n) x v)) lst))

(defn in? [list elem]
  (some #(= % elem) list))

(defn concatv [& args]
  (vec
   (apply concat args)))

(defn subs* [s start & [end]]
  (let [s-len (count s)
        start (if (neg? start) (+ s-len start) start)
        end   (if (and end (neg? end)) (+ s-len end) end)]
    (subs s start end)))

#?(:clj
   (defn sane-me [form]
     (clojure.walk/prewalk (fn [x] (if (qualified-symbol? x)
                                    (symbol (name x))
                                    x))
                           (macroexpand-1 form))))

(defn ignore-first
  "Transducer that ignores the first value in a sequence."
  []
  (fn [rf]
    (let [first? (atom true)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (if @first?
           (do (reset! first? false) result)
           (rf result input)))))))

#?(:clj
   (do
     (defmacro relay-atom
       "Returns a relay atom that 1) propagates changes from user via
  `server-effect` and 2) receives changes from the `server-query`.
  Note that `server-effect` is a Clojure function, not Electric.
  Specify `skip-effect-list` to prevent effects when they change."
       [server-query server-effect]
       `(let [!relay#            (atom (e/server
                                        (e/snapshot ~server-query)))
              !send-count#       (atom 0)
              !stop-propagation# (atom false)]
          (e/for-event
           [v# (->> (e/fn [] (e/watch !relay#))
                    (m/eduction (remove e/failure?)
                                (ignore-first)))]
           ( ;; js/console.log
            identity
            (new
             (e/fn [!send-count# !stop-propagation#]
               (if @!stop-propagation#
                 (reset! !stop-propagation# false)
                 (do
                   ;; (js/console.log "sending" v#)
                   (swap! !send-count# inc)
                   (e/server (new ~server-effect v#)))))
             !send-count# !stop-propagation#)))
          (e/for-event
           [v# (->> (e/fn []
                      (e/server ~server-query))
                    (m/eduction (remove e/failure?)))]
           (new
            (e/fn [!send-count#]
              (if (<= @!send-count# 0)
                (do
                  ;; (js/console.log "received change" v#)
                  (reset! !stop-propagation# true)
                  (reset! !relay# v#))
                (do
                  ;; (js/console.log "received what was sent" v#)
                  (swap! !send-count# dec))))
            !send-count#))
          !relay#))
     ;; (defmacro e-relay-atom [e-id attr]
     ;;   `(relay-atom
     ;;     (~attr
     ;;      (d/entity ~'db ~e-id))
     ;;     (e/fn [v#]
     ;;       (tx/transact! ~'!conn
     ;;                     [{:db/id (e/snapshot ~e-id)
     ;;                       ~attr  v#}]))))
     ;; (defmacro selected-task-relay-atom [attr]
     ;;   `(e-relay-atom ~'selected-task-id ~attr))
     ))

#?(:cljs (defn value [^js e] (.-target.value e))
   ;; workaround inference warnings, todo rename
   )

#?(:clj
   (do
     (defmacro control* [event-type parse unparse v V! setter & body]
       `(let [[state# v#] (e/for-event-pending-switch [e# (e/listen> dom/node ~event-type)]
                                                      (some->> (~parse e#) (new ~V!)))]
          ;; (dom/style {:background-color (when (= ::e/pending state#) "yellow")})
                                        ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
          (when-some [v# (when (and (not (new dom/Focused?)) (#{::e/init ::e/ok} state#)) ~v)]
            (~setter dom/node (~unparse v#))) ; js coerce
          ~@body
          (case state# (::e/pending ::e/failed) (throw v#) (::e/init ::e/ok) v#)))
     (defmacro range* [v V! & body]
       `(dom/input (dom/props {:type "range"})
                   (control* "input" (comp parse-long value) identity ~v ~V! dom/set-val ~@body)))
     (defmacro textarea* [v V! & body]
       `(dom/textarea
         (control* "input" value identity ~v ~V! dom/set-val ~@body)))
     (defmacro input* [v V! & body]
       `(dom/input
         (control* "input" value identity ~v ~V! dom/set-val ~@body)))))
